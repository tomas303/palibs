unit trl_upersistxml;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  trl_irttibroker, trl_ipersist, trl_ifactory,
  trl_urttibroker,
  DOM, XMLRead, XMLWrite, xpath,
  SysUtils, fgl, classes, base64,
  trl_iminipersist, trl_udifactory;

const
  cSID = 'SID';
  cRefID = 'refid';
  cRefClass = 'refclass';
  cValue = 'Value';
  cRoot = 'root';
  cStoreSettings = 'storesettings';
  cLastSid = 'lastsid';
  cMemo = 'memo';

type

  { TXmlStore }
  TXmlStore = class(TInterfacedObject, IPersistStoreDevice)
  private type

    { TSelector }

    TSelector = class(TInterfacedObject, IRBDataEnumerable, IRBDataEnumerator)
    private
      fStore: TXmlStore;
      fClassEl: TDOMElement;
      fIndex: Integer;
      fCurrent: IRBData;
      function MoveNext: Boolean;
      function GetCurrent: IRBData;
      function GetEnumerator: IRBDataEnumerator;
    public
      constructor Create(AStore: TXmlStore; AClassEl: TDOMElement);
    end;

  private
    const
      cListItemTag = 'i';
      cListItemNilTag = 'n';
  private
    fFactory: IPersistFactory;
    fFile: string;
    fDoc: TXMLDocument;
    function GetDoc: TXMLDocument;
    function GetDataClassEl(const ADataClassName: string; ACanCreate: Boolean): TDOMElement;
    class function FindElement(const AContextEl: TDOMElement; const AXPath: string): TDOMElement;
    //save
    procedure SaveDataItemValue(AStoreEl: TDOMElement; const AName, AValue: string);
    procedure SaveDataItemMemo(AStoreEl: TDOMElement; const AName, AValue: string);
    procedure SaveDataItemStream(AStoreEl: TDOMElement; AValue: TStream);
    procedure SaveDataItemObject(AStoreEl: TDOMElement; const AValue: TObject; AIsReference: Boolean);
    procedure SaveDataItemRef(AStoreEl: TDOMElement; const AName: string{;
      const AValue: IPersistRef});
    procedure SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure SaveData(AStoreEl: TDOMElement; AData: IRBData);
    //load
    function LoadDataItemValue(AStoreEl: TDOMElement; const AName: string): string;
    function LoadDataItemMemo(AStoreEl: TDOMElement; const AName: string): string;
    procedure LoadDataItemStream(AStoreEl: TDOMElement; const AName: string; AValue: TStream);
    procedure LoadDataItemObject(AStoreEl: TDOMElement; const AData: IRBData);
    procedure LoadDataItemRef(AStoreEl: TDOMElement{; const AValue: IPersistRef});
    procedure LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure LoadData(AStoreEl: TDOMElement; AData: IRBData);
    //
    procedure Read(AStoreEl: TDOMElement; AData: IRBData);
    procedure ReadList(AStoreEl: TDOMElement; const AName: String{; const AList: IDataList});
    procedure ReadObject(AStoreEl: TDOMElement; const AName: String; AObject: TObject);
    procedure ReadStream(AStoreEl: TDOMElement; AValue: TStream);
    function ReadMemo(AStoreEl: TDOMElement; const AName: string): string;
    function ReadValue(AStoreEl: TDOMElement; const AName: string): string;
    procedure Write(AStoreEl: TDOMElement; AData: IRBData);
    procedure WriteList(AStoreEl: TDOMElement; const AName: String{; const AList: IDataList});
    procedure WriteObject(AStoreEl: TDOMElement; const AName: String; AObject: TObject);
    procedure WriteStream(AStoreEl: TDOMElement; AValue: TStream);
    procedure WriteMemo(AStoreEl: TDOMElement; const AName, AValue: string);
    procedure WriteValue(AStoreEl: TDOMElement; const AName, AValue: string);
  private
    procedure FillAutoFields(const AData: IRBData);
    function FindStoreEl(const AData: IRBData): TDOMElement;
  protected
    procedure CheckOpen;
    property Doc: TXMLDocument read GetDoc;
  public
    destructor Destroy; override;
    // IPersistStoreDevice
    procedure Save2(const AData: IRBData);
    procedure Delete2(const AData: IRBData);
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    function IsOpened: Boolean;
    procedure Flush;
    //
    function Select2(const AClass: string): IRBDataEnumerable;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property XMLFile: string read fFile write fFile;
  end;

implementation

type

  { EXMLStore }

  EXMLStore = class(Exception)
  public
    class procedure ClassNotExists(const AClass: string);
    class procedure ClassForSIDNotExists(const ASID: string);
    class procedure ClassForNameNotExists(const AName: string);
    class procedure ObjectNotExists(const AClass, AName: string); overload;
    class procedure ObjectNotExists(const AClass: string; AID: integer); overload;
    class procedure MoreObjectWithNameExists(const AClass, AName: string); overload;
    class procedure MoreObjectWithNameExists(const AClass: string;  AID: integer); overload;
    class procedure CannotAddItemWithoutSIDToList(const AClass: string);
    class procedure CannotFindReferencedSID(const AOwner: string; ASID: string);
  end;

{ TXmlStore.TSelector }

function TXmlStore.TSelector.MoveNext: Boolean;
begin
  fCurrent := nil;
  Inc(fIndex);
  Result := (fClassEl <> nil) and (fIndex <= fClassEl.ChildNodes.Count - 1);
  if not Result then
    Exit;
end;

function TXmlStore.TSelector.GetCurrent: IRBData;
var
  mEl: TDOMElement;
begin
  if fCurrent = nil then begin
    mEl := fClassEl.ChildNodes[fIndex] as TDOMElement;
    fCurrent := fStore.Factory.CreateObject(fClassEl.TagName);
    fStore.LoadData(mEl, fCurrent);
  end;
  Result := fCurrent
end;

function TXmlStore.TSelector.GetEnumerator: IRBDataEnumerator;
begin
  Result := Self;
end;

constructor TXmlStore.TSelector.Create(AStore: TXmlStore; AClassEl: TDOMElement);
begin
  inherited Create;
  fStore := AStore;
  fClassEl := AClassEl;
  fIndex := -1;
end;

{ EXMLStore }

class procedure EXMLStore.ClassNotExists(const AClass: string);
begin
  raise EXMLStore.CreateFmt('Class %s not found in store', [AClass]);
end;

class procedure EXMLStore.ClassForSIDNotExists(const ASID: string);
begin
  raise EXMLStore.CreateFmt('Cannot find class for SID %s', [ASID]);
end;

class procedure EXMLStore.ClassForNameNotExists(const AName: string);
begin
  raise EXMLStore.CreateFmt('Cannot find class for Name %s', [AName]);
end;

class procedure EXMLStore.ObjectNotExists(const AClass, AName: string);
begin
  raise EXMLStore.CreateFmt('Object %s.%s not found in store', [AClass, AName]);
end;

class procedure EXMLStore.ObjectNotExists(const AClass: string; AID: integer);
begin
  raise EXMLStore.CreateFmt('Object %s.%d not found in store', [AClass, AID]);
end;

class procedure EXMLStore.MoreObjectWithNameExists(const AClass, AName: string);
begin
  raise EXMLStore.CreateFmt('Object %s.%s exists more then once in store', [AClass, AName]);
end;

class procedure EXMLStore.MoreObjectWithNameExists(const AClass: string;
  AID: integer);
begin
  raise EXMLStore.CreateFmt('Object %s.%d exists more then once in store', [AClass, AID]);
end;

class procedure EXMLStore.CannotAddItemWithoutSIDToList(const AClass: string);
begin
  raise EXMLStore.CreateFmt('Object of class %s do not have SID stored - cannot add to list', [AClass]);
end;

class procedure EXMLStore.CannotFindReferencedSID(const AOwner: string;
  ASID: string);
begin
  raise EXMLStore.CreateFmt('Object %s refer to not exists SID %s', [AOwner, ASID]);
end;

{ TXmlStore }

function TXmlStore.GetDoc: TXMLDocument;
begin
  if fDoc= nil then
    raise exception.Create('not opend');
  Result := fDoc;
end;

procedure TXmlStore.SaveData(AStoreEl: TDOMElement; AData: IRBData);
var
  i: integer;
  mObjStoreEl: TDOMElement;
begin
  //for i := 0 to AData.Count - 1 do
  //begin
  //  if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistMany) then
  //    SaveDataList(AStoreEl, AData[i])
  //  else if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistRef) then
  //    SaveDataItemRef(AStoreEl, AData[i].Name, AData[i].AsInterface as IPersistRef)
  //  else if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IRBData) then
  //  begin
  //    mObjStoreEl := Doc.CreateElement(AData[i].Name);
  //    AStoreEl.AppendChild(mObjStoreEl);
  //    SaveData(mObjStoreEl, AData[i].AsInterface as IRBData)
  //  end
  //  else if AData[i].IsObject then
  //  begin
  //    if AData[i].AsObject <> nil then
  //    begin
  //      mObjStoreEl := Doc.CreateElement(AData[i].Name);
  //      AStoreEl.AppendChild(mObjStoreEl);
  //      SaveDataItemObject(mObjStoreEl, AData[i].AsObject, {AData[i].IsReference} False);
  //    end;
  //  end
  //  else if AData[i].IsMemo then
  //  begin
  //    SaveDataItemMemo(AStoreEl, AData[i].Name, AData[i].AsPersist);
  //  end
  //  else if AData[i].IsInterface then
  //  begin
  //    //maybe in future some special interface
  //  end
  //  else
  //  begin
  //    SaveDataItemValue(AStoreEl, AData[i].Name, AData[i].AsPersist);
  //  end;
  //end;
end;

procedure TXmlStore.Write(AStoreEl: TDOMElement; AData: IRBData);
//var
//  i: integer;
//  mDataList: IDataList;
begin
  //for i := 0 to AData.Count - 1 do begin
  //  if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IDataList, mDataList) then
  //    WriteList(AStoreEl, AData[i].Name, mDataList)
  //  else if AData[i].IsObject and (AData[i].AsObject <> nil) then
  //    WriteObject(AStoreEl, AData[i].Name, AData[i].AsObject)
  //  else if AData[i].IsMemo then
  //    WriteMemo(AStoreEl, AData[i].Name, AData[i].AsPersist)
  //  else
  //    WriteValue(AStoreEl, AData[i].Name, AData[i].AsPersist);
  //end;
end;

procedure TXmlStore.WriteList(AStoreEl: TDOMElement; const AName: String{; const AList: IDataList});
//var
//  mStoreEl: TDOMElement;
//  mStoreItemEl: TDOMElement;
//  i: integer;
begin
  //if AList.Count = 0 then
  // Exit;
  //mStoreEl := Doc.CreateElement(AName);
  //AStoreEl.AppendChild(mStoreEl);
  //for i := 0 to AList.Count - 1 do
  //begin
  //  mStoreItemEl := Doc.CreateElement(cListItemTag);
  //  mStoreEl.AppendChild(mStoreItemEl);
  //  Write(mStoreItemEl, AList.Data[i]);
  //end;
end;

procedure TXmlStore.WriteObject(AStoreEl: TDOMElement; const AName: String; AObject: TObject);
var
  mObjStoreEl: TDOMElement;
  mData: IRBData;
begin
  mObjStoreEl := Doc.CreateElement(AName);
  AStoreEl.AppendChild(mObjStoreEl);
  if AObject is TStream then
    WriteStream(mObjStoreEl, AObject as TStream)
  else begin
    mData := TRBData.Create(AObject);
    Write(mObjStoreEl, mData);
  end;
end;

procedure TXmlStore.WriteStream(AStoreEl: TDOMElement; AValue: TStream);
var
  mMemoNode: TDOMCDATASection;
  mValue: DOMString;
  mEncoder: TBase64EncodingStream;
  mEncoded: TBytesStream;
begin
  if AValue <> nil then
  begin
    mEncoded := TBytesStream.Create;
    try
      mEncoder := TBase64EncodingStream.Create(mEncoded);
      try
        AValue.Position := 0;
        mEncoder.CopyFrom(AValue, AValue.Size);
      finally
        mEncoder.Free;
      end;
      mValue := TEncoding.ASCII.GetString(mEncoded.Bytes, 0, mEncoded.Size);
      mMemoNode := Doc.CreateCDATASection(mValue);
      AStoreEl.AppendChild(mMemoNode);
    finally
      mEncoded.Free;
    end;
  end;
end;

procedure TXmlStore.WriteMemo(AStoreEl: TDOMElement; const AName, AValue: string);
var
  mStoreEl: TDOMElement;
  mMemoNode: TDOMCDATASection;
begin
  if AValue <> '' then
  begin
    mStoreEl := Doc.CreateElement(AName);
    AStoreEl.AppendChild(mStoreEl);
    mMemoNode := Doc.CreateCDATASection(AValue);
    mStoreEl.AppendChild(mMemoNode);
  end;
end;

procedure TXmlStore.WriteValue(AStoreEl: TDOMElement; const AName, AValue: string);
begin
  if AValue <> '' then
    AStoreEl.AttribStrings[AName] := AValue;
end;

procedure TXmlStore.FillAutoFields(const AData: IRBData);
var
  i: Integer;
  mAttr: TCustomAttribute;
begin
  for i := 0 to AData.Count - 1 do begin
    mAttr := AData[i].FindAttribute(PersistAUTOAttribute);
    if mAttr <> nil then begin
      if (AData[i].TypeKind in [tkAString, tkWString]) then begin
        if AData[i].AsString = '' then
          AData[i].AsString := TGuid.NewGuid.ToString(True);
      end
      else
        raise Exception.CreateFmt('unsupported auto atribbute %s.%s', [AData.ClassName, AData[i].Name]);
    end;
  end;
end;

function TXmlStore.FindStoreEl(const AData: IRBData): TDOMElement;
var
  i: Integer;
  mAttr: TCustomAttribute;
begin
  for i := 0 to AData.Count - 1 do begin
    mAttr := AData[i].FindAttribute(PersistIDAttribute);
    if mAttr <> nil then begin
      case AData[i].TypeKind of
        tkAString, tkWString:
          Result := FindElement(fDoc.DocumentElement, './' + AData.ClassName + '/' + cListItemTag + '[@' + AData[i].Name + '=''' + AData[i].AsString + ''']');
        tkInteger:
          Result := FindElement(fDoc.DocumentElement, './' + AData.ClassName + '/' + cListItemTag + '[@' + AData[i].Name + '=' + AData[i].AsString + ']');
      else
        raise Exception.CreateFmt('unsupported id atribbute %s.%s', [AData.ClassName, AData[i].Name]);
      end;
    end;
  end;
end;

procedure TXmlStore.LoadData(AStoreEl: TDOMElement; AData: IRBData);
//var
//  i: integer;
//  mObjStoreEl: TDOMElement;
//  mData: IRBData;
//  mID: string;
begin
  //for i := 0 to AData.Count - 1 do
  //begin
  //  if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistMany) then
  //    LoadDataList(AStoreEl, AData[i])
  //  else
  //  if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistRef) then
  //  begin
  //    mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
  //    if mObjStoreEl <> nil then
  //      LoadDataItemRef(mObjStoreEl, AData[i].AsInterface as IPersistRef)
  //  end
  //  else if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IRBData) then
  //  begin
  //    mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
  //    if mObjStoreEl <> nil then
  //       LoadData(mObjStoreEl, AData[i].AsInterface as IRBData)
  //  end
  //  else
  //  if AData[i].IsObject then
  //  begin
  //    if AData[i].AsObject is TStream then begin
  //      LoadDataItemStream(AStoreEl, AData[i].Name, AData[i].AsObject as TStream);
  //    end
  //    else
  //    begin
  //      mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
  //      if mObjStoreEl = nil then
  //        Continue;
  //      mData := fFactory.CreateObject(AData[i].ClassName);
  //      mData.UnderObject := AData[i].AsObject;
  //      LoadDataItemObject(mObjStoreEl, mData);
  //    end;
  //  end
  //  else
  //  if AData[i].IsMemo then
  //  begin
  //    AData[i].AsPersist := LoadDataItemMemo(AStoreEl, AData[i].Name);
  //  end
  //  else
  //  if AData[i].IsID then
  //  begin
  //    mID := LoadDataItemValue(AStoreEl, AData[i].Name);
  //    // just because of possible older item stored without ID will not overwrite
  //    // id created for new object with empty value
  //    if mID <> '' then
  //      AData[i].AsPersist := mID;
  //  end
  //  else
  //  if AData[i].IsInterface then
  //  begin
  //    //maybe in future some special interface
  //  end
  //  else
  //  begin
  //    AData[i].AsPersist := LoadDataItemValue(AStoreEl, AData[i].Name);
  //  end;
  //end;
end;

procedure TXmlStore.Read(AStoreEl: TDOMElement; AData: IRBData);
//var
//  i: integer;
//  mObjStoreEl: TDOMElement;
//  mData: IRBData;
//  mID: string;
//  mDataList: IDataList;
begin
  //for i := 0 to AData.Count - 1 do
  //begin
  //  if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IDataList, mDataList) then
  //    ReadList(AStoreEl, AData[i].Name, mDataList)
  //  else if AData[i].IsObject then
  //    ReadObject(AStoreEl, AData[i].Name, AData[i].AsObject)
  //  else if AData[i].IsMemo then
  //    AData[i].AsPersist := ReadMemo(AStoreEl, AData[i].Name)
  //  else
  //    AData[i].AsPersist := ReadValue(AStoreEl, AData[i].Name);
  //end;
end;

procedure TXmlStore.ReadList(AStoreEl: TDOMElement; const AName: String{; const AList: IDataList});
//var
//  mStoreEl: TDOMElement;
//  mStoreItemEl: TDOMElement;
//  i: integer;
begin
  //mStoreEl := AStoreEl.FindNode(AName) as TDOMElement;
  //if mStoreEl = nil then
  //  Exit;
  //for i := 0 to mStoreEl.ChildNodes.Count - 1 do
  //begin
  //  mStoreItemEl := mStoreEl.ChildNodes[i] as TDOMElement;
  //  Read(mStoreItemEl, AList.Append);
  //end;
end;

procedure TXmlStore.ReadObject(AStoreEl: TDOMElement; const AName: String; AObject: TObject);
var
  mObjStoreEl: TDOMElement;
  mData: IRBData;
begin
  mObjStoreEl := AStoreEl.FindNode(AName) as TDOMElement;
  if mObjStoreEl = nil then
    Exit;
  if AObject is TStream then
    ReadStream(mObjStoreEl, AObject as TStream)
  else begin
    mData := TRBData.Create(AObject);
    Read(mObjStoreEl, mData);
  end;
end;

procedure TXmlStore.ReadStream(AStoreEl: TDOMElement; AValue: TStream);
var
  mMemoNode: TDOMCDATASection;
  mInstream: TBytesStream;
  mDecoder: TBase64DecodingStream;
begin
  mMemoNode := AStoreEl.ChildNodes[0] as TDOMCDATASection;
  mInstream := TBytesStream.Create(TEncoding.ASCII.GetBytes(mMemoNode.TextContent));
  try
    mDecoder := TBase64DecodingStream.Create(mInstream);
    try
      AValue.CopyFrom(mDecoder, mDecoder.Size);
    finally
      mDecoder.Free;
    end;
  finally
    mInstream.Free;
  end;
end;

function TXmlStore.ReadMemo(AStoreEl: TDOMElement; const AName: string): string;
var
  mStoreEl: TDOMElement;
  mMemoNode: TDOMCDATASection;
begin
  Result := '';
  mStoreEl := FindElement(AStoreEl, './' + AName);
  if mStoreEl = nil then
    Exit;
  mMemoNode := mStoreEl.ChildNodes[0] as TDOMCDATASection;
  Result := mMemoNode.TextContent;
end;

function TXmlStore.ReadValue(AStoreEl: TDOMElement; const AName: string): string;
begin
  Result := AStoreEl.AttribStrings[AName];
end;

procedure TXmlStore.CheckOpen;
begin
  if fDoc <> nil then
    raise Exception.Create('already opened');
end;

function TXmlStore.LoadDataItemValue(AStoreEl: TDOMElement; const AName: string): string;
begin
  Result := AStoreEl.AttribStrings[AName];
end;

function TXmlStore.LoadDataItemMemo(AStoreEl: TDOMElement; const AName: string
  ): string;
var
  mStoreEl: TDOMElement;
  mMemoNode: TDOMCDATASection;
begin
  Result := '';
  mStoreEl := FindElement(AStoreEl, './' + AName);
  if mStoreEl = nil then
    Exit;
  mMemoNode := mStoreEl.ChildNodes[0] as TDOMCDATASection;
  Result := mMemoNode.TextContent;
end;

procedure TXmlStore.LoadDataItemStream(AStoreEl: TDOMElement;
  const AName: string; AValue: TStream);
var
  mStoreEl: TDOMElement;
  mMemoNode: TDOMCDATASection;
  mInstream: TBytesStream;
  mDecoder: TBase64DecodingStream;
begin
  mStoreEl := FindElement(AStoreEl, './' + AName);
  if mStoreEl = nil then begin
    AValue.Size := 0;
    Exit;
  end;
  mMemoNode := mStoreEl.ChildNodes[0] as TDOMCDATASection;
  mInstream := TBytesStream.Create(TEncoding.ASCII.GetBytes(mMemoNode.TextContent));
  try
    mDecoder := TBase64DecodingStream.Create(mInstream);
    try
      AValue.CopyFrom(mDecoder, mDecoder.Size);
    finally
      mDecoder.Free;
    end;
  finally
    mInstream.Free;
  end;
end;

procedure TXmlStore.LoadDataItemObject(AStoreEl: TDOMElement; const AData: IRBData);
begin
  LoadData(AStoreEl, AData);
end;

procedure TXmlStore.LoadDataItemRef(AStoreEl: TDOMElement{; const AValue: IPersistRef});
//var
//  mSID: TSID;
//  mRefStoreEl: TDOMElement;
//  mData: IRBData;
begin
  //if AStoreEl.AttribStrings[cRefID] <> '' then
  //begin
  //  mSID := AStoreEl.AttribStrings[cRefID];
  //  AValue.SID := mSID;
  //end;
end;

procedure TXmlStore.LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
//var
//  mStoreEl: TDOMElement;
//  mStoreItemEl: TDOMElement;
//  i: integer;
//  mMany: IPersistMany;
begin
  //if ADataItem.IsObject then
  //  mMany := ADataItem.AsObject as IPersistMany
  //else if ADataItem.IsInterface then
  //  mMany := ADataItem.AsInterface as IPersistMany
  //else
  //  raise Exception.Create('not list');
  //mStoreEl := AStoreEl.FindNode(ADataItem.Name) as TDOMElement;
  //if mStoreEl = nil then
  //  Exit;
  //mMany.Count := mStoreEl.ChildNodes.Count;
  //for i := 0 to mStoreEl.ChildNodes.Count - 1 do
  //begin
  //  mStoreItemEl := mStoreEl.ChildNodes[i] as TDOMElement;
  //  if mMany.IsObject then
  //  begin
  //    // only when ID is stored, otherwise is nil
  //    if mStoreItemEl.NodeName = cListItemNilTag then
  //      mMany.AsObject[i] := nil
  //    else
  //      LoadDataItemObject(mStoreItemEl, mMany.AsPersistData[i]);
  //  end
  //  else
  //  if Supports(mMany, IPersistManyRefs) then
  //    LoadDataItemRef(mStoreItemEl, mMany.AsInterface[i] as IPersistRef)
  //  else
  //    mMany.AsPersist[i] := LoadDataItemValue(mStoreItemEl, cValue);
  //end;
end;

function TXmlStore.GetDataClassEl(const ADataClassName: string;
  ACanCreate: Boolean): TDOMElement;
begin
  Result := Doc.DocumentElement.FindNode(ADataClassName) as TDOMElement;
  if Result = nil then
  begin
    if ACanCreate then
    begin
      Result := Doc.CreateElement(ADataClassName);
      Doc.DocumentElement.AppendChild(Result);
    end
    //else
    //  EXMLStore.ClassNotExists(ADataClassName);
  end;
end;

class function TXmlStore.FindElement(const AContextEl: TDOMElement;
  const AXPath: string): TDOMElement;
var
  mXPV: TXPathVariable;
begin
  mXPV := EvaluateXPathExpression(AXPath, AContextEl);
  case mXPV.AsNodeSet.Count of
    0: Result := nil;
    1: Result := TDOMElement(mXPV.AsNodeSet[0]);
    else
      EXMLStore.MoreObjectWithNameExists(AContextEl.NodeName, AXPath);
  end;
end;

procedure TXmlStore.SaveDataItemValue(AStoreEl: TDOMElement; const AName,
  AValue: string);
begin
  if AValue <> '' then
    AStoreEl.AttribStrings[AName] := AValue;
end;

procedure TXmlStore.SaveDataItemMemo(AStoreEl: TDOMElement; const AName,
  AValue: string);
var
  mStoreEl: TDOMElement;
  mMemoNode: TDOMCDATASection;
begin
  if AValue <> '' then
  begin
    mStoreEl := Doc.CreateElement(AName);
    AStoreEl.AppendChild(mStoreEl);
    mMemoNode := Doc.CreateCDATASection(AValue);
    mStoreEl.AppendChild(mMemoNode);
  end;
end;

procedure TXmlStore.SaveDataItemStream(AStoreEl: TDOMElement; AValue: TStream);
var
  mMemoNode: TDOMCDATASection;
  mValue: DOMString;
  mEncoder: TBase64EncodingStream;
  mEncoded: TBytesStream;
begin
  if AValue <> nil then
  begin
    mEncoded := TBytesStream.Create;
    try
      mEncoder := TBase64EncodingStream.Create(mEncoded);
      try
        AValue.Position := 0;
        mEncoder.CopyFrom(AValue, AValue.Size);
      finally
        mEncoder.Free;
      end;
      mValue := TEncoding.ASCII.GetString(mEncoded.Bytes, 0, mEncoded.Size);
      mMemoNode := Doc.CreateCDATASection(mValue);
      AStoreEl.AppendChild(mMemoNode);
    finally
      mEncoded.Free;
    end;
  end;
end;

procedure TXmlStore.SaveDataItemObject(AStoreEl: TDOMElement;
  const AValue: TObject; AIsReference: Boolean);
//var
//  mSID: TSID;
//  mData: IRBData;
begin
  //if AValue is TStream then
  //  SaveDataItemStream(AStoreEl, AValue as TStream)
  //else
  //if AValue <> nil then begin
  //  mData := fFactory.CreateObject(AValue.ClassName);
  //  mData.UnderObject := AValue;
  //  SaveData(AStoreEl, mData);
  //end;
end;

procedure TXmlStore.SaveDataItemRef(AStoreEl: TDOMElement; const AName: string{;
  const AValue: IPersistRef});
var
  mObjStoreEl: TDOMElement;
begin
  //if (AValue <> nil) and (AValue.Data <> nil) then
  //begin
  //  mObjStoreEl := Doc.CreateElement(AName);
  //  AStoreEl.AppendChild(mObjStoreEl);
  //  //SaveDataItemValue(mObjStoreEl, cRefClass, AValue.Data.ClassName);
  //  //SaveDataItemValue(mObjStoreEl, cRefID, AValue.SID);
  //end;
end;

procedure TXmlStore.SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
//var
//  mStoreEl: TDOMElement;
//  mStoreItemEl: TDOMElement;
//  i: integer;
//  mMany: IPersistMany;
begin
  //if ADataItem.IsObject then
  //  mMany := ADataItem.AsObject as IPersistMany
  //else if ADataItem.IsInterface then
  //  mMany := ADataItem.AsInterface as IPersistMany
  //else
  //  raise Exception.Create('not list');
  //if mMany.Count = 0 then
  // Exit;
  //mStoreEl := Doc.CreateElement(ADataItem.Name);
  //AStoreEl.AppendChild(mStoreEl);
  //for i := 0 to mMany.Count - 1 do
  //begin
  //  // create element for store list item
  //  if mMany.IsObject and (mMany.AsObject[i] = nil) then
  //  begin
  //    mStoreItemEl := Doc.CreateElement(cListItemNilTag);
  //    mStoreEl.AppendChild(mStoreItemEl);
  //  end
  //  else
  //  if Supports(mMany, IPersistManyRefs) then
  //   SaveDataItemRef(mStoreEl, cListItemTag, mMany.AsInterface[i] as IPersistRef)
  //  else
  //  begin
  //    mStoreItemEl := Doc.CreateElement(cListItemTag);
  //    mStoreEl.AppendChild(mStoreItemEl);
  //    // store data
  //    if mMany.IsObject then
  //      SaveDataItemObject(mStoreItemEl, mMany.AsObject[i], False)
  //    else
  //      SaveDataItemValue(mStoreItemEl, cValue, mMany.AsPersist[i]);
  //  end;
  //end;
end;

procedure TXmlStore.Save2(const AData: IRBData);
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  mOriginalEl: TDOMElement;
begin
  FillAutoFields(AData);
  mClassEl := GetDataClassEl(AData.ClassName, True);
  mStoreEl := Doc.CreateElement(cListItemTag);
  mOriginalEl := FindStoreEl(AData);
  if mOriginalEl <> nil then
  begin
    mClassEl.InsertBefore(mStoreEl, mOriginalEl);
    mOriginalEl.ParentNode.DetachChild(mOriginalEl);
    mOriginalEl.Free;
  end
  else
  begin
    mClassEl.AppendChild(mStoreEl);
  end;
  Write(mStoreEl, AData);
end;

procedure TXmlStore.Delete2(const AData: IRBData);
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  mOriginalEl: TDOMElement;
begin
  mOriginalEl := FindStoreEl(AData);
  if mOriginalEl <> nil then
  begin
    mOriginalEl.ParentNode.DetachChild(mOriginalEl);
    mOriginalEl.Free;
  end;
end;

procedure TXmlStore.Open;
begin
  CheckOpen;
  if (fFile <> '') and FileExists(fFile) then
    ReadXMLFile(fDoc, fFile);
  if fDoc = nil then
  begin
    fDoc := TXMLDocument.Create;
    fDoc.AppendChild(fDoc.CreateElement(cRoot));
  end;
end;

procedure TXmlStore.Open(const AFile: string);
begin
  CheckOpen;
  fFile := ExpandFileName(AFile);
  Open;
end;

procedure TXmlStore.Open(const AStream: TStream);
begin
  CheckOpen;
  ReadXMLFile(fDoc, AStream);
end;

procedure TXmlStore.Close;
begin
  Flush;
  FreeAndNil(fDoc);
end;

procedure TXmlStore.Close(const AStream: TStream);
begin
  WriteXMLFile(fDoc, AStream);
  FreeAndNil(fDoc);
end;

function TXmlStore.IsOpened: Boolean;
begin
  Result := fDoc <> nil;
end;

procedure TXmlStore.Flush;
begin
  if (fFile <> '') and (fDoc <> nil) then begin
    WriteXMLFile(Doc, fFile);
  end;
end;

function TXmlStore.Select2(const AClass: string): IRBDataEnumerable;
begin
  Result := TSelector.Create(Self, GetDataClassEl(AClass, False));
end;

destructor TXmlStore.Destroy;
begin
  Flush;
  inherited Destroy;
end;

end.

