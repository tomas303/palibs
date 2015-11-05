unit trl_upersistxml;

interface

uses
  trl_irttibroker, trl_ipersist, trl_ifactory,
  trl_urttibroker,
  DOM, XMLRead, XMLWrite, xpath,
  SysUtils, fgl, trl_ipersiststore;

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

    { TSIDManager }

    TSIDManager = class
    private
     fLast: integer;
     function GetStoreSettingsElement(ADoc: TXMLDocument): TDOMElement;
    public
      procedure Load(ADoc: TXMLDocument);
      procedure Save(ADoc: TXMLDocument);
      function New: TSID;
    end;

  private
    const
      cListItemTag = 'i';
      cListItemNilTag = 'n';
  private
    fFactory: IPersistFactory;
    fFile: string;
    fDoc: TXMLDocument;
    fSIDMgr: TSIDManager;
    function GetDoc: TXMLDocument;
    function GetDataClassEl(const ADataClassName: string; ACanCreate: Boolean): TDOMElement;
    class function FindElement(const AContextEl: TDOMElement; const AXPath: string): TDOMElement;
    function FindStoreElForSID(const ADataClassEl: TDOMElement; const ASID: TSID): TDOMElement; overload;
    function FindStoreElForSID(const ASID: TSID): TDOMElement; overload;
    function GetDataStoreElForSID(const ADataClassEl: TDOMElement; const ASID: TSID): TDOMElement; overload;
    function GetDataStoreElForSID(const ASID: TSID): TDOMElement; overload;
    function FindStoreElForName(const ADataClassEl: TDOMElement; const AName: string): TDOMElement; overload;
    function FindStoreElForName(const AName: string): TDOMElement; overload;
    function GetDataStoreElForName(const ADataClassEl: TDOMElement; const AName: string): TDOMElement; overload;
    function GetDataStoreElForName(const AName: string): TDOMElement; overload;
    //save
    procedure SaveDataItemValue(AStoreEl: TDOMElement; const AName, AValue: string);
    procedure SaveDataItemMemo(AStoreEl: TDOMElement; const AName, AValue: string);
    procedure SaveDataItemObject(AStoreEl: TDOMElement; const AName: string;
      const AValue: TObject; AIsReference: Boolean);
    procedure SaveDataItemRef(AStoreEl: TDOMElement; const AName: string;
      const AValue: IPersistRef);
    procedure SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure SaveData(AStoreEl: TDOMElement; AData: IRBData);
    //load
    function LoadDataItemValue(AStoreEl: TDOMElement; const AName: string): string;
    function LoadDataItemMemo(AStoreEl: TDOMElement; const AName: string): string;
    procedure LoadDataItemObject(AStoreEl: TDOMElement; const AData: IRBData);
    procedure LoadDataItemRef(AStoreEl: TDOMElement; const AValue: IPersistRef);
    procedure LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure LoadData(AStoreEl: TDOMElement; AData: IRBData);
  protected
    property Doc: TXMLDocument read GetDoc;
  public
    constructor Create(AFactory: IFactory; const AFile: string);
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // IPersistStoreDevice
    procedure Load(const ASID: TSID; AData: IRBData);
    procedure Save(const ASID: TSID; AData: IRBData);
    procedure Delete(const ASID: TSID);
    function NewSID: TSID;
    function GetSIDClass(const ASID: TSID): string;
    procedure Open;
    procedure Close;
    procedure Flush;
    function GetSIDs(const AClass: string): ISIDList;
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

{ TXmlStore.TSIDManager }

function TXmlStore.TSIDManager.GetStoreSettingsElement(ADoc: TXMLDocument): TDOMElement;
begin
  Result := FindElement(ADoc.DocumentElement, './' + cStoreSettings);
  if Result = nil then
  begin
    Result := ADoc.CreateElement(cStoreSettings);
    ADoc.DocumentElement.AppendChild(Result);
  end;
end;

procedure TXmlStore.TSIDManager.Load(ADoc: TXMLDocument);
var
  mEl: TDOMElement;
begin
  mEl := GetStoreSettingsElement(ADoc);
  fLast := StrToIntDef(mEl.AttribStrings[cLastSid], 1);
end;

procedure TXmlStore.TSIDManager.Save(ADoc: TXMLDocument);
var
  mEl: TDOMElement;
begin
  mEl := GetStoreSettingsElement(ADoc);
  mEl.AttribStrings[cLastSid] := IntToStr(fLast);
end;

function TXmlStore.TSIDManager.New: TSID;
begin
  inc(fLast);
  Result := fLast;
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
  for i := 0 to AData.Count - 1 do
  begin
    if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistMany) then
      SaveDataList(AStoreEl, AData[i])
    else if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistRef) then
        SaveDataItemRef(AStoreEl, AData[i].Name, AData[i].AsInterface as IPersistRef)
    else if AData[i].IsObject then
    begin
      if AData[i].AsObject <> nil then
      begin
        mObjStoreEl := Doc.CreateElement(AData[i].Name);
        AStoreEl.AppendChild(mObjStoreEl);
        SaveDataItemObject(mObjStoreEl, AData[i].Name, AData[i].AsObject, {AData[i].IsReference} False);
      end;
    end
    //else if AData[i].IsMemo then
    //begin
    //  SaveDataItemMemo(AStoreEl, AData[i].Name, AData[i].AsPersist);
    //end
    else
    begin
      SaveDataItemValue(AStoreEl, AData[i].Name, AData[i].AsPersist);
    end;
  end;
end;

procedure TXmlStore.LoadData(AStoreEl: TDOMElement; AData: IRBData);
var
  i: integer;
  mObjStoreEl: TDOMElement;
  mData: IRBData;
begin
  for i := 0 to AData.Count - 1 do
  begin
    if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistMany) then
      LoadDataList(AStoreEl, AData[i])
    else
    if (AData[i].IsInterface) and Supports(AData[i].AsInterface, IPersistRef) then
    begin
      mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
      if mObjStoreEl <> nil then
        LoadDataItemRef(mObjStoreEl, AData[i].AsInterface as IPersistRef)
    end
    else
    if AData[i].IsObject then
    begin
      mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
      if mObjStoreEl = nil then
        Continue;
      mData := fFactory.CreateObject(AData[i].ClassName);
      mData.UnderObject := AData[i].AsObject;
      LoadDataItemObject(mObjStoreEl, mData);
    end
    else
    //if AData[i].IsMemo then
    //begin
    //  AData[i].AsPersist := LoadDataItemMemo(AStoreEl, AData[i].Name);
    //end
    //else
    begin
      AData[i].AsPersist := LoadDataItemValue(AStoreEl, AData[i].Name);
    end;
  end;
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
  mStoreEl := FindElement(AStoreEl, './' + cMemo);
  if mStoreEl = nil then
    Exit;
  mMemoNode := mStoreEl.ChildNodes[0] as TDOMCDATASection;
  Result := mMemoNode.TextContent;
end;

procedure TXmlStore.LoadDataItemObject(AStoreEl: TDOMElement; const AData: IRBData);
begin
  LoadData(AStoreEl, AData);
end;

procedure TXmlStore.LoadDataItemRef(AStoreEl: TDOMElement; const AValue: IPersistRef);
var
  mSID: TSID;
  mRefStoreEl: TDOMElement;
  mData: IRBData;
begin
  if AStoreEl.AttribStrings[cRefID] <> '' then
  begin
    mSID := AStoreEl.AttribStrings[cRefID];
    AValue.SID := mSID;
  end;
end;

procedure TXmlStore.LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
var
  mStoreEl: TDOMElement;
  mStoreItemEl: TDOMElement;
  i: integer;
  mMany: IPersistMany;
begin
  if ADataItem.IsObject then
    mMany := ADataItem.AsObject as IPersistMany
  else if ADataItem.IsInterface then
    mMany := ADataItem.AsInterface as IPersistMany
  else
    raise Exception.Create('not list');
  mStoreEl := AStoreEl.FindNode(ADataItem.Name) as TDOMElement;
  if mStoreEl = nil then
    Exit;
  mMany.Count := mStoreEl.ChildNodes.Count;
  for i := 0 to mStoreEl.ChildNodes.Count - 1 do
  begin
    mStoreItemEl := mStoreEl.ChildNodes[i] as TDOMElement;
    if mMany.IsObject then
    begin
      // only when ID is stored, otherwise is nil
      if mStoreItemEl.NodeName = cListItemNilTag then
        mMany.AsObject[i] := nil
      else
        LoadDataItemObject(mStoreItemEl, mMany.AsPersistData[i]);
    end
    else
    if Supports(mMany, IPersistManyRefs) then
      LoadDataItemRef(mStoreItemEl, mMany.AsInterface[i] as IPersistRef)
    else
      mMany.AsPersist[i] := LoadDataItemValue(mStoreItemEl, cValue);
  end;
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

function TXmlStore.GetDataStoreElForSID(const ADataClassEl: TDOMElement;
  const ASID: TSID): TDOMElement;
begin
  Result := FindStoreElForSID(ADataClassEl, ASID);
  if Result = nil then
    EXMLStore.ClassForSIDNotExists(ASID);
end;

function TXmlStore.FindStoreElForSID(const ASID: TSID): TDOMElement;
begin
  Result := FindElement(Doc.DocumentElement, '//*[@SID=''' + ASID + ''']');
end;

function TXmlStore.FindStoreElForSID(const ADataClassEl: TDOMElement;
  const ASID: TSID): TDOMElement;
begin
  Result := FindElement(ADataClassEl, './*[@SID=''' + ASID + ''']');
end;

function TXmlStore.GetDataStoreElForSID(const ASID: TSID): TDOMElement;
begin
  Result := FindStoreElForSID(ASID);
  if Result = nil then
    EXMLStore.ClassForSIDNotExists(ASID);
end;

function TXmlStore.FindStoreElForName(const ADataClassEl: TDOMElement;
  const AName: string): TDOMElement;
begin
  Result := FindElement(ADataClassEl, './*[@Name=''' + AName + ''']');
end;

function TXmlStore.FindStoreElForName(const AName: string): TDOMElement;
begin
  Result := FindElement(Doc.DocumentElement, '//*[@Name=''' + AName + ''']');
end;

function TXmlStore.GetDataStoreElForName(const ADataClassEl: TDOMElement;
  const AName: string): TDOMElement;
begin
  Result := FindStoreElForName(ADataClassEl, AName);
  if Result = nil then
    EXMLStore.ClassForNameNotExists(AName);
end;

function TXmlStore.GetDataStoreElForName(const AName: string): TDOMElement;
begin
  Result := FindStoreElForName(AName);
  if Result = nil then
    EXMLStore.ClassForNameNotExists(AName);
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
    mStoreEl := Doc.CreateElement(cMemo);
    mStoreEl.AttribStrings[AName] := '';
    AStoreEl.AppendChild(mStoreEl);
    mMemoNode := Doc.CreateCDATASection(AValue);
    mStoreEl.AppendChild(mMemoNode);
  end;
end;

procedure TXmlStore.SaveDataItemObject(AStoreEl: TDOMElement;
  const AName: string; const AValue: TObject; AIsReference: Boolean);
var
  mSID: TSID;
  mData: IRBData;
begin
  //if AIsReference then
  //begin
  //  if AValue <> nil then
  //  begin
  //    mSID := fStoreCache.FindSID(AValue);
  //    SaveDataItemValue(AStoreEl, cRefID, mSID);
  //  end;
  //end
  //else
  //begin
    if AValue <> nil then begin
      mData := fFactory.CreateObject(AValue.ClassName);
      mData.UnderObject := AValue;
      SaveData(AStoreEl, mData);
    end;
  //end;
end;

procedure TXmlStore.SaveDataItemRef(AStoreEl: TDOMElement; const AName: string;
  const AValue: IPersistRef);
var
  mObjStoreEl: TDOMElement;
begin
  if (AValue <> nil) and (AValue.Data <> nil) then
  begin
    //if AValue.SID.IsClear then
    //  fStore.Save
    mObjStoreEl := Doc.CreateElement(AName);
    AStoreEl.AppendChild(mObjStoreEl);
    SaveDataItemValue(mObjStoreEl, cRefClass, AValue.Data.ClassName);
    SaveDataItemValue(mObjStoreEl, cRefID, AValue.SID);
  end;
end;

procedure TXmlStore.SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
var
  mStoreEl: TDOMElement;
  mStoreItemEl: TDOMElement;
  i: integer;
  mMany: IPersistMany;
begin
  if ADataItem.IsObject then
    mMany := ADataItem.AsObject as IPersistMany
  else if ADataItem.IsInterface then
    mMany := ADataItem.AsInterface as IPersistMany
  else
    raise Exception.Create('not list');
  if mMany.Count = 0 then
   Exit;
  mStoreEl := Doc.CreateElement(ADataItem.Name);
  AStoreEl.AppendChild(mStoreEl);
  for i := 0 to mMany.Count - 1 do
  begin
    // create element for store list item
    if mMany.IsObject and (mMany.AsObject[i] = nil) then
    begin
      mStoreItemEl := Doc.CreateElement(cListItemNilTag);
      mStoreEl.AppendChild(mStoreItemEl);
    end
    else
    if Supports(mMany, IPersistManyRefs) then
     SaveDataItemRef(mStoreEl, cListItemTag, mMany.AsInterface[i] as IPersistRef)
    else
    begin
      mStoreItemEl := Doc.CreateElement(cListItemTag);
      mStoreEl.AppendChild(mStoreItemEl);
      // store data
      if mMany.IsObject then
        SaveDataItemObject(mStoreItemEl, ADataItem.Name, mMany.AsObject[i], False)
      else
        SaveDataItemValue(mStoreItemEl, cValue, mMany.AsPersist[i]);
    end;
  end;
end;

procedure TXmlStore.Load(const ASID: TSID; AData: IRBData);
var
  mStoreEl: TDOMElement;
begin
  mStoreEl := FindElement(Doc.DocumentElement, './' + AData.ClassName + '/' + cListItemTag + '[@' + cSID + '=''' + ASID + ''']');
  if mStoreEl <> nil then begin
    LoadData(mStoreEl, AData);
  end;
end;

procedure TXmlStore.Save(const ASID: TSID; AData: IRBData);
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  mOriginalEl: TDOMElement;
begin
  mClassEl := GetDataClassEl(AData.ClassName, True);
  mStoreEl := Doc.CreateElement(cListItemTag);
  mOriginalEl := FindStoreElForSID(mClassEl, ASID);
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
  mStoreEl.AttribStrings[cSID] := ASID;
  //
  SaveData(mStoreEl, AData);
end;

procedure TXmlStore.Delete(const ASID: TSID);
begin

end;

function TXmlStore.NewSID: TSID;
begin
  Result := fSIDMgr.New;
end;

function TXmlStore.GetSIDClass(const ASID: TSID): string;
var
  mStoreEl: TDOMElement;
begin
  Result := '';
  mStoreEl := FindStoreElForSID(ASID);
  if mStoreEl <> nil then
    Result := mStoreEl.ParentNode.NodeName;
end;

procedure TXmlStore.Open;
begin
  if fDoc <> nil then
    raise Exception.Create('already opened');
  if (fFile <> '') and FileExists(fFile) then
    ReadXMLFile(fDoc, fFile);
  if fDoc = nil then
  begin
    fDoc := TXMLDocument.Create;
    fDoc.AppendChild(fDoc.CreateElement(cRoot));
  end;
  fSIDMgr.Load(fDoc);
end;

procedure TXmlStore.Close;
begin
  Flush;
  FreeAndNil(fDoc);
end;

procedure TXmlStore.Flush;
begin
  fSIDMgr.Save(Doc);
  WriteXMLFile(Doc, fFile);
end;

function TXmlStore.GetSIDs(const AClass: string): ISIDList;
var
  mClassEl: TDOMElement;
  i: integer;
begin
  Result := Factory.Create(ISIDList) as ISIDList;
  mClassEl := GetDataClassEl(AClass, False);
  if mClassEl = nil then
    Exit;
  Result.Count := mClassEl.ChildNodes.Count;
  for i :=  0 to mClassEl.ChildNodes.Count - 1 do
  begin
    Result[i] := (mClassEl.ChildNodes[i] as TDOMElement).AttribStrings[cSID];
  end;
end;

constructor TXmlStore.Create(AFactory: IFactory; const AFile: string);
begin
//  fFactory := AFactory;
  fFile := AFile;
  Create;
end;

constructor TXmlStore.Create;
begin
//  GetDoc;
end;

destructor TXmlStore.Destroy;
begin
  Flush;
  FreeAndNil(fSIDMgr);
  inherited Destroy;
end;

procedure TXmlStore.AfterConstruction;
begin
  inherited AfterConstruction;
  fSIDMgr := TSIDManager.Create;
end;

end.

