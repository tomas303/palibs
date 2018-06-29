unit rtti_serializer_uXmlStore;

interface

uses
  rtti_broker_iBroker, DOM, XMLRead, XMLWrite,
  SysUtils, xpath, fgl, rtti_broker_uData;

const
  cSID = 'SID';
  cRefID = 'refid';
  cValue = 'Value';
  cRoot = 'root';
  cStoreSettings = 'storesettings';
  cLastSid = 'lastsid';
  cMemo = 'memo';

type

  { TXmlStore }
  TXmlStore = class(TInterfacedObject, IRBStore, IRBDataQuery)
  private type

    { TLoadCache }

    TLoadCache = class
    private type
      TCache = specialize TFPGMap<integer, TObject>;
    private
      fCache: TCache;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(AID: integer; AObject: TObject);
      procedure Remove(AID: integer);
      function Find(AID: integer): TObject;
    end;

    TSID = string;

    { TStoreCache }

    TStoreCache = class
    private type
      TSIDCache = specialize TFPGMap<TSID, TObject>;
      TObjCache = specialize TFPGMap<Pointer, TSID>;
    private
      fSIDCache: TSIDCache;
      fObjCache: TObjCache;
    public
      constructor Create;
      destructor Destroy; override;
      function FindObject(const ASID: TSID): TObject;
      function FindSID(const AObject: TObject): TSID;
      procedure Add(const ASID: TSID; const AObject: TObject);
      procedure Remove(const ASID: TSID); overload;
      procedure Remove(const AObject: TObject); overload;
    end;

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
    fFactory: IRBFactory;
    fFile: string;
    fDoc: TXMLDocument;
    fLoadCache: TLoadCache;
    fStoreCache: TStoreCache;
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
    procedure SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure SaveData(AStoreEl: TDOMElement; AData: IRBData);
    //load
    function LoadDataItemValue(AStoreEl: TDOMElement; const AName: string): string;
    function LoadDataItemMemo(AStoreEl: TDOMElement; const AName: string): string;
    function LoadDataItemObject(AStoreEl: TDOMElement; const ADataItem: IRBDataItem): TObject;
    procedure LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
    procedure LoadData(AStoreEl: TDOMElement; AData: IRBData);
  protected
    property Doc: TXMLDocument read GetDoc;
  public
    constructor Create(AFactory: IRBFactory; const AFile: string);
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // IRBStore
    procedure Save(AData: TObject);
    procedure Save(AData: IRBData);
    function  Load(const AClass: string; const AProperty, AValue: string): TObject;
    function LoadList(const AClass: string): IRBDataList;
    procedure Flush;
    procedure Delete(AData: TObject);
    procedure Delete(AData: IRBData);

    // IRBDataQuery
    function Retrive(const AClass: string): IRBDataList;
  published
    property Factory: IRBFactory read fFactory write fFactory;
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
  Result := IntToStr(fLast);
end;

{ TXmlStore.TStoreCache }

constructor TXmlStore.TStoreCache.Create;
begin
  fSIDCache := TSIDCache.Create;
  fObjCache := TObjCache.Create;
end;

destructor TXmlStore.TStoreCache.Destroy;
begin
  FreeAndNil(fSIDCache);
  FreeAndNil(fObjCache);
  inherited Destroy;
end;

function TXmlStore.TStoreCache.FindObject(const ASID: TSID): TObject;
var
  mIndex: integer;
begin
  mIndex := fSIDCache.IndexOf(ASID);
  if mIndex = -1 then
    Result := nil
  else
    Result := fSIDCache.Data[mIndex];
end;

function TXmlStore.TStoreCache.FindSID(const AObject: TObject): TSID;
var
  mIndex: integer;
begin
  mIndex := fObjCache.IndexOf(AObject);
  if mIndex = -1 then
    Result := ''
  else
    Result := fObjCache.Data[mIndex];
end;

procedure TXmlStore.TStoreCache.Add(const ASID: TSID; const AObject: TObject);
begin
  fSIDCache.Add(ASID, AObject);
  fObjCache.Add(AObject, ASID);
end;

procedure TXmlStore.TStoreCache.Remove(const ASID: TSID);
var
  mIndex: integer;
  mObj: TObject;
begin
  mIndex := fSIDCache.IndexOf(ASID);
  if mIndex <> -1 then
  begin
    mObj := fSIDCache.Data[mIndex];
    fSIDCache.Delete(mIndex);
    mIndex := fObjCache.IndexOf(mObj);
    if mIndex <> -1 then
    begin
      fObjCache.Delete(mIndex);
    end;
    mObj.Free;
  end;
end;

procedure TXmlStore.TStoreCache.Remove(const AObject: TObject);
var
  mIndex: integer;
  mSID: TSID;
begin
  mIndex := fObjCache.IndexOf(AObject);
  if mIndex <> -1 then
  begin
    mSID := fObjCache.Data[mIndex];
    fObjCache.Delete(mIndex);
    mIndex := fSIDCache.IndexOf(mSID);
    if mIndex <> -1 then
    begin
      fSIDCache.Delete(mIndex);
    end;
  end;
  AObject.Free;
end;

{ TXmlStore.TLoadCache }

constructor TXmlStore.TLoadCache.Create;
begin
  fCache := TCache.Create;
end;

destructor TXmlStore.TLoadCache.Destroy;
begin
  FreeAndNil(fCache);
  inherited Destroy;
end;

procedure TXmlStore.TLoadCache.Add(AID: integer; AObject: TObject);
begin
  fCache.Add(AID, AObject);
end;

procedure TXmlStore.TLoadCache.Remove(AID: integer);
begin
  fCache.Remove(AID);
end;

function TXmlStore.TLoadCache.Find(AID: integer): TObject;
var
  mIndex: integer;
begin
  Result := nil;
  mIndex := fCache.IndexOf(AID);
  if mIndex <> -1 then
    Result := fCache.Data[mIndex];
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
  if fDoc = nil then
  begin
    if (fFile <> '') and FileExists(fFile) then
      ReadXMLFile(fDoc, fFile);
    if fDoc = nil then
    begin
      fDoc := TXMLDocument.Create;
      fDoc.AppendChild(fDoc.CreateElement(cRoot));
    end;
    fSIDMgr.Load(fDoc);
  end;
  Result := fDoc;
end;

procedure TXmlStore.SaveData(AStoreEl: TDOMElement; AData: IRBData);
var
  i: integer;
  mObjStoreEl: TDOMElement;
begin
  for i := 0 to AData.Count - 1 do
  begin
    if AData[i].IsListCounter then
      Continue;
    if AData[i].IsNotStored then
      Continue;
    if AData[i].IsList then
      SaveDataList(AStoreEl, AData[i])
    else
    if AData[i].IsObject then
    begin
      if AData[i].AsObject <> nil then
      begin
        mObjStoreEl := Doc.CreateElement(AData[i].Name);
        AStoreEl.AppendChild(mObjStoreEl);
        SaveDataItemObject(mObjStoreEl, AData[i].Name, AData[i].AsObject, AData[i].IsReference);
      end;
    end
    else if AData[i].IsMemo then
    begin
      SaveDataItemMemo(AStoreEl, AData[i].Name, AData[i].AsPersist);
    end
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
begin
  for i := 0 to AData.Count - 1 do
  begin
    if AData[i].IsListCounter then
      Continue;
    if AData[i].IsList then
      LoadDataList(AStoreEl, AData[i])
    else
    if AData[i].IsObject then
    begin
      mObjStoreEl := AStoreEl.FindNode(AData[i].Name) as TDOMElement;
      if mObjStoreEl = nil then
        Continue;
      AData[i].AsObject := LoadDataItemObject(mObjStoreEl, AData[i]);
    end
    else
    if AData[i].IsMemo then
    begin
      AData[i].AsPersist := LoadDataItemMemo(AStoreEl, AData[i].Name);
    end
    else
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

function TXmlStore.LoadDataItemObject(AStoreEl: TDOMElement; const ADataItem: IRBDataItem): TObject;
var
  mSID: TSID;
  mRefStoreEl: TDOMElement;
  mData: IRBData;
begin
  Result := nil;
  if ADataItem.IsReference then
  begin
    if AStoreEl.AttribStrings[cRefID] <> '' then
    begin
      mSID := AStoreEl.AttribStrings[cRefID];
      if mSID = '' then
        Exit;
      Result := fStoreCache.FindObject(mSID);
      if Result = nil then
      begin
        mRefStoreEl := FindStoreElForSID(mSID);
        if mRefStoreEl = nil then
          EXMLStore.CannotFindReferencedSID(AStoreEl.NodeName, mSID);
        Result := fFactory.CreateObject(mRefStoreEl.ParentNode.NodeName);
        if Supports(Result, IRBData) then
          mData := Result as IRBData
        else begin
          mData := TRBData.Create(Result);
        end;
        LoadData(mRefStoreEl, mData);
        fStoreCache.Add(mSID, Result);
      end;
    end;
  end
  else
  begin
    Result := fFactory.CreateObject(ADataItem.ClassName);
    LoadData(AStoreEl, Result as IRBData);
  end;
end;

procedure TXmlStore.LoadDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
var
  mStoreEl: TDOMElement;
  mStoreItemEl: TDOMElement;
  i: integer;
begin
  Assert(ADataItem.IsList);
  mStoreEl := AStoreEl.FindNode(ADataItem.Name) as TDOMElement;
  if mStoreEl = nil then
    Exit;
  ADataItem.ListCount := mStoreEl.ChildNodes.Count;
  for i := 0 to mStoreEl.ChildNodes.Count - 1 do
  begin
    mStoreItemEl := mStoreEl.ChildNodes[i] as TDOMElement;
    if ADataItem.IsObject then
    begin
      // only when ID is stored, otherwise is nil
      if mStoreItemEl.NodeName = cListItemNilTag then
        ADataItem.AsObjectList[i] := nil
      else
        ADataItem.AsObjectList[i] := LoadDataItemObject(mStoreItemEl, ADataItem);
    end
    else
      ADataItem.AsPersistList[i] := LoadDataItemValue(mStoreItemEl, cValue);
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
    else
      EXMLStore.ClassNotExists(ADataClassName);
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

function TXmlStore.LoadList(const AClass: string): IRBDataList;
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  i: integer;
  mObj: TObject;
  mSID: TSID;
  mData: IRBData;
begin
  Result := TRBDataList.Create;
  mClassEl := GetDataClassEl(AClass, True);
  for i := 0 to mClassEl.ChildNodes.Count - 1 do
  begin
    mStoreEl := mClassEl.ChildNodes[i] as TDOMElement;
    mSID := mStoreEl.AttribStrings[cSID];
    if mSID = '' then
      EXMLStore.CannotAddItemWithoutSIDToList(AClass);
    mObj := fStoreCache.FindObject(mSID);
    if mObj = nil then
    begin
      mObj := fFactory.CreateObject(AClass);
      if Supports(mObj, IRBData) then
        mData := mObj as IRBData
      else begin
        mData := TRBData.Create(mObj);
      end;
      LoadData(mStoreEl, mData);
      fStoreCache.Add(mSID, mObj);
    end;
    Result.Add(mObj);
  end;
end;

function TXmlStore.Load(const AClass: string; const AProperty, AValue: string): TObject;
var
  mStoreEl: TDOMElement;
  mData: IRBData;
  mSID: TSID;
begin
  Result := nil;
  mStoreEl := FindElement(Doc.DocumentElement, './' + AClass + '[@' + AProperty + '=''' + AValue + '''');
  if mStoreEl <> nil then begin
    mSID := mStoreEl.AttribStrings[cSID];
    if mSID = '' then
      EXMLStore.CannotAddItemWithoutSIDToList(AClass);
    Result := fStoreCache.FindObject(mSID);
    if Result = nil then
    begin
      Result := fFactory.CreateObject(AClass);
      if Supports(Result, IRBData) then
        mData := Result as IRBData
      else begin
        mData := TRBData.Create(Result);
      end;
      LoadData(mStoreEl, mData);
      fStoreCache.Add(mSID, Result);
    end;
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
begin
  if AIsReference then
  begin
    if AValue <> nil then
    begin
      mSID := fStoreCache.FindSID(AValue);
      SaveDataItemValue(AStoreEl, cRefID, mSID);
    end;
  end
  else
  begin
    if AValue <> nil then
      SaveData(AStoreEl, AValue as IRBData);
  end;
end;

procedure TXmlStore.SaveDataList(AStoreEl: TDOMElement; ADataItem: IRBDataItem);
var
  mStoreEl: TDOMElement;
  mStoreItemEl: TDOMElement;
  i: integer;
begin
  Assert(ADataItem.IsList);
  if ADataItem.ListCount = 0 then
   Exit;
  mStoreEl := Doc.CreateElement(ADataItem.Name);
  AStoreEl.AppendChild(mStoreEl);
  for i := 0 to ADataItem.ListCount - 1 do
  begin
    // create element for store list item
    if ADataItem.IsObject and (ADataItem.AsObjectList[i] = nil) then
    begin
      mStoreItemEl := Doc.CreateElement(cListItemNilTag);
      mStoreEl.AppendChild(mStoreItemEl);
    end else begin
      mStoreItemEl := Doc.CreateElement(cListItemTag);
      mStoreEl.AppendChild(mStoreItemEl);
      // store data
      if ADataItem.IsObject then
        SaveDataItemObject(mStoreItemEl, ADataItem.Name, ADataItem.AsObjectList[i], ADataItem.IsReference)
      else
        SaveDataItemValue(mStoreItemEl, cValue, ADataItem.AsPersistList[i]);
    end;
  end;
end;

procedure TXmlStore.Save(AData: IRBData);
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  mSID: TSID;
  mIsNew: Boolean;
begin
  mSID := fStoreCache.FindSID(AData.UnderObject);
  mIsNew := False;
  if mSID = '' then
  begin
    mSID := fSIDMgr.New;
    mIsNew := True;
  end;
  mClassEl := GetDataClassEl(AData.ClassName, True);
  mStoreEl := FindStoreElForSID(mClassEl, mSID);
  // when store, remove old data and create all new
  if mStoreEl <> nil then
  begin
    mStoreEl.ParentNode.DetachChild(mStoreEl);
    mStoreEl.Free;
  end;
  mStoreEl := Doc.CreateElement(cListItemTag);
  mClassEl.AppendChild(mStoreEl);
  mStoreEl.AttribStrings[cSID] := mSID;
  //
  SaveData(mStoreEl, AData);
  if mIsNew then
    fStoreCache.Add(mSID, AData.UnderObject);
end;

procedure TXmlStore.Flush;
begin
  fSIDMgr.Save(Doc);
  WriteXMLFile(Doc, fFile);
end;

procedure TXmlStore.Delete(AData: TObject);
begin
  if Supports(AData, IRBData) then
    Delete(AData as IRBData)
  else begin
    Delete(TRBData.Create(AData));
  end;
end;

procedure TXmlStore.Delete(AData: IRBData);
var
  mClassEl: TDOMElement;
  mStoreEl: TDOMElement;
  mSID: TSID;
begin
  mSID := fStoreCache.FindSID(AData.UnderObject);
  if mSID = '' then
  begin
    // without SID - not stored
    Exit;
  end;
  mClassEl := GetDataClassEl(AData.ClassName, True);
  mStoreEl := FindStoreElForSID(mClassEl, mSID);
  // remove data
  if mStoreEl = nil then
    raise EXMLStore.CreateFmt('Delete - no element find for %s[%s]', [AData.ClassName, mSID]);
  mStoreEl.ParentNode.DetachChild(mStoreEl);
  mStoreEl.Free;
end;

function TXmlStore.Retrive(const AClass: string): IRBDataList;
begin
  Result := LoadList(AClass);
end;

constructor TXmlStore.Create(AFactory: IRBFactory; const AFile: string);
begin
  fFactory := AFactory;
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
  FreeAndNil(fLoadCache);
  FreeAndNil(fStoreCache);
  inherited Destroy;
end;

procedure TXmlStore.AfterConstruction;
begin
  inherited AfterConstruction;
  fLoadCache := TLoadCache.Create;
  fStoreCache := TStoreCache.Create;
  fSIDMgr := TSIDManager.Create;
end;

procedure TXmlStore.Save(AData: TObject);
begin
  if Supports(AData, IRBData) then
    Save(AData as IRBData)
  else begin
    Save(TRBData.Create(AData));
  end;
end;


end.

