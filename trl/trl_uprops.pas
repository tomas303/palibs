unit trl_uprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_iprops, fgl;

type

  { TProp }

  TProp = class(TInterfacedObject, IProp)
  protected
    fName: string;
  protected
    //IProp
    function Equals(const AProp: IProp): Boolean; virtual;
    function Clone(const AName: string = ''): IProp; virtual; abstract;
    function GetName: string;
    function GetPropType: TPropType; virtual;
    function GetDebugInfo: string; virtual;
    function GetAsInteger: integer; virtual;
    procedure SetAsInteger(AValue: integer); virtual;
    function GetAsString: string; virtual;
    procedure SetAsString(AValue: string); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    function GetAsObject: TObject; virtual;
    procedure SetAsObject(AValue: TObject); virtual;
    function GetAsVariant: Variant; virtual;
    procedure SetAsVariant(AValue: Variant); virtual;
    function GetAsPtrInt: PtrInt; virtual;
    procedure SetAsPtrInt(AValue: PtrInt); virtual;
    function GetAsInterface: IUnknown; virtual;
    procedure SetAsInterface(AValue: IUnknown); virtual;
    function GetAsTGuid: TGuid; virtual;
    procedure SetAsTGuid(AValue: TGuid); virtual;
    property Name: string read GetName;
    property PropType: TPropType read GetPropType;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property AsPtrInt: PtrInt read GetAsPtrInt write SetAsPtrInt;
    property AsInterface: IUnknown read GetAsInterface write SetAsInterface;
    property AsTGuid: TGuid read GetAsTGuid write SetAsTGuid;
  public
    constructor Create(const AName: string);
  end;

  { TStringProp }

  TStringProp = class(TProp)
  protected
    fValue: String;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
  public
    constructor Create(const AName: string; const AValue: string);
  end;

  { TIntegerProp }

  TIntegerProp = class(TProp)
  protected
    fValue: Integer;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(AValue: integer); override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
  public
    constructor Create(const AName: string; const AValue: Integer);
  end;

  { TBooleanProp }

  TBooleanProp = class(TProp)
  protected
    fValue: Boolean;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(const AName: string; const AValue: Boolean);
  end;

  { TGuidProp }

  TGuidProp = class(TProp)
  protected
    fValue: TGuid;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsTGuid: TGuid; override;
    procedure SetAsTGuid(AValue: TGuid); override;
  public
    constructor Create(const AName: string; const AValue: TGuid);
  end;

  { TInterfaceProp }

  TInterfaceProp = class(TProp)
  protected
    fValue: IUnknown;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsInterface: IUnknown; override;
    procedure SetAsInterface(AValue: IUnknown); override;
  public
    constructor Create(const AName: string; const AValue: IUnknown);
  end;

  { TObjectProp }

  TObjectProp = class(TProp)
  protected
    fValue: TObject;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone(const AName: string = ''): IProp; override;
    function GetPropType: TPropType; override;
    function GetDebugInfo: string; override;
    function GetAsObject: TObject; override;
    procedure SetAsObject(AValue: TObject); override;
  public
    constructor Create(const AName: string; const AValue: TObject);
  end;

  { TProps }

  TProps = class(TInterfacedObject, IProps, IPropFinder)
  protected const
    cDefaultStr = '';
    cDefaultInt = 0;
    cDefaultBool = False;
    cDefaultGuid: TGUID = '{00000000-0000-0000-0000-000000000000}';
  protected type

    { TItems }

    TItems = class(specialize TFPGMapInterfacedObjectData<string, IProp>)
    protected
      procedure Deref(Item: Pointer); override;
    end;

  protected
    fItems: TItems;
    function IndexFit(const AIndex: integer): Boolean;
    procedure CheckIndexFit(const AIndex: integer);
  protected
    //IProps
    function Count: integer;
    function GetProp(AIndex: integer): IProp;
    property Prop[AIndex: integer]: IProp read GetProp; default;
    function GetPropByName(const AName: string): IProp;
    property PropByName[const AName: string]: IProp read GetPropByName;
    function Equals(const AProps: IProps): Boolean;
    function Clone: IProps;
    function Clone(ANames: array of string): IProps;
    function PropType(const AName: string): TPropType;
    function PropType(const AIndex: integer): TPropType;
    function Name(const AIndex: integer): string;
    function SetIt(const AName: string): IProps;
    function SetStr(const AName: string; const AValue: string): IProps;
    function SetInt(const AName: string; const AValue: integer): IProps;
    function SetBool(const AName: string; const AValue: Boolean): IProps;
    function SetGuid(const AName: string; const AValue: TGUID): IProps;
    function SetIntf(const AName: string; const AValue: IUnknown): IProps;
    function SetObject(const AName: string; const AValue: TObject): IProps;
    function SetStr(const AIndex: integer; const AValue: string): IProps;
    function SetInt(const AIndex: integer; const AValue: integer): IProps;
    function SetBool(const AIndex: integer; const AValue: Boolean): IProps;
    function SetGuid(const AIndex: integer; const AValue: TGUID): IProps;
    function SetIntf(const AIndex: integer; const AValue: IUnknown): IProps;
    function SetObject(const AIndex: integer; const AValue: TObject): IProps;
    function SetProp(const AName: string; const AProp: IProp): IProps;
    function SetProp(const AProp: IProp): IProps;
    function AsStr(const AName: string): string;
    function AsInt(const AName: string): integer;
    function AsBool(const AName: string): Boolean;
    function AsGuid(const AName: string): TGUID;
    function AsIntf(const AName: string): IUnknown;
    function AsObject(const AName: string): TObject;
    function AsStr(const AIndex: integer): string;
    function AsInt(const AIndex: integer): integer;
    function AsBool(const AIndex: integer): Boolean;
    function AsGuid(const AIndex: integer): TGUID;
    function AsIntf(const AIndex: integer): IUnknown;
    function AsObject(const AIndex: integer): TObject;
    function Diff(const AProps: IProps; AMode: TPropDiffMode): IProps;
    function Info: string;
  protected
    // IPropFinder
    function Find(const AID: string): IProp;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function New: IProps;
  end;

implementation

{ TObjectProp }

function TObjectProp.Equals(const AProp: IProp): Boolean;
begin
  Result := inherited Equals(AProp) and (AProp.GetAsObject = GetAsObject);
end;

function TObjectProp.Clone(const AName: string): IProp;
begin
  if AName <> '' then
    Result := TObjectProp.Create(AName, AsObject)
  else
    Result := TObjectProp.Create(Name, AsObject);
end;

function TObjectProp.GetPropType: TPropType;
begin
  Result := ptObject;
end;

function TObjectProp.GetDebugInfo: string;
begin
  if fValue = nil then
    Result := ''
  else
    Result := 'instance of ' + fValue.ClassName;
end;

function TObjectProp.GetAsObject: TObject;
begin
  Result := fValue;
end;

procedure TObjectProp.SetAsObject(AValue: TObject);
begin
  fValue := AValue;
end;

constructor TObjectProp.Create(const AName: string; const AValue: TObject);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TProps.TItems }

procedure TProps.TItems.Deref(Item: Pointer);
begin
  // in TFPGMapInterfacedObjectData.CopyData is error
  // TData(Dest^) := TData(Src^); will also ref. data
  // based on similar list it should be pointer(Dest^) := pointer(Src^);
  // -- that is why inherited is called 2x
  inherited Deref(Item);
  inherited Deref(Item);
end;

{ TInterfaceProp }

function TInterfaceProp.Equals(const AProp: IProp): Boolean;
begin
  Result := inherited Equals(AProp) and (AProp.GetAsInterface as IUnknown = GetAsInterface as IUnknown);
end;

function TInterfaceProp.GetPropType: TPropType;
begin
  Result := ptInterface;
end;

function TInterfaceProp.GetDebugInfo: string;
begin
  if fValue = nil then
    Result := ''
  else
    Result := 'interface of ' + (fValue as TObject).ClassName;
end;

function TInterfaceProp.GetAsInterface: IUnknown;
begin
  Result := fValue;
end;

procedure TInterfaceProp.SetAsInterface(AValue: IUnknown);
begin
  fValue := AValue;
end;

function TInterfaceProp.Clone(const AName: string = ''): IProp;
begin
  if AName <> '' then
    Result := TInterfaceProp.Create(AName, AsInterface)
  else
    Result := TInterfaceProp.Create(Name, AsInterface);
end;

constructor TInterfaceProp.Create(const AName: string; const AValue: IUnknown);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TGuidProp }

function TGuidProp.Equals(const AProp: IProp): Boolean;
var
  mG1, mG2:TGuid;
begin
  mG1 := AProp.GetAsTGuid;
  mG2 := GetAsTGuid;
  Result := inherited Equals(AProp) and CompareMem(@mG1, @mG2, SizeOf(mG1));
end;

function TGuidProp.GetPropType: TPropType;
begin
  Result := ptGuid;
end;

function TGuidProp.GetDebugInfo: string;
begin
  Result := GUIDToString(fValue);
end;

function TGuidProp.GetAsTGuid: TGuid;
begin
  Result := fValue;
end;

procedure TGuidProp.SetAsTGuid(AValue: TGuid);
begin
  fValue := AValue;
end;

function TGuidProp.Clone(const AName: string = ''): IProp;
begin
  if AName <> '' then
    Result := TGuidProp.Create(AName, AsTGuid)
  else
    Result := TGuidProp.Create(Name, AsTGuid);
end;

constructor TGuidProp.Create(const AName: string; const AValue: TGuid);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TBooleanProp }

function TBooleanProp.Equals(const AProp: IProp): Boolean;
begin
  Result := inherited Equals(AProp) and (AProp.GetAsBoolean = GetAsBoolean);
end;

function TBooleanProp.GetPropType: TPropType;
begin
  Result := ptBool;
end;

function TBooleanProp.GetDebugInfo: string;
begin
  Result := BoolToStr(fValue);
end;

function TBooleanProp.GetAsBoolean: Boolean;
begin
  Result := fValue;
end;

procedure TBooleanProp.SetAsBoolean(AValue: Boolean);
begin
  fValue := AValue;
end;

function TBooleanProp.Clone(const AName: string = ''): IProp;
begin
  if AName <> '' then
    Result := TBooleanProp.Create(AName, AsBoolean)
  else
    Result := TBooleanProp.Create(Name, AsBoolean);
end;

constructor TBooleanProp.Create(const AName: string; const AValue: Boolean);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TIntegerProp }

function TIntegerProp.Equals(const AProp: IProp): Boolean;
begin
  Result := inherited Equals(AProp) and (AProp.GetAsInteger = GetAsInteger);
end;

function TIntegerProp.GetPropType: TPropType;
begin
  Result := ptInt;
end;

function TIntegerProp.GetDebugInfo: string;
begin
  Result := IntToStr(fValue);
end;

function TIntegerProp.GetAsInteger: integer;
begin
  Result := fValue;
end;

procedure TIntegerProp.SetAsInteger(AValue: integer);
begin
  fValue := AValue;
end;

function TIntegerProp.GetAsString: string;
begin
  Result := IntToStr(AsInteger);
end;

procedure TIntegerProp.SetAsString(AValue: string);
begin
  AsInteger := StrToInt(AValue);
end;

function TIntegerProp.Clone(const AName: string = ''): IProp;
begin
  if AName <> '' then
    Result := TIntegerProp.Create(AName, AsInteger)
  else
    Result := TIntegerProp.Create(Name, AsInteger);
end;

constructor TIntegerProp.Create(const AName: string; const AValue: Integer);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TStringProp }

function TStringProp.GetDebugInfo: string;
begin
  Result := fValue;
end;

function TStringProp.Equals(const AProp: IProp): Boolean;
begin
  Result := inherited Equals(AProp) and (AProp.GetAsString = GetAsString);
end;

function TStringProp.GetPropType: TPropType;
begin
  Result := ptStr;
end;

function TStringProp.GetAsString: string;
begin
  Result := fValue;
end;

procedure TStringProp.SetAsString(AValue: string);
begin
  fValue := AValue;
end;

function TStringProp.Clone(const AName: string = ''): IProp;
begin
  if AName <> '' then
    Result := TStringProp.Create(AName, AsString)
  else
    Result := TStringProp.Create(Name, AsString);
end;

constructor TStringProp.Create(const AName: string; const AValue: string);
begin
  inherited Create(AName);
  fValue := AValue;
end;

{ TProp }

function TProp.Equals(const AProp: IProp): Boolean;
begin
  Result := (AProp <> nil) and (AProp.PropType = GetPropType);
end;

function TProp.GetName: string;
begin
  Result := fName;
end;

function TProp.GetPropType: TPropType;
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetDebugInfo: string;
begin
  Result := '';
end;

function TProp.GetAsInteger: integer;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsInteger(AValue: integer);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsString: string;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsString(AValue: string);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsBoolean: Boolean;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsBoolean(AValue: Boolean);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsObject: TObject;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsObject(AValue: TObject);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsVariant: Variant;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsVariant(AValue: Variant);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsPtrInt: PtrInt;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsPtrInt(AValue: PtrInt);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsInterface: IUnknown;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsInterface(AValue: IUnknown);
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetAsTGuid: TGuid;
begin
  raise Exception.Create('unsupported');
end;

procedure TProp.SetAsTGuid(AValue: TGuid);
begin
  raise Exception.Create('unsupported');
end;

constructor TProp.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
end;

{ TProps }

procedure TProps.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TItems.Create;
end;

destructor TProps.Destroy;
begin
  FreeAndNil(fItems);
  inherited;
end;

class function TProps.New: IProps;
begin
  Result := Create;
end;

function TProps.IndexFit(const AIndex: integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= fItems.Count -1);
end;

procedure TProps.CheckIndexFit(const AIndex: integer);
begin
  if not IndexFit(AIndex) then
    raise Exception.CreateFmt('Index %0:d not fit, last index is %0:d', [AIndex, Count - 1]);
end;

function TProps.Count: integer;
begin
  Result := fItems.Count;
end;

function TProps.GetProp(AIndex: integer): IProp;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex]
  else
    Result := nil;
end;

function TProps.GetPropByName(const AName: string): IProp;
begin
  Result := GetProp(fItems.IndexOf(AName));
end;

function TProps.Equals(const AProps: IProps): Boolean;
var
  i, mInd: integer;
  mProp: IProp;
begin
  if AProps = nil then
    Result := False
  else  if AProps.Count <> Count then
    Result := False
  else begin
    Result := True;
    for i := 0 to Count - 1 do begin
      mProp := AProps.PropByName[Name(i)];
      if (mProp = nil) or not mProp.Equals(Prop[i]) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TProps.Clone: IProps;
var
  mProp: IProp;
  i: integer;
begin
  Result := TProps.Create;
  for i := 0 to Count - 1 do
    Result.SetProp(Name(i), Prop[i]);
end;

function TProps.Clone(ANames: array of string): IProps;
var
  mProp: IProp;
  i: integer;
  mName: string;
begin
  Result := TProps.Create;
  for mName in ANames do
  begin
    mProp := PropByName[mName];
    if mProp <> nil then
      Result.SetProp(mProp.Name, mProp);
  end;
end;

function TProps.PropType(const AName: string): TPropType;
begin
  Result := PropType(fItems.IndexOf(AName));
end;

function TProps.PropType(const AIndex: integer): TPropType;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].PropType
  else
    Result := ptUndefined;
end;

function TProps.Name(const AIndex: integer): string;
begin
  if IndexFit(AIndex) then
    Result := fItems.Keys[AIndex]
  else
    Result := cDefaultStr;
end;

function TProps.SetIt(const AName: string): IProps;
begin
  Result := SetStr(AName, '');
end;

function TProps.SetStr(const AName: string; const AValue: string): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TStringProp.Create(AName, AValue));
end;

function TProps.SetInt(const AName: string; const AValue: integer): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TIntegerProp.Create(AName, AValue));
end;

function TProps.SetBool(const AName: string; const AValue: Boolean): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TBooleanProp.Create(AName, AValue));
end;

function TProps.SetGuid(const AName: string; const AValue: TGUID): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TGuidProp.Create(AName, AValue));
end;

function TProps.SetIntf(const AName: string; const AValue: IUnknown): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TInterfaceProp.Create(AName, AValue));
end;

function TProps.SetObject(const AName: string; const AValue: TObject): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TObjectProp.Create(AName, AValue));
end;

function TProps.SetStr(const AIndex: integer; const AValue: string): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TStringProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetInt(const AIndex: integer; const AValue: integer): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TIntegerProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetBool(const AIndex: integer; const AValue: Boolean): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TBooleanProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetGuid(const AIndex: integer; const AValue: TGUID): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TGuidProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetIntf(const AIndex: integer; const AValue: IUnknown): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TInterfaceProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetObject(const AIndex: integer; const AValue: TObject): IProps;
begin
  Result := Self;
  CheckIndexFit(AIndex);
  fItems.Data[AIndex] := TObjectProp.Create(fItems.Keys[AIndex], AValue);
end;

function TProps.SetProp(const AName: string; const AProp: IProp): IProps;
begin
  if AProp <> nil then
  begin
    Result := Self;
    fItems.AddOrSetData(AName, AProp.Clone(AName));
  end;
end;

function TProps.SetProp(const AProp: IProp): IProps;
begin
  if AProp <> nil then
  begin
    Result := Self;
    fItems.AddOrSetData(AProp.Name, AProp.Clone);
  end;
end;

function TProps.AsStr(const AName: string): string;
begin
  Result := AsStr(fItems.IndexOf(AName));
end;

function TProps.AsInt(const AName: string): integer;
begin
  Result := AsInt(fItems.IndexOf(AName));
end;

function TProps.AsBool(const AName: string): Boolean;
begin
  Result := AsBool(fItems.IndexOf(AName));
end;

function TProps.AsGuid(const AName: string): TGUID;
begin
  Result := AsGuid(fItems.IndexOf(AName));
end;

function TProps.AsIntf(const AName: string): IUnknown;
begin
  Result := AsIntf(fItems.IndexOf(AName));
end;

function TProps.AsObject(const AName: string): TObject;
begin
  Result := AsObject(fItems.IndexOf(AName));
end;

function TProps.AsStr(const AIndex: integer): string;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsString
  else
    Result := cDefaultStr;
end;

function TProps.AsInt(const AIndex: integer): integer;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsInteger
  else
    Result := cDefaultInt;
end;

function TProps.AsBool(const AIndex: integer): Boolean;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsBoolean
  else
    Result := cDefaultBool;
end;

function TProps.AsGuid(const AIndex: integer): TGUID;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsTGuid
  else
    Result := cDefaultGuid;
end;

function TProps.AsIntf(const AIndex: integer): IUnknown;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsInterface
  else
    Result := nil;
end;

function TProps.AsObject(const AIndex: integer): TObject;
begin
  if IndexFit(AIndex) then
    Result := fItems.Data[AIndex].AsObject
  else
    Result := nil;
end;

function TProps.Diff(const AProps: IProps; AMode: TPropDiffMode): IProps;
var
  i: integer;
  mMyProp, mTheirProp: IProp;
begin
  Result := New;
  for i := 0 to Count - 1 do begin
    mMyProp := Prop[i];
    mTheirProp := AProps.PropByName[Name(i)];
    if AMode = pdmAll then
      Result.SetProp(Name(i), mMyProp)
    else if (mTheirProp = nil) then
      Result.SetProp(Name(i), mMyProp)
    else if (AMode = pdmDifferent) and mTheirProp.Equals(mMyProp) then
      Result.SetProp(Name(i), mMyProp);
  end;
  for i := 0 to AProps.Count - 1 do begin
    mMyProp := PropByName[AProps.Name(i)];
    if mMyProp = nil then begin
      case AProps.Prop[i].PropType of
        ptUndefined: raise Exception.Create('cannot process diff for undefined type');
        ptInt: Result.SetInt(AProps.Name(i), 0);
        ptStr: Result.SetStr(AProps.Name(i), '');
        ptBool: Result.SetBool(AProps.Name(i), False);
        ptGuid: Result.SetGuid(AProps.Name(i), GUID_NULL);
        ptInterface: Result.SetIntf(AProps.Name(i), nil);
        ptObject: Result.SetObject(AProps.Name(i), nil);
      end;
    end;
  end;
end;

function TProps.Info: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    Result := Result + Prop[i].Name + '=' + Prop[i].DebugInfo + ', ';
  end;
end;

function TProps.Find(const AID: string): IProp;
begin
  Result := PropByName[AID];
end;

end.

