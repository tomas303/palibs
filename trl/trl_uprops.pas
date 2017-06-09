unit trl_uprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_iprops, fgl;

type

  { TProp }

  TProp = class(TInterfacedObject, IProp)
  protected
    //IProp
    function Equals(const AProp: IProp): Boolean; virtual;
    function Clone: IProp; virtual; abstract;
    function GetName: string;
    function GetPropType: TPropType; virtual;
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
  end;

  { TStringProp }

  TStringProp = class(TProp)
  protected
    fValue: String;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone: IProp; override;
    function GetPropType: TPropType; override;
    function GetAsString: string; override;
    procedure SetAsString(AValue: string); override;
  public
    constructor Create(const AValue: string);
  end;

  { TIntegerProp }

  TIntegerProp = class(TProp)
  protected
    fValue: Integer;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone: IProp; override;
    function GetPropType: TPropType; override;
    function GetAsInteger: integer; override;
    procedure SetAsInteger(AValue: integer); override;
  public
    constructor Create(const AValue: Integer);
  end;

  { TBooleanProp }

  TBooleanProp = class(TProp)
  protected
    fValue: Boolean;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone: IProp; override;
    function GetPropType: TPropType; override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(const AValue: Boolean);
  end;

  { TGuidProp }

  TGuidProp = class(TProp)
  protected
    fValue: TGuid;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone: IProp; override;
    function GetPropType: TPropType; override;
    function GetAsTGuid: TGuid; override;
    procedure SetAsTGuid(AValue: TGuid); override;
  public
    constructor Create(const AValue: TGuid);
  end;

  { TInterfaceProp }

  TInterfaceProp = class(TProp)
  protected
    fValue: IUnknown;
  protected
    function Equals(const AProp: IProp): Boolean; override;
    function Clone: IProp; override;
    function GetPropType: TPropType; override;
    function GetAsInterface: IUnknown; override;
    procedure SetAsInterface(AValue: IUnknown); override;
  public
    constructor Create(const AValue: IUnknown);
  end;


  { TProps }

  TProps = class(TInterfacedObject, IProps)
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
      procedure CopyData(Src, Dest: Pointer); override;
    end;

  protected
    fItems: TItems;
    function IndexFit(const AIndex: integer): Boolean;
  protected
    //IProps
    function Count: integer;
    function GetProp(AIndex: integer): IProp;
    property Prop[AIndex: integer]: IProp read GetProp; default;
    function GetPropByName(const AName: string): IProp;
    property PropByName[const AName: string]: IProp read GetPropByName;
    function Equals(const AProps: IProps): Boolean;
    function PropType(const AName: string): TPropType;
    function PropType(const AIndex: integer): TPropType;
    function Name(const AIndex: integer): string;
    function SetIt(const AName: string): IProps;
    function SetStr(const AName: string; const AValue: string): IProps;
    function SetInt(const AName: string; const AValue: integer): IProps;
    function SetBool(const AName: string; const AValue: Boolean): IProps;
    function SetGuid(const AName: string; const AValue: TGUID): IProps;
    function SetIntf(const AName: string; const AValue: IUnknown): IProps;
    function SetProp(const AName: string; const AProp: IProp): IProps;
    function AsStr(const AName: string): string;
    function AsInt(const AName: string): integer;
    function AsBool(const AName: string): Boolean;
    function AsGuid(const AName: string): TGUID;
    function AsIntf(const AName: string): IUnknown;
    function AsStr(const AIndex: integer): string;
    function AsInt(const AIndex: integer): integer;
    function AsBool(const AIndex: integer): Boolean;
    function AsGuid(const AIndex: integer): TGUID;
    function AsIntf(const AIndex: integer): IUnknown;
    function Diff(const AProps: IProps): IProps;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IProps;
  end;

implementation

{ TProps.TItems }

procedure TProps.TItems.Deref(Item: Pointer);
begin
  // in TFPGMapInterfacedObjectData.CopyData is error
  // TData(Dest^) := TData(Src^); will also ref. data
  // based on similar list it should be pointer(Dest^) := pointer(Src^);
  inherited Deref(Item);
end;

procedure TProps.TItems.CopyData(Src, Dest: Pointer);
begin
  inherited CopyData(Src, Dest);
  //if Assigned(Pointer(Dest^)) then
  //  IProp(Dest^)._Release;
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

function TInterfaceProp.GetAsInterface: IUnknown;
begin
  Result := fValue;
end;

procedure TInterfaceProp.SetAsInterface(AValue: IUnknown);
begin
  fValue := AValue;
end;

function TInterfaceProp.Clone: IProp;
begin
  Result := TInterfaceProp.Create(AsInterface);
end;

constructor TInterfaceProp.Create(const AValue: IUnknown);
begin
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

function TGuidProp.GetAsTGuid: TGuid;
begin
  Result := fValue;
end;

procedure TGuidProp.SetAsTGuid(AValue: TGuid);
begin
  fValue := AValue;
end;

function TGuidProp.Clone: IProp;
begin
  Result := TGuidProp.Create(AsTGuid);
end;

constructor TGuidProp.Create(const AValue: TGuid);
begin
  inherited Create;
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

function TBooleanProp.GetAsBoolean: Boolean;
begin
  Result := fValue;
end;

procedure TBooleanProp.SetAsBoolean(AValue: Boolean);
begin
  fValue := AValue;
end;

function TBooleanProp.Clone: IProp;
begin
  Result := TBooleanProp.Create(AsBoolean);
end;

constructor TBooleanProp.Create(const AValue: Boolean);
begin
  inherited Create;
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

function TIntegerProp.GetAsInteger: integer;
begin
  Result := fValue;
end;

procedure TIntegerProp.SetAsInteger(AValue: integer);
begin
  fValue := AValue;
end;

function TIntegerProp.Clone: IProp;
begin
  Result := TIntegerProp.Create(AsInteger);
end;

constructor TIntegerProp.Create(const AValue: Integer);
begin
  inherited Create;
  fValue := AValue;
end;

{ TStringProp }

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

function TStringProp.Clone: IProp;
begin
  Result := TStringProp.Create(AsString);
end;

constructor TStringProp.Create(const AValue: string);
begin
  inherited Create;
  fValue := AValue;
end;

{ TProp }

function TProp.Equals(const AProp: IProp): Boolean;
begin
  Result := (AProp <> nil) and (AProp.PropType = GetPropType);
end;

function TProp.GetName: string;
begin
  raise Exception.Create('unsupported');
end;

function TProp.GetPropType: TPropType;
begin
  raise Exception.Create('unsupported');
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

{ TProps }

constructor TProps.Create;
begin
  inherited;
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
  fItems.AddOrSetData(AName, TStringProp.Create(AValue));
end;

function TProps.SetInt(const AName: string; const AValue: integer): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TIntegerProp.Create(AValue));
end;

function TProps.SetBool(const AName: string; const AValue: Boolean): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TBooleanProp.Create(AValue));
end;

function TProps.SetGuid(const AName: string; const AValue: TGUID): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TGuidProp.Create(AValue));
end;

function TProps.SetIntf(const AName: string; const AValue: IUnknown): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, TInterfaceProp.Create(AValue));
end;

function TProps.SetProp(const AName: string; const AProp: IProp): IProps;
begin
  Result := Self;
  fItems.AddOrSetData(AName, AProp.Clone);
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

function TProps.Diff(const AProps: IProps): IProps;
var
  i: integer;
  mMyProp, mTheirProp: IProp;
begin
  Result := New;
  for i := 0 to Count - 1 do begin
    mMyProp := Prop[i];
    mTheirProp := AProps.PropByName[Name(i)];
    if (mTheirProp = nil) or not mTheirProp.Equals(mMyProp) then
      Result.SetProp(Name(i), mMyProp);
  end;
end;

end.

