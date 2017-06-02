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
  end;

  { TStringProp }

  TStringProp = class(TProp)
  protected
    fValue: String;
  protected
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
    TItems = specialize TFPGMapInterfacedObjectData<string, IProp>;
  protected
    fItems: TItems;
    function IndexFit(const AIndex: integer): Boolean;
  protected
    //IProps
    function Count: integer;
    function PropType(const AName: string): TPropType;
    function PropType(const AIndex: integer): TPropType;
    function Name(const AIndex: integer): string;
    function SetIt(const AName: string): IProps;
    function SetStr(const AName: string; const AValue: string): IProps;
    function SetInt(const AName: string; const AValue: integer): IProps;
    function SetBool(const AName: string; const AValue: Boolean): IProps;
    function SetGuid(const AName: string; const AValue: TGUID): IProps;
    function SetIntf(const AName: string; const AValue: IUnknown): IProps;
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
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IProps;
  end;

implementation

{ TInterfaceProp }

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

constructor TInterfaceProp.Create(const AValue: IUnknown);
begin
  fValue := AValue;
end;

{ TGuidProp }

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

constructor TGuidProp.Create(const AValue: TGuid);
begin
  inherited Create;
  fValue := AValue;
end;

{ TBooleanProp }

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

constructor TBooleanProp.Create(const AValue: Boolean);
begin
  inherited Create;
  fValue := AValue;
end;

{ TIntegerProp }

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

constructor TIntegerProp.Create(const AValue: Integer);
begin
  inherited Create;
  fValue := AValue;
end;

{ TStringProp }

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

constructor TStringProp.Create(const AValue: string);
begin
  inherited Create;
  fValue := AValue;
end;

{ TProp }

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

end.

