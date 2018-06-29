unit rdx_ustate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, flu_iflux, trl_iprops, trl_idifactory, trl_igenericaccess;

type

  { TRdxState }

  {
   abych mohl z kodu odkazovat na konkretni cast, musim umet state podle
  toho pripravit
    aktualne by asi bylo nejliepsi hom it

  }

  TRdxState = class(TInterfacedObject, IFluxState, IPropFinder, IGenericAccess)
  protected
    // IPropFinder
    function Find(const AID: string): IProp;
  protected
    // IFluxState
    function GetID: string;
    //function Props(const AID: string): IProps;
  protected
    // IGenericAccess
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
    function SetStr(const AName: string; const AValue: string): IGenericAccess;
    function SetInt(const AName: string; const AValue: integer): IGenericAccess;
    function SetBool(const AName: string; const AValue: Boolean): IGenericAccess;
    function SetGuid(const AName: string; const AValue: TGUID): IGenericAccess;
    function SetIntf(const AName: string; const AValue: IUnknown): IGenericAccess;
    function SetStr(const AIndex: integer; const AValue: string): IGenericAccess;
    function SetInt(const AIndex: integer; const AValue: integer): IGenericAccess;
    function SetBool(const AIndex: integer; const AValue: Boolean): IGenericAccess;
    function SetGuid(const AIndex: integer; const AValue: TGUID): IGenericAccess;
    function SetIntf(const AIndex: integer; const AValue: IUnknown): IGenericAccess;
    function Count: integer;
    function GAType(const AName: string): TGenericAccessType;
    function GAType(const AIndex: integer): TGenericAccessType;
    function Name(const AIndex: integer): string;
  protected
    fID: string;
    //fFactory: IDIFactory;
    fData: IProps;
    procedure SetAddState(const AValue: IFluxState);
    procedure SetAddBool(const AName: String);
    procedure SetAddNumber(const AName: String);
    procedure SetAddText(const AName: String);
  published
    property ID: string read GetID write fID;
    //property Factory: IDIFactory read fFactory write fFactory;
    property Data: IProps read fData write fData;
    property AddState: IFluxState write SetAddState;
    property AddText: String write SetAddText;
    property AddNumber: String write SetAddNumber;
    property AddBool: String write SetAddBool;
  end;

implementation

{ TRdxState }

procedure TRdxState.SetAddState(const AValue: IFluxState);
begin
  Data.SetIntf(AValue.GetID, AValue);
end;

procedure TRdxState.SetAddBool(const AName: String);
begin
  Data.SetBool(AName, False);
end;

procedure TRdxState.SetAddNumber(const AName: String);
begin
  Data.SetInt(AName, 0);
end;

procedure TRdxState.SetAddText(const AName: String);
begin
  Data.SetStr(AName, '');
end;

function TRdxState.Find(const AID: string): IProp;
begin
  Result := (Data as IPropFinder).Find(AID);
end;

function TRdxState.GetID: string;
begin
  Result := ID;
end;

function TRdxState.AsStr(const AName: string): string;
begin
  Result := Data.AsStr(AName);
end;

function TRdxState.AsInt(const AName: string): integer;
begin
  Result := Data.AsInt(AName);
end;

function TRdxState.AsBool(const AName: string): Boolean;
begin
  Result := Data.AsBool(AName);
end;

function TRdxState.AsGuid(const AName: string): TGUID;
begin
  Result := Data.AsGuid(AName);
end;

function TRdxState.AsIntf(const AName: string): IUnknown;
begin
  Result := Data.AsIntf(AName);
end;

function TRdxState.AsStr(const AIndex: integer): string;
begin
  Result := Data.AsStr(AIndex);
end;

function TRdxState.AsInt(const AIndex: integer): integer;
begin
  Result := Data.AsInt(AIndex);
end;

function TRdxState.AsBool(const AIndex: integer): Boolean;
begin
  Result := Data.AsBool(AIndex);
end;

function TRdxState.AsGuid(const AIndex: integer): TGUID;
begin
  Result := Data.AsGuid(AIndex);
end;

function TRdxState.AsIntf(const AIndex: integer): IUnknown;
begin
  Result := Data.AsIntf(AIndex);
end;

function TRdxState.SetStr(const AName: string; const AValue: string
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetStr(AName, AValue);
end;

function TRdxState.SetInt(const AName: string; const AValue: integer
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetInt(AName, AValue);
end;

function TRdxState.SetBool(const AName: string; const AValue: Boolean
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetBool(AName, AValue);
end;

function TRdxState.SetGuid(const AName: string; const AValue: TGUID
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetGuid(AName, AValue);
end;

function TRdxState.SetIntf(const AName: string; const AValue: IUnknown
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetIntf(AName, AValue);
end;

function TRdxState.SetStr(const AIndex: integer; const AValue: string
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetStr(AIndex, AValue);
end;

function TRdxState.SetInt(const AIndex: integer; const AValue: integer
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetInt(AIndex, AValue);
end;

function TRdxState.SetBool(const AIndex: integer; const AValue: Boolean
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetBool(AIndex, AValue);
end;

function TRdxState.SetGuid(const AIndex: integer; const AValue: TGUID
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetGuid(AIndex, AValue);
end;

function TRdxState.SetIntf(const AIndex: integer; const AValue: IUnknown
  ): IGenericAccess;
begin
  Result := Self as IGenericAccess;
  Data.SetIntf(AIndex, AValue);
end;

function TRdxState.Count: integer;
begin
  Result := Data.Count;
end;

function TRdxState.GAType(const AName: string): TGenericAccessType;
begin
  case Data.PropType(AName) of
    ptInt: Result := TGenericAccessType.gatInt;
    ptStr: Result := TGenericAccessType.gatStr;
    ptBool: Result := TGenericAccessType.gatBool;
    ptGuid: Result := TGenericAccessType.gatGuid;
    ptInterface: Result := TGenericAccessType.gatInterface;
    else
      Result := TGenericAccessType.gatUndefined;
  end;
end;

function TRdxState.GAType(const AIndex: integer): TGenericAccessType;
begin
  Result := GAType(Name(AIndex));
end;

function TRdxState.Name(const AIndex: integer): string;
begin
  Result := Data.Name(AIndex);
end;

//function TRdxState.Props(const AID: string): IProps;
//var
//  mProp: IProp;
//begin
//  mProp := Data.PropByName[AID];
//  if mProp = nil then
//  begin
//    Result := IProps(Factory.Locate(IProps));
//    Data.SetIntf(AID, Result);
//  end
//  else
//  begin
//    Result := mProp.AsInterface as IProps;
//  end;
//end;

end.

