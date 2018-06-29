unit rdx_urttistate;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, trl_iprops, trl_uprops, trl_igenericaccess, trl_irttibroker,
  trl_urttibroker, TypInfo, sysutils;

type
  TRttiState = class(TInterfacedObject, IFluxState, IPropFinder, IGenericAccess)
  protected
    fRBData: IRBData;
    function DataItem2Prop(const AItem: IRBDataItem): IProp;
    function TypeKind2GenericAccessType(const ATypeKind:TTypeKind): TGenericAccessType;
  protected
    // IFluxState
    function GetID: string;
  protected
    // IPropFinder
    function Find(const AID: string): IProp;
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
  public
    procedure AfterConstruction; override;
    class function CollectPaths(const APrefix: string): TStatePaths;
  protected
    fID: string;
  published
    property ID: string read GetID write fID;
  end;

implementation

{ TRttiState }

function TRttiState.DataItem2Prop(const AItem: IRBDataItem): IProp;
begin
  case AItem.TypeKind of
    tkAString:
      Result := TStringProp.Create(AItem.Name, AItem.AsString);
    tkInteger:
      Result := TIntegerProp.Create(AItem.Name, AItem.AsInteger);
    tkBool:
      Result := TBooleanProp.Create(AItem.Name, AItem.AsBoolean);
    //tkClass:
    //  Result := not have now
    tkEnumeration:
      Result := TIntegerProp.Create(AItem.Name, AItem.AsInteger);
    //tkVariant:
    //  Result := not have now
    //tkClassRef:
    //  Result := not have now
    tkInterface:
      Result := TInterfaceProp.Create(AItem.Name, AItem.AsInterface);
    else
      raise Exception.Create('unsupported');
  end;
end;

function TRttiState.TypeKind2GenericAccessType(const ATypeKind: TTypeKind
  ): TGenericAccessType;
begin
  case ATypeKind of
    tkAString:
      Result := TGenericAccessType.gatStr;
    tkInteger:
      Result := TGenericAccessType.gatInt;
    tkBool:
      Result := TGenericAccessType.gatBool;
    //tkClass:
    //  Result := not have now
    tkEnumeration:
      Result := TGenericAccessType.gatInt;
    //tkVariant:
    //  Result := not have now
    //tkClassRef:
    //  Result := not have now
    tkInterface:
      Result := TGenericAccessType.gatInterface;
    else
      raise Exception.Create('unsupported');
  end;
end;

function TRttiState.GetID: string;
begin
  Result := fID;
end;

function TRttiState.Find(const AID: string): IProp;
var
  mItem: IRBDataItem;
begin
  // todo cache
  mItem := fRBData.ItemByName[AID];
  if mItem <> nil then
    //Result := TRBDataProp.New(mItem)
    Result := DataItem2Prop(mItem)
  else
    Result := nil;
end;

function TRttiState.AsStr(const AName: string): string;
begin
  Result := fRBData.ItemByName[AName].AsString;
end;

function TRttiState.AsInt(const AName: string): integer;
begin
  Result := fRBData.ItemByName[AName].AsInteger;
end;

function TRttiState.AsBool(const AName: string): Boolean;
begin
  Result := fRBData.ItemByName[AName].AsBoolean;
end;

function TRttiState.AsGuid(const AName: string): TGUID;
begin
  raise Exception.Create('unsupported');
end;

function TRttiState.AsIntf(const AName: string): IUnknown;
begin
  Result := fRBData.ItemByName[AName].AsInterface;
end;

function TRttiState.AsStr(const AIndex: integer): string;
begin
  Result := fRBData.Items[AIndex].AsString;
end;

function TRttiState.AsInt(const AIndex: integer): integer;
begin
  Result := fRBData.Items[AIndex].AsInteger;
end;

function TRttiState.AsBool(const AIndex: integer): Boolean;
begin
  Result := fRBData.Items[AIndex].AsBoolean;
end;

function TRttiState.AsGuid(const AIndex: integer): TGUID;
begin
  raise Exception.Create('unsupported');
end;

function TRttiState.AsIntf(const AIndex: integer): IUnknown;
begin
  Result := fRBData.Items[AIndex].AsInterface;
end;

function TRttiState.SetStr(const AName: string; const AValue: string
  ): IGenericAccess;
begin
  fRBData.ItemByName[AName].AsString := AValue;
end;

function TRttiState.SetInt(const AName: string; const AValue: integer
  ): IGenericAccess;
begin
  fRBData.ItemByName[AName].AsInteger := AValue;
end;

function TRttiState.SetBool(const AName: string; const AValue: Boolean
  ): IGenericAccess;
begin
  fRBData.ItemByName[AName].AsBoolean := AValue;
end;

function TRttiState.SetGuid(const AName: string; const AValue: TGUID
  ): IGenericAccess;
begin
  raise Exception.Create('unsupported');
end;

function TRttiState.SetIntf(const AName: string; const AValue: IUnknown
  ): IGenericAccess;
begin
  fRBData.ItemByName[AName].AsInterface := AValue;
end;

function TRttiState.SetStr(const AIndex: integer; const AValue: string
  ): IGenericAccess;
begin
  fRBData.Items[AIndex].AsString := AValue;
end;

function TRttiState.SetInt(const AIndex: integer; const AValue: integer
  ): IGenericAccess;
begin
  fRBData.Items[AIndex].AsInteger := AValue;
end;

function TRttiState.SetBool(const AIndex: integer; const AValue: Boolean
  ): IGenericAccess;
begin
  fRBData.Items[AIndex].AsBoolean := AValue;
end;

function TRttiState.SetGuid(const AIndex: integer; const AValue: TGUID
  ): IGenericAccess;
begin
  raise Exception.Create('unsupported');
end;

function TRttiState.SetIntf(const AIndex: integer; const AValue: IUnknown
  ): IGenericAccess;
begin
  fRBData.Items[AIndex].AsInterface := AValue;
end;

function TRttiState.Count: integer;
begin
  Result := fRBData.Count;
end;

function TRttiState.GAType(const AName: string): TGenericAccessType;
begin
  Result := TypeKind2GenericAccessType(fRBData.ItemByName[AName].TypeKind);
end;

function TRttiState.GAType(const AIndex: integer): TGenericAccessType;
begin
  Result := TypeKind2GenericAccessType(fRBData.Items[AIndex].TypeKind);
end;

function TRttiState.Name(const AIndex: integer): string;
begin
  Result := fRBData.Items[AIndex].Name;
end;

procedure TRttiState.AfterConstruction;
begin
  inherited AfterConstruction;
  fRBData := TRBData.Create(Self, True);
end;

class function TRttiState.CollectPaths(const APrefix: string): TStatePaths;
var
  mRBData: IRBData;
  i: integer;
begin
  mRBData := TRBData.Create(Self);
  SetLength(Result, mRBData.Count);
  for i := 0 to mRBData.Count - 1 do
  begin
    Result[i] := APrefix + mRBData.Items[i].Name;
  end;
end;

end.

