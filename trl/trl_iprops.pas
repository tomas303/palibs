unit trl_iprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  //IProp = interface;
  //// children???
  //IPropArray = array of IProp;

  TPropType = (ptUndefined, ptInt, ptStr, ptBool, ptGuid, ptInterface);

  IProp = interface
  ['{B36E5E43-AFB2-4A28-8E09-E7BC682BE1F0}']
    function Equals(const AProp: IProp): Boolean;
    function Clone: IProp;
    function GetName: string;
    function GetPropType: TPropType; //??? probably own type and if necessary map it to typekind
    function GetAsInteger: integer;
    procedure SetAsInteger(AValue: integer);
    function GetAsString: string;
    procedure SetAsString(AValue: string);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);
    function GetAsObject: TObject;
    procedure SetAsObject(AValue: TObject);
    function GetAsVariant: Variant;
    procedure SetAsVariant(AValue: Variant);
    function GetAsPtrInt: PtrInt;
    procedure SetAsPtrInt(AValue: PtrInt);
    function GetAsInterface: IUnknown;
    procedure SetAsInterface(AValue: IUnknown);
    function GetAsTGuid: TGuid;
    procedure SetAsTGuid(AValue: TGuid);
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

  // maybe good make it immutable ... like Lock without unlock, or readonly
  // SetIt.SetIt.SetIt.ReadOnly;
  // better to use Writeable field(strict private) so by default is false
  // and is set only in some builder methods - then and back

  { IProps }

  IProps = interface
  ['{D3841623-443A-4177-8FED-DDA65A75CD43}']
    function Count: integer;
    function GetProp(AIndex: integer): IProp;
    property Prop[AIndex: integer]: IProp read GetProp; default;
    function GetPropByName(const AName: string): IProp;
    property PropByName[const AName: string]: IProp read GetPropByName;
    function Equals(const AProps: IProps): Boolean;
    function Clone: IProps;
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
  end;

implementation

end.

