unit trl_iminipersist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}


interface

uses
  Classes, SysUtils, trl_irttibroker, trl_pubsub, trl_ipersist;

type

  { PersistIDAttribute }

  PersistIDAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  { PersistAUTOAttribute }

  PersistAUTOAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TPersistPredicate = reference to function(const x: IRBData): Boolean;

  TPersistAction = (paNew, paChange, paDelete);

  { TPersistInfo }

  TPersistInfo = record
  private
    fData: IRBData;
    fAction: TPersistAction;
  public
    constructor Create(const AData: IRBData; AAction: TPersistAction);
    class operator equal(a,b: TPersistInfo): Boolean;
    property Data: IRBData read fData;
    property Action: TPersistAction read fAction;
  end;

  IPSPersistChannel = IPubSubDataChannel<TPersistInfo>;

  { IMiniDataList }

  IMiniList = interface
  ['{EEA85C61-F9C5-493E-8034-C7983355D906}']
    function GetCount: Integer;
    function GetField(AIndex: Integer; const AName: String): String;
    procedure SetField(AIndex: Integer; const AName: String; AValue: String);
    function Insert(APos: Integer): Integer; overload;
    function Append: Integer; overload;
    procedure Delete(APos: Integer);
    property Count: Integer read GetCount;
    property Field[AIndex: Integer; const AName: String]: String read GetField write SetField;
    function GetEnumerator: IRBDataEnumerator;
  end;

  IMiniList<T: TObject> = interface(IMiniList)
  ['{2F401415-D81E-41D0-83AB-063BFC4E5261}']
  end;


  IMiniDataList<T: TObject> = interface
  ['{042B2A5C-4450-4A70-A1E6-7A3020F79B0E}']
    function NewList: IMiniList<T>; overload;
    function NewList(const APredicate: TPersistPredicate): IMiniList<T>; overload;
    function GetEnumerator: IRBDataEnumerator;
    procedure Load;
    procedure Save;
  end;

implementation

{ PersistAUTOAttribute }

constructor PersistAUTOAttribute.Create;
begin

end;

{ PersistIDAttribute }

constructor PersistIDAttribute.Create;
begin

end;

{ TPersistInfo }

constructor TPersistInfo.Create(const AData: IRBData; AAction: TPersistAction);
begin
  fData := AData;
  fAction := AAction;
end;

class operator TPersistInfo.equal(a, b: TPersistInfo): Boolean;
begin
  Result := (a.fData = b.fData) and (a.fAction = b.fAction);
end;

end.

