unit trl_ipersist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  Classes, SysUtils, trl_irttibroker, trl_usystem, fgl;

const
  cMemoStringType = 'TMemoString';
  cIDStringType = 'TIDString';

type
  TMemoString = type string;
  TIDString = type string;

  { IPersistStoreDevice }

  IPersistStoreDevice = interface
  ['{32674407-3D99-4BF9-8BBE-99DABA186655}']
    function Select2(const AClass: string): IRBDataEnumerable;
    procedure Save2(const AData: IRBData);
    procedure Delete2(const AData: IRBData);
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    procedure Flush;
    function IsOpened: Boolean;
  end;

  { IPersistFactory }

  IPersistFactory = interface
  ['{66F2248D-87D0-49D8-ACBF-DA19CC862A11}']
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;

implementation

end.

