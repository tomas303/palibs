unit trl_icryptic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { ICryptic }

  ICryptic = interface
  ['{DC755129-2DA8-482D-A359-D9DB43D54DA2}']
    function Decode(const AData: String): String;
    function Encode(const AData: String): String;
    procedure Decode(AInStream, AOutStream: TStream);
    procedure Encode(AInStream, AOutStream: TStream);
    function GetKey: string;
    procedure SetKey(AValue: string);
    property Key: string read GetKey write SetKey;
  end;

implementation

end.

