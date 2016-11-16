unit trl_isysutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ISysUtils = interface
  ['{82076917-5987-4BB0-9431-4BBF7D56A65F}']
    function NewGID: string;
    function NewGuid: TGuid;
  end;

implementation

end.

