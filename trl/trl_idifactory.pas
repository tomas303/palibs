unit trl_idifactory;

{$mode delphi}{$H+}

interface

type
  IDIFactory = interface
  ['{80F7C3AD-0120-4792-9404-F48B6695A14D}']
    function Locate(AClass: TClass; const AID: string = ''): pointer; overload;
    function Locate(AInterface: TGUID; const AID: string = ''): pointer; overload;
    function Locate(const AClass: string; const AID: string = ''): pointer; overload;
  end;

implementation

end.

