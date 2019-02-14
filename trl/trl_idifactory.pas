unit trl_idifactory;

{$mode delphi}{$H+}

interface

uses
  trl_iprops;

type
  IDIFactory = interface
  ['{80F7C3AD-0120-4792-9404-F48B6695A14D}']
    function Locate(AClass: TClass; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(AInterface: TGUID; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(const AClass: string; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function CanLocateAs(AClass: TClass; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(AClass: TClass; const AID: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AInterface: TGUID; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AInterface: TGUID; const AID: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AClass: string; const AAsInterface: TGUID): Boolean; overload;
    function CanLocateAs(const AClass: string; const AID: string; const AAsInterface: TGUID): Boolean; overload;
  end;

implementation

end.

