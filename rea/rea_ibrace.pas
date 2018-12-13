unit rea_ibrace;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, trl_itree;

type

  { IBrace }

  IBrace = interface
  ['{278C5A0E-8BE1-48E9-BB20-9E2759B17799}']
    function GetGist: INode;
    function GetOrigin: IMetaElement;
    procedure SetGist(AValue: INode);
    procedure SetOrigin(AValue: IMetaElement);
    property Origin: IMetaElement read GetOrigin write SetOrigin;
    property Gist: INode read GetGist write SetGist;
  end;

implementation

end.

