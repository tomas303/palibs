unit trl_itree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  INode = interface;

  INodeEnumerator = interface
  ['{0C7E3A23-2552-4D0A-82D1-51496D5BFF96}']
    function MoveNext: Boolean;
    function GetCurrent: INode;
    property Current: INode read GetCurrent;
  end;

  INode = interface
  ['{74337291-3342-4AA5-A3D1-90F430315C23}']
    //function GetParent: INode;
    //property Parent: INode read GetParent;
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function GetEnumerator: INodeEnumerator;
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    property Child[const AIndex: integer]: INode read GetChild; default;
  end;

implementation

end.

