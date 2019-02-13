unit rea_ubrace;

{$mode objfpc}{$H+}

interface

uses
  rea_ibrace, trl_itree, trl_imetaelement, trl_ilog;

type

  { TBrace }

  TBrace = class(TInterfacedObject, IBrace, INode)
  protected
    // IBrace
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    fLog: ILog;
    fNode: INode;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
  end;

implementation

{ TBrace }

procedure TBrace.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TBrace.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TBrace.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TBrace.Delete(const AIndex: integer);
begin
  Node.Delete(AIndex);
end;

function TBrace.Count: integer;
begin
  Result := node.Count;
end;

function TBrace.GetChild(const AIndex: integer): INode;
begin
  Result := Node.GetChild(AIndex);
end;

function TBrace.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

end.

