unit trl_umetaelement;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, trl_itree, trl_iprops, SysUtils, trl_ilog;

type

  { TMetaElementEnumerator }

  TMetaElementEnumerator = class(TInterfacedObject, IMetaElementEnumerator)
  protected
    fNodeEnumerator: INodeEnumerator;
  protected
    // IMetaElementEnumerator
    function MoveNext: Boolean;
    function GetCurrent: IMetaElement;
    property Current: IMetaElement read GetCurrent;
  public
    constructor Create(const ANodeEnumerator: INodeEnumerator);
  end;

  { TMetaElement }

  TMetaElement = class(TInterfacedObject, IMetaElement, INode, ILogSupport)
  protected
    fTypeGuid: string;
    fTypeID: string;
    fProps: IProps;
    fIsMetaElementProvider: Boolean;
  protected
    // IMetaElement
    function Guid: TGuid;
    function GetTypeGuid: string;
    function GetTypeID: string;
    function GetProps: IProps;
    function GetMetaElementEnumerator: IMetaElementEnumerator;
    function IMetaElement.GetEnumerator = GetMetaElementEnumerator;
    function GetIsMetaElementProvider: Boolean;
  protected
    // ILogSupport
    function LogInfo: string;
  published
    property TypeGuid: string read GetTypeGuid write fTypeGuid;
    property TypeID: string read GetTypeID write fTypeID;
    property Props: IProps read GetProps write fProps;
    property IsMetaElementProvider: Boolean read GetIsMetaElementProvider write fIsMetaElementProvider;
  protected
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
    fNode: INode;
  published
    property Node: INode read fNode write fNode;
  end;

implementation

{ TMetaElementEnumerator }

function TMetaElementEnumerator.MoveNext: Boolean;
begin
  Result := fNodeEnumerator.MoveNext;
end;

function TMetaElementEnumerator.GetCurrent: IMetaElement;
begin
  Result := fNodeEnumerator.Current as IMetaElement;
end;

constructor TMetaElementEnumerator.Create(const ANodeEnumerator: INodeEnumerator
  );
begin
  fNodeEnumerator := ANodeEnumerator;
end;

{ TMetaElement }

function TMetaElement.Guid: TGuid;
begin
  Result := StringToGUID(fTypeGuid);
end;

function TMetaElement.GetTypeGuid: string;
begin
  Result := fTypeGuid;
end;

function TMetaElement.GetTypeID: string;
begin
  Result := fTypeID;
end;

function TMetaElement.GetProps: IProps;
begin
  Result := fProps;
end;

function TMetaElement.LogInfo: string;
var
  mChild: INode;
  mChildEl: IMetaElement;
begin
  Result := TypeGuid + '(' + TypeID + ')';
  for mChild in Node do
  begin
    mChildEl := mChild as IMetaElement;
    Result := Result + LineEnding + '->' +  (mChildEl as ILogSupport).LogInfo;
  end;
end;

function TMetaElement.GetMetaElementEnumerator: IMetaElementEnumerator;
begin
  Result := TMetaElementEnumerator.Create(GetNodeEnumerator);
end;

function TMetaElement.GetIsMetaElementProvider: Boolean;
begin
  Result := fIsMetaElementProvider;
end;

procedure TMetaElement.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TMetaElement.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TMetaElement.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TMetaElement.Delete(const AIndex: integer);
begin
  Node.Delete(AIndex);
end;

function TMetaElement.Count: integer;
begin
  Result := Node.Count;
end;

function TMetaElement.GetChild(const AIndex: integer): INode;
begin
  Result := Node[AIndex];
end;

function TMetaElement.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

end.

