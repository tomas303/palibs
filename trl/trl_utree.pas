unit trl_utree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_itree, fgl;

type

  { INodeCursor }

  { IBlackBox }

  // as memento - owner will hold, but not manipulate directly
  IBlackBox = interface
  ['{C2FE6101-DC3B-4597-8948-DDCCDBF2F9B1}']
  end;

  INodeCursor = interface
  ['{210BC050-0F9E-407F-BF5D-BC9CAB1BE4EC}']
    function GetIndex: integer;
    procedure SetIndex(const AValue: integer);
    property Index: Integer read GetIndex write SetIndex;
  end;

  { INodeNavigation }

  INodeNavigation = interface
  ['{2F485E3E-5296-4837-93D9-7F708C70979B}']
    function MoveNext(const ACursor: IBlackBox): INode;
  end;

  { TNodeEnumerator }

  TNodeEnumerator = class(TInterfacedObject, INodeEnumerator)
  protected
    fCurrent: INode;
    fCursor: IBlackBox;
    fNavigation: INodeNavigation;
  protected
    // INodeEnumerator
    function MoveNext: Boolean;
    function GetCurrent: INode;
    property Current: INode read GetCurrent;
    // INodeCursor
    procedure SetCurrent(const AValue: INode);
  public
    constructor Create(const ANavigation: INodeNavigation);
  end;

  { TNodeCursor }

  TNodeCursor = class(TInterfacedObject, INodeCursor, IBlackBox)
  protected
    fIndex: integer;
  protected
    //INodeCursor
    function GetIndex: integer;
    procedure SetIndex(const AValue: integer);
    property Index: Integer read GetIndex write SetIndex;
  public
    constructor Create;
  end;

  { TNode }

  TNode = class(TInterfacedObject, INode, INodeNavigation)
  protected
    // INodeNavigation
    function MoveNext(const ACursor: IBlackBox): INode; virtual; abstract;
  protected
    //INode
    procedure AddChild(const ANode: INode); virtual; abstract;
    procedure RemoveChild(const ANode: INode); virtual; abstract;
    procedure Insert(const AIndex: integer; const ANode: INode); virtual; abstract;
    procedure Delete(const AIndex: integer); virtual; abstract;
    function Count: integer; virtual; abstract;
    function GetChild(const AIndex: integer): INode; virtual; abstract;
    function GetEnumerator: INodeEnumerator;
  public
    destructor Destroy; override;
  end;

  { TParentNode }

  TParentNode = class(TNode)
  protected type
    TItems = specialize TFPGInterfacedObjectList<INode>;
  protected
    fChildren: TItems;
    function GetChildren: TItems;
    property Children: TItems read GetChildren;
  protected
    function MoveNext(const ACursor: IBlackBox): INode; override;
    procedure AddChild(const ANode: INode); override;
    procedure RemoveChild(const ANode: INode); override;
    procedure Insert(const AIndex: integer; const ANode: INode); override;
    procedure Delete(const AIndex: integer); override;
    function Count: integer; override;
    function GetChild(const AIndex: integer): INode; override;
  public
    destructor Destroy; override;
  end;

  { TLeafNode }

  TLeafNode = class(TNode)
  protected
    function MoveNext(const ACursor: IBlackBox): INode; override;
    procedure AddChild(const ANode: INode); override;
    procedure RemoveChild(const ANode: INode); override;
    procedure Insert(const AIndex: integer; const ANode: INode); override;
    procedure Delete(const AIndex: integer); override;
    function Count: integer; override;
    function GetChild(const AIndex: integer): INode; override;
  end;

implementation

{ TNodeCursor }

function TNodeCursor.GetIndex: integer;
begin
  Result := fIndex;
end;

procedure TNodeCursor.SetIndex(const AValue: integer);
begin
  fIndex := AValue;
end;

constructor TNodeCursor.Create;
begin
  fIndex := -1;
end;

{ TParentNode }

function TParentNode.GetChildren: TItems;
begin
  if fChildren = nil then
    fChildren := TItems.Create;
  Result := fChildren;
end;

function TParentNode.MoveNext(const ACursor: IBlackBox): INode;
var
  mCursor: INodeCursor;
begin
  //ACursor.Current := ACursor.;
  //fChildren;
  mCursor := ACursor as INodeCursor;
  mCursor.Index := mCursor.Index + 1;
  if (mCursor.Index < 0) or (mCursor.Index > Children.Count - 1) then
    Result := nil
  else
    Result := Children.Items[mCursor.Index];
end;

procedure TParentNode.AddChild(const ANode: INode);
begin
  Children.Add(ANode);
end;

procedure TParentNode.RemoveChild(const ANode: INode);
begin
  Children.Remove(ANode);
end;

procedure TParentNode.Insert(const AIndex: integer; const ANode: INode);
begin
  Children.Insert(AIndex, ANode);
end;

procedure TParentNode.Delete(const AIndex: integer);
begin
  Children.Delete(AIndex);
end;

function TParentNode.Count: integer;
begin
  Result := Children.Count;
end;

function TParentNode.GetChild(const AIndex: integer): INode;
begin
  Result := Children.Items[AIndex];
end;

destructor TParentNode.Destroy;
begin
  FreeAndNil(fChildren);
  inherited Destroy;
end;

{ TLeafNode }

function TLeafNode.MoveNext(const ACursor: IBlackBox): INode;
begin
  Result := nil;
end;

procedure TLeafNode.AddChild(const ANode: INode);
begin
end;

procedure TLeafNode.RemoveChild(const ANode: INode);
begin
end;

procedure TLeafNode.Insert(const AIndex: integer; const ANode: INode);
begin
end;

procedure TLeafNode.Delete(const AIndex: integer);
begin
end;

function TLeafNode.Count: integer;
begin
  Result := 0;
end;

function TLeafNode.GetChild(const AIndex: integer): INode;
begin
  Result := nil;
end;

{ TNodeEnumerator }

function TNodeEnumerator.MoveNext: Boolean;
begin
  fCurrent := fNavigation.MoveNext(fCursor);
  Result := Current <> nil;
end;

function TNodeEnumerator.GetCurrent: INode;
begin
  Result := fCurrent;
end;

procedure TNodeEnumerator.SetCurrent(const AValue: INode);
begin
  fCurrent := AValue;
end;

constructor TNodeEnumerator.Create(const ANavigation: INodeNavigation);
begin
  fNavigation := ANavigation;
  fCursor := TNodeCursor.Create;
end;

{ TNode }

function TNode.GetEnumerator: INodeEnumerator;
begin
  Result := TNodeEnumerator.Create(Self);
end;

destructor TNode.Destroy;
begin
  inherited Destroy;
end;

end.

