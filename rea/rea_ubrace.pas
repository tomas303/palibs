unit rea_ubrace;

{$mode objfpc}{$H+}

interface

uses
  rea_ibrace, trl_itree, trl_imetaelement, trl_ilog, fgl, rea_ireact,
  trl_idifactory, SysUtils, rea_ibits, trl_inexus, math;

type

  { TBrace }

  TBrace = class(TInterfacedObject, IBrace, INode)
  protected type
    TComponentChain = specialize TFPGInterfacedObjectList<IReactComponent>;
  protected
    // IBrace
    function Refresh(const AElement: IMetaElement): IBit;
    function Remove(const AParent: IBit): IBit;
  protected
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure ExchangeChild(const AFromNode, AToNode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    fComponentChain: TComponentChain;
  protected
    fLog: ILog;
    fNode: INode;
    fFactory: IDIFactory;
    fNexus: INexus;
    function ChainBitElement(const AInput: IMetaElement): IMetaElement;
    procedure ProcessChildren(const ABitElement: IMetaElement);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Factory: IDIFactory read fFactory write fFactory;
    property Nexus: INexus read fNexus write fNexus;
  end;

implementation

{ TBrace }

function TBrace.Refresh(const AElement: IMetaElement): IBit;
var
  mNewBitElement: IMetaElement;
  i: integer;
  //mOldChildEl: IMetaElement;
  mNewChildEl: IMetaElement;
  mBit: IBit;
  mChild: IBrace;
  mNewChildBit: IBit;

  mmm: IUnknown;
  sss: string;
begin
  mNewBitElement := ChainBitElement(AElement);

  //if fNexus = nil then
  //  fNexus := IBit(Factory.Locate(mNewBitElement.Guid, mNewBitElement.TypeID, mNewBitElement.Props))
  //else
  //  fNexus := fNexus.Renew(mNewBitElement) as IBit;


  mmm := fNexus.Renew(mNewBitElement);
  sss := (mmm as TObject).ClassName;

  Result := fNexus.Renew(mNewBitElement) as IBit;


  // result has children elements - should fit with brace children ... based on order
  for i := 0 to Count - 1 do
  begin
    mChild := GetChild(i) as IBrace;
    mNewChildEl := (mNewBitElement as INode).Child[i] as IMetaElement;
    mNewChildBit := mChild.Refresh(mNewChildEl);
    //(Result as INode).Child[i] := mNewChildBit;
  end;

end;

function TBrace.Remove(const AParent: IBit): IBit;
begin
  (AParent as INode).RemoveChild(fNexus.Instance as INode);
end;

procedure TBrace.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TBrace.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TBrace.ExchangeChild(const AFromNode, AToNode: INode);
begin
  Node.ExchangeChild(AFromNode, AToNode);
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

function TBrace.ChainBitElement(const AInput: IMetaElement): IMetaElement;
var
  mProvider: IMetaElementProvider;
  sss: string;
  mEl: IMetaElement;
begin
  if Factory.CanLocateAs(AInput.Guid, IMetaElementProvider) then
  begin
    mProvider := IUnknown(Factory.Locate(AInput.Guid, AInput.TypeID, AInput.Props)) as IMetaElementProvider;
    sss := (mProvider as tobject).ClassName;
    mEl := mProvider.ProvideMetaElement;
    Result := ChainBitElement(mEl);
  end
  else
    Result := AInput;
end;

procedure TBrace.ProcessChildren(const ABitElement: IMetaElement);
var
  mOldChildEl: IMetaElement;
  mNewChildEl: IMetaElement;
  i: Integer;
  mChildBit: IBit;
  mChild: IBrace;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  // changed
  for i := 0 to Min((Self as INode).Count, (ABitElement as INode).Count) - 1 do
  begin
    mNewChildEl := (ABitElement as INode).Child[i] as IMetaElement;
    mChild := (Self as INode).Child[i] as IBrace;
    mChildBit := mChild.Refresh(mNewChildEl);
    (fNexus as INode).Delete(i);
    (fNexus as INode).Insert(i, mChildBit as INode);
  end;
  if (Self as INode).Count < (ABitElement as INode).Count then
  begin
    // add new
    for i := (Self as INode).Count to (ABitElement as INode).Count - 1 do
    begin
      mNewChildEl := (ABitElement as INode).Child[i] as IMetaElement;
      mChild := IBrace(Factory.Locate(IBrace));
      mChildBit := mChild.Refresh(mNewChildEl);
      (fNexus as INode).AddChild(mChildBit as INode);
    end;
  end
  else
  begin
    // remove
    while (Self as INode).Count > (ABitElement as INode).Count do
    begin
      mChild := (Self as INode).Child[(Self as INode).Count - 1] as IBrace;
      mChild.Remove(fNexus.Instance as IBit);
      (Self as INode).RemoveChild(mChild as INode);
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

procedure TBrace.AfterConstruction;
begin
  inherited AfterConstruction;
  fComponentChain := TComponentChain.Create;
end;

procedure TBrace.BeforeDestruction;
begin
  FreeAndNil(fComponentChain);
  inherited BeforeDestruction;
end;

end.

