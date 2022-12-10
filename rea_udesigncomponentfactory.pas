unit rea_udesigncomponentfactory;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_iprops, rea_iflux, trl_idifactory, trl_isequence,
  rea_ilayout, rea_udesigncomponentfunc, rea_udesigncomponentdata, trl_itree,
  graphics, trl_udifactory, rea_irenderer, rea_iprops, rea_uprops;

type

  { TDesignComponentFactory }

  TDesignComponentFactory = class(TInterfacedObject, IDesignComponentFactory)
  protected
    function New(const AProps: IProps): IDesignComponent;
    function DoNew(const AProps: IProps): IDesignComponent; virtual; abstract;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
    function NewProps: IProps;
  protected
    fFactory: IDIFactory;
    fFactory2: TDIFactory2;
    fFluxDispatcher: IFluxDispatcher;
    fSequence: ISequence;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
    property Sequence: ISequence read fSequence write fSequence;
  end;

  { TDesignComponentGridFactory }

  TDesignComponentGridFactory = class(TDesignComponentFactory, IDesignComponentGridFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentLabelEditFactory }

  TDesignComponentLabelEditFactory = class(TDesignComponentFactory, IDesignComponentLabelEditFactory)
  private
    function TextProps(const AProps: IProps): IProps;
    function NewText(const AProps: IProps): IDesignComponent;
    function EditProps(const AProps: IProps): IProps;
    function NewEdit(const AProps: IProps): IDesignComponent;
    function ContainerProps(const AProps: IProps): IProps;
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
    function NewDCProps: IDCLabelEditProps;
  end;

implementation

{ TDesignComponentLabelEditFactory }

function TDesignComponentLabelEditFactory.NewEdit(const AProps: IProps): IDesignComponent;
var
  //mF: IDesignComponentEditFactory;
  mEdit: IDesignComponent;
begin
  //mF := IDesignComponentEditFactory(Factory.Locate(IDesignComponentEditFactory));
  //mEdit := mF.New(AProps.Clone
  //  .SetInt(cProps.Place, cPlace.Elastic)
  //);
  Result := IDesignComponentFrame(Factory.Locate(IDesignComponentFrame, '',
    NewProps
      .SetInt(cProps.Border, AProps.AsInt(cProps.CaptionEditBorder))
      .SetInt(cProps.BorderColor, AProps.AsInt(cProps.CaptionEditBorderColor))
  ));
  (Result as INode).AddChild(mEdit as INode);
end;

function TDesignComponentLabelEditFactory.TextProps(const AProps: IProps): IProps;
begin
  Result := AProps.Clone
    .SetStr(cProps.Text, AProps.AsStr(cProps.Caption));
  case AProps.AsInt(cProps.CaptionEdge) of
    cEdge.Left, cEdge.Top:
      Result.SetInt(cProps.Place, cPlace.FixFront);
    cEdge.Right, cEdge.Bottom:
      Result.SetInt(cProps.Place, cPlace.FixBack);
  end;
  case AProps.AsInt(cProps.CaptionEdge) of
    cEdge.Left, cEdge.Right:
      Result.SetInt(cProps.MMWidth, AProps.AsInt(cProps.CaptionWidth));
    cEdge.Top, cEdge.Bottom:
      Result.SetInt(cProps.MMHeight, AProps.AsInt(cProps.CaptionHeight));
  end;
end;

function TDesignComponentLabelEditFactory.ContainerProps(const AProps: IProps
  ): IProps;
begin
  Result := AProps.Clone([cProps.Color, cProps.Transparent, cProps.MMWidth, cProps.MMHeight,
    cProps.Border, cProps.BorderColor, cProps.Place]);
  case AProps.AsInt(cProps.CaptionEdge) of
    cEdge.Left, cEdge.Right:
      Result.SetInt(cProps.Layout, cLayout.Horizontal);
    cEdge.Top, cEdge.Bottom:
      Result.SetInt(cProps.Layout, cLayout.Vertical);
  end;
end;

function TDesignComponentLabelEditFactory.NewText(const AProps: IProps): IDesignComponent;
//var
//  mF: IDesignComponentTextFactory;
begin
  //mF := IDesignComponentTextFactory(Factory.Locate(IDesignComponentTextFactory));
  //Result := mF.New(AProps);
end;

function TDesignComponentLabelEditFactory.EditProps(const AProps: IProps
  ): IProps;
begin
  Result := AProps.Clone
    .SetInt(cProps.Place, cPlace.Elastic);
end;

function TDesignComponentLabelEditFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  //mF: IDesignComponentStripFactory;
  mText, mEdit: IDesignComponent;
begin
  //mF := IDesignComponentStripFactory(Factory.Locate(IDesignComponentStripFactory));
  //Result := mF.New(ContainerProps(AProps));
  mEdit := NewEdit(EditProps(AProps));
  mText := NewText(TextProps(AProps));
  case AProps.AsInt(cProps.CaptionEdge) of
    cEdge.Left, cEdge.Top:
     begin
       (Result as INode).AddChild(mText as INode);
       (Result as INode).AddChild(mEdit as INode);
     end;
    cEdge.Right, cEdge.Bottom:
     begin
       (Result as INode).AddChild(mEdit as INode);
       (Result as INode).AddChild(mText as INode);
     end;
  end;
end;

function TDesignComponentLabelEditFactory.NewDCProps: IDCLabelEditProps;
begin
  Result := TDCLabelEditProps.Create;
  Result
    .SetCaption('???')
    .SetCaptionEdge(cEdge.Top)
    .SetCaptionHeight(25)
    .SetPlace(cPlace.Elastic)
    .SetMMWidth(250)
    .SetMMHeight(50)
    .SetColor(clPurple);
end;

{ TDesignComponentGridFactory }

function TDesignComponentGridFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TGridData;
  mTextChangedFunc, mKeyDownFunc: IFluxFunc;
begin
  mData := AProps.AsObject(cProps.Data) as TGridData;
  mTextChangedFunc := TGridEdTextChangedFunc.Create(Sequence.Next, mData, NewNotifier(cNotifyRender));
  FluxDispatcher.RegisterFunc(mTextChangedFunc);
  mKeyDownFunc := TGridEdKeyDownFunc.Create(Sequence.Next, mData, NewNotifier(cNotifyRender));
  FluxDispatcher.RegisterFunc(mKeyDownFunc);
  //mProps := AProps.Clone
  //  .SetObject(cProps.Data, mData)
  //  .SetIntf(cEdit.EdTextChangedNotifier, NewNotifier(mTextChangedFunc.ID))
  //  .SetIntf(cEdit.EdKeyDownNotifier, NewNotifier(mKeyDownFunc.ID));
  Result := IDesignComponentGrid(Factory.Locate(IDesignComponentGrid, '', mProps));
end;

{ TDesignComponentFactory }

function TDesignComponentFactory.New(const AProps: IProps): IDesignComponent;
begin
  Result := DoNew(AProps);
end;

function TDesignComponentFactory.NewNotifier(const AActionID: integer
  ): IFluxNotifier;
begin
  Result := IFluxNotifier(Factory.Locate(IFluxNotifier, '',
    NewProps
    .SetInt(cAction.ActionID, AActionID)
    .SetIntf(cAction.Dispatcher, FluxDispatcher)
  ));
end;

function TDesignComponentFactory.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

end.

