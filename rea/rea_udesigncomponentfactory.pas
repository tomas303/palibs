unit rea_udesigncomponentfactory;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_iprops, flu_iflux, trl_idifactory, trl_isequence,
  rea_ilayout, rea_udesigncomponentfunc, rea_udesigncomponentdata, trl_itree,
  graphics, trl_udifactory;

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
    fActionIDSequence: ISequence;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
    property ActionIDSequence: ISequence read fActionIDSequence write fActionIDSequence;
  end;

  { TDesignComponentFormFactory }

  TDesignComponentFormFactory = class(TDesignComponentFactory, IDesignComponentFormFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentButtonFactory }

  TDesignComponentButtonFactory = class(TDesignComponentFactory, IDesignComponentButtonFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentEditFactory }

  TDesignComponentEditFactory = class(TDesignComponentFactory, IDesignComponentEditFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentTextFactory }

  TDesignComponentTextFactory = class(TDesignComponentFactory, IDesignComponentTextFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentStripFactory }

  TDesignComponentStripFactory = class(TDesignComponentFactory, IDesignComponentStripFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentPagerFactory }

  TDesignComponentPagerFactory = class(TDesignComponentFactory, IDesignComponentPagerFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TDesignComponentPagerSwitchFactory }

  TDesignComponentPagerSwitchFactory = class(TDesignComponentFactory, IDesignComponentPagerSwitchFactory)
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
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
  end;

implementation

{ TDesignComponentTextFactory }

function TDesignComponentTextFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
begin
  Result := IDesignComponentText(Factory.Locate(IDesignComponentText, '', AProps.Clone));
end;

{ TDesignComponentLabelEditFactory }

function TDesignComponentLabelEditFactory.NewEdit(const AProps: IProps): IDesignComponent;
var
  mF: IDesignComponentEditFactory;
  mEdit: IDesignComponent;
begin
  mF := IDesignComponentEditFactory(Factory.Locate(IDesignComponentEditFactory));
  mEdit := mF.New(AProps.Clone
    .SetInt(cProps.Place, cPlace.Elastic)
  );
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
    cProps.Border, cProps.BorderColor])
    .SetInt(cProps.Place, cPlace.FixFront);
  case AProps.AsInt(cProps.CaptionEdge) of
    cEdge.Left, cEdge.Right:
      Result.SetInt(cProps.Layout, cLayout.Horizontal);
    cEdge.Top, cEdge.Bottom:
      Result.SetInt(cProps.Layout, cLayout.Vertical);
  end;
end;

function TDesignComponentLabelEditFactory.NewText(const AProps: IProps): IDesignComponent;
var
  mF: IDesignComponentTextFactory;
begin
  mF := IDesignComponentTextFactory(Factory.Locate(IDesignComponentTextFactory));
  Result := mF.New(AProps);
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
  mF: IDesignComponentStripFactory;
  mText, mEdit: IDesignComponent;
begin
  mF := IDesignComponentStripFactory(Factory.Locate(IDesignComponentStripFactory));
  Result := mF.New(ContainerProps(AProps));
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

{ TDesignComponentStripFactory }

function TDesignComponentStripFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
begin
  Result := IDesignComponentStrip(Factory.Locate(IDesignComponentStrip, '', AProps.Clone));
end;

{ TDesignComponentEditFactory }

function TDesignComponentEditFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TEditData;
  mKeyDownFunc: IFluxFunc;
  mTextChangedFunc, mUserTextChangedFunc: IFluxFunc;
  mmm: string;
begin
  mProps := AProps.Clone;
  mData := mProps.AsObject(cProps.Data) as TEditData;
  if mData = nil then begin
    mData := TEditData.Create;
    mProps.SetObject(cProps.Data, mData);
  end;
  mUserTextChangedFunc := AProps.AsIntf(cProps.TextChangedFunc) as IFluxFunc;
  if mUserTextChangedFunc <> nil then begin
    mTextChangedFunc := TextChangedFunc.Create(mUserTextChangedFunc.ID, mData);
    FluxDispatcher.RegisterFunc(mTextChangedFunc);
    FluxDispatcher.RegisterFunc(mUserTextChangedFunc);
  end else begin
    mTextChangedFunc := TextChangedFunc.Create(ActionIDSequence.Next, mData);
    FluxDispatcher.RegisterFunc(mTextChangedFunc);
  end;
  mProps.SetIntf(cProps.TextChangedNotifier, NewNotifier(mTextChangedFunc.ID));
  mKeyDownFunc := AProps.AsIntf(cProps.KeyDownFunc) as IFluxFunc;
  if mKeyDownFunc <> nil then begin
    FluxDispatcher.RegisterFunc(mKeyDownFunc);
    mProps.SetIntf(cProps.KeyDownNotifier, NewNotifier(mKeyDownFunc.ID));
  end;
  mmm := mProps.Info;
  Result := IDesignComponentEdit(Factory.Locate(IDesignComponentEdit, '', mProps));
end;

{ TDesignComponentButtonFactory }

function TDesignComponentButtonFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mClickFunc: IFluxFunc;
begin
  mProps := AProps.Clone;
  mClickFunc := AProps.AsIntf(cProps.ClickFunc) as IFluxFunc;
  if mClickFunc <> nil then begin
    mProps.SetIntf(cProps.ClickNotifier, NewNotifier(mClickFunc.ID));
    FluxDispatcher.RegisterFunc(mClickFunc);
  end;
  Result := IDesignComponentButton(Factory.Locate(IDesignComponentButton, '', mProps));
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
  mTextChangedFunc := TGridEdTextChangedFunc.Create(ActionIDSequence.Next, mData, NewNotifier(-400));
  FluxDispatcher.RegisterFunc(mTextChangedFunc);
  mKeyDownFunc := TGridEdKeyDownFunc.Create(ActionIDSequence.Next, mData, NewNotifier(-400));
  FluxDispatcher.RegisterFunc(mKeyDownFunc);
  mProps := AProps.Clone
    .SetObject(cProps.Data, mData)
    .SetIntf(cEdit.EdTextChangedNotifier, NewNotifier(mTextChangedFunc.ID))
    .SetIntf(cEdit.EdKeyDownNotifier, NewNotifier(mKeyDownFunc.ID));
  Result := IDesignComponentGrid(Factory.Locate(IDesignComponentGrid, '', mProps));
end;

{ TDesignComponentPagerFactory }

function TDesignComponentPagerFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TPagerData;
begin
  mData := AProps.AsObject(cProps.Data) as TPagerData;
  if mData = nil then begin
    mData := TPagerData.Create;
  end;
  mProps := AProps.Clone
    .SetObject(cProps.Data, mData);
  Result := IDesignComponentPager(Factory.Locate(IDesignComponentPager, '', mProps));
end;

{ TDesignComponentFormFactory }

function TDesignComponentFormFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TFormData;
  mCloseFunc, mSizeFunc, mMoveFunc: IFluxFunc;
begin
  mData := AProps.AsObject(cProps.Data) as TFormData;
  if mData = nil then begin
    mData := TFormData.Create;
  end;
  mCloseFunc := TCloseQueryFunc.Create(ActionIDSequence.Next);
  mSizeFunc := TSizeFunc.Create(ActionIDSequence.Next, mData, NewNotifier(-400));
  mMoveFunc := TMoveFunc.Create(ActionIDSequence.Next, mData);
  mProps := AProps.Clone
    .SetObject(cProps.Data, mData)
    .SetIntf(cForm.CloseQueryNotifier, NewNotifier(mCloseFunc.ID))
    .SetIntf(cForm.SizeNotifier, NewNotifier(mSizeFunc.ID))
    .SetIntf(cForm.MoveNotifier, NewNotifier(mMoveFunc.ID));
  FluxDispatcher.RegisterFunc(mCloseFunc);
  FluxDispatcher.RegisterFunc(mSizeFunc);
  FluxDispatcher.RegisterFunc(mMoveFunc);
  Result := IDesignComponentForm(Factory.Locate(IDesignComponentForm, '', mProps));
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

{ TDesignComponentPagerSwitchFactory }

function TDesignComponentPagerSwitchFactory.DoNew(const AProps: IProps): IDesignComponent;
var
  mProps: IProps;
  mTabChangedFunc: IFluxFunc;
begin
  mTabChangedFunc := TTabChangedFunc.Create(
    ActionIDSequence.Next,
    AProps.AsObject(cPager.PagerData) as TPagerData,
    NewNotifier(-400),
    AProps.AsInt(cPager.PageIndex));
  FluxDispatcher.RegisterFunc(mTabChangedFunc);
  mProps := AProps.Clone
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetIntf(cProps.ClickNotifier, NewNotifier(mTabChangedFunc.ID));
  Result := IDesignComponentButton(Factory.Locate(IDesignComponentButton, '', mProps));
end;

end.

