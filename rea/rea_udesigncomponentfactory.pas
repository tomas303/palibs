unit rea_udesigncomponentfactory;

{$mode objfpc}{$H+}

interface

uses
  rea_idesigncomponent, trl_iprops, flu_iflux, trl_idifactory, trl_isequence,
  rea_ilayout, rea_udesigncomponentfunc, rea_udesigncomponentdata;

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
    fFluxDispatcher: IFluxDispatcher;
    fActionIDSequence: ISequence;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
    property ActionIDSequence: ISequence read fActionIDSequence write fActionIDSequence;
  end;

  { TDesignComponentFormFactory }

  TDesignComponentFormFactory = class(TDesignComponentFactory, IDesignComponentFormFactory)
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

implementation

{ TDesignComponentPagerFactory }

function TDesignComponentPagerFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TPagerData;
begin
  mData := AProps.AsObject('Data') as TPagerData;
  mProps := AProps.Clone
    .SetObject('Data', mData)
    .SetIntf('SwitchFactory', IUnknown(Factory.Locate(IDesignComponentPagerSwitchFactory)));
  Result := IDesignComponentPager(Factory.Locate(IDesignComponentPager, '', mProps));
end;

{ TDesignComponentFormFactory }

function TDesignComponentFormFactory.DoNew(const AProps: IProps
  ): IDesignComponent;
var
  mProps: IProps;
  mData: TFormData;
  mActClose, mActSize, mActMove: Integer;
begin
  mData := AProps.AsObject('Data') as TFormData;
  mActClose := ActionIDSequence.Next;
  mActSize := ActionIDSequence.Next;
  mActMove := ActionIDSequence.Next;
  mProps := AProps.Clone
    .SetObject('Data', mData)
    .SetIntf('CloseQueryNotifier', NewNotifier(mActClose))
    .SetIntf('SizeNotifier', NewNotifier(mActSize))
    .SetIntf('MoveNotifier', NewNotifier(mActMove));
  Result := IDesignComponentForm(Factory.Locate(IDesignComponentForm, '', mProps));
  fFluxDispatcher.RegisterFunc(TCloseQueryFunc.Create(mActClose));
  fFluxDispatcher.RegisterFunc(TSizeFunc.Create(mActSize, mData, NewNotifier(-400)));
  fFluxDispatcher.RegisterFunc(TMoveFunc.Create(mActMove, mData));
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
    .SetInt('ActionID', AActionID)
    .SetIntf('Dispatcher', FluxDispatcher)
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
  mActionID: Integer;
begin
  mActionID := ActionIDSequence.Next;
  mProps := AProps.Clone
    .SetInt(cProps.Place, cPlace.Elastic)
    .SetIntf(cProps.ClickNotifier, NewNotifier(mActionID));
  Result := IDesignComponentButton(Factory.Locate(IDesignComponentButton, '', mProps));
  FluxDispatcher.RegisterFunc(
    TTabChangedFunc.Create(
      mActionID,
      mProps.AsObject('PagerData') as TPagerData,
      NewNotifier(-400),
      mProps.AsInt('PageIndex')));
end;

end.

