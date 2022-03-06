unit rea_udesigncomponentfunc;

{$mode objfpc}{$H+}

interface

uses
  rea_iflux, rea_udesigncomponentdata, LCLType, rea_idesigncomponent,
  trl_imetaelement, trl_iExecutor;

type

  { TDesignComponentFunc }

  TDesignComponentFunc = class(TInterfacedObject, IFluxFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); virtual; abstract;
  protected
    procedure Execute(const AAction: IFluxAction);
    function GetID: integer;
  public
    constructor Create(AID: integer);
  protected
    fID: integer;
  published
    property ID: integer read fID write fID;
  end;


  { TSizeFunc }

  TSizeFunc = class(TDesignComponentFunc, IFluxFunc)
  private
    fData: TFormData;
    fRenderNotifier: IFluxNotifier;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; AData: TFormData; ARenderNotifier: IFluxNotifier);
  end;

  { TMoveFunc }

  TMoveFunc = class(TDesignComponentFunc, IFluxFunc)
  private
    fData: TFormData;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; AData: TFormData);
  end;

  { TCloseQueryFunc }

  TCloseQueryFunc = class(TDesignComponentFunc, IFluxFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TGridFunc }

  TGridFunc = class(TDesignComponentFunc, IFluxFunc)
  private
    fRenderNotifier: IFluxNotifier;
  protected
    fData: TGridData;
  public
    constructor Create(AID: integer; const AData: TGridData; const ARenderNotifier: IFluxNotifier);
  end;

  { TGridEdTextChangedFunc }

  TGridEdTextChangedFunc = class(TGridFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TGridEdKeyDownFunc }

  TGridEdKeyDownFunc = class(TGridFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  { TextChangedFunc }

  TextChangedFunc = class(TDesignComponentFunc, IFluxFunc)
  private
    fData: TEditData;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; AData: TEditData);
  end;

  { TTabChangedFunc }

  TTabChangedFunc = class(TDesignComponentFunc)
  private
    fPagerData: TPagerData;
    fIndex: integer;
    fRenderNotifier: IFluxNotifier;
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; APagerData: TPagerData;
      const ARenderNotifier: IFluxNotifier; AIndex: integer);
  end;

implementation

{ TCloseQueryFunc }

procedure TCloseQueryFunc.DoExecute(const AAction: IFluxAction);
begin
  raise EExecutorStop.Create('');
end;

{ TTabChangedFunc }

procedure TTabChangedFunc.DoExecute(
  const AAction: IFluxAction);
begin
  fPagerData.ActiveIndex := fIndex;
  fRenderNotifier.Notify;
end;

constructor TTabChangedFunc.Create(AID: integer; APagerData: TPagerData;
      const ARenderNotifier: IFluxNotifier; AIndex: integer);
begin
  inherited Create(AID);
  fPagerData := APagerData;
  fRenderNotifier := ARenderNotifier;
  fIndex := AIndex;
end;

{ TDesignComponentFunc }

procedure TDesignComponentFunc.Execute(const AAction: IFluxAction);
begin
  DoExecute(AAction);
end;

function TDesignComponentFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TDesignComponentFunc.Create(AID: integer);
begin
  inherited Create;
  fID := AID;
end;

{ TGridFunc }

constructor TGridFunc.Create(AID: integer; const AData: TGridData; const ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID);
  fData := AData;
  fRenderNotifier := ARenderNotifier;
end;

{ TGridEdTextChangedFunc }

procedure TGridEdTextChangedFunc.DoExecute(const AAction: IFluxAction);
begin
  fData.EditData.Text := AAction.Props.AsStr('Text');
  fData[fData.CurrentRow, fData.CurrentCol] := AAction.Props.AsStr('Text');
end;

{ TGridEdKeyDownFunc }

procedure TGridEdKeyDownFunc.DoExecute(const AAction: IFluxAction);
begin
  case AAction.Props.AsInt('CharCode') of
    VK_ESCAPE:
      fData.BrowseMode := True;
    VK_RETURN:
      fData.BrowseMode := False;
    VK_LEFT:
      if fData.BrowseMode and (fData.CurrentCol > 0) then begin
        fData.CurrentCol := fData.CurrentCol - 1;
        fRenderNotifier.Notify;
      end;
    VK_RIGHT:
      if fData.BrowseMode and (fData.CurrentCol < fData.ColCount - 1) then begin
        fData.CurrentCol := fData.CurrentCol + 1;
        fRenderNotifier.Notify;
      end;
    VK_UP:
      if fData.BrowseMode and (fData.CurrentRow > 0) then begin
        fData.CurrentRow := fData.CurrentRow - 1;
        fRenderNotifier.Notify;
      end;
    VK_DOWN:
      if fData.BrowseMode and (fData.CurrentRow < fData.RowCount - 1) then begin
        fData.CurrentRow := fData.CurrentRow + 1;
        fRenderNotifier.Notify;
      end;
  end;
end;

{ TextChangedFunc }

procedure TextChangedFunc.DoExecute(const AAction: IFluxAction);
begin
  fData.Text := AAction.Props.AsStr('Text');
  if fData.ChangedNotifier <> nil then
    fData.ChangedNotifier.Notify;
end;

constructor TextChangedFunc.Create(AID: integer; AData: TEditData);
begin
  inherited Create(AID);
  fData := AData;
end;

{ TMoveFunc }

procedure TMoveFunc.DoExecute(const AAction: IFluxAction);
begin
  fData.Left := AAction.Props.AsInt(cProps.MMLeft);
  fData.Top := AAction.Props.AsInt(cProps.MMTop);
end;

constructor TMoveFunc.Create(AID: integer; AData: TFormData);
begin
  inherited Create(AID);
  fData := AData;
end;

{ TSizeFunc }

procedure TSizeFunc.DoExecute(const AAction: IFluxAction);
var
  mChange: Boolean;
begin
  mChange := False;
  if fData.Width <> AAction.Props.AsInt(cProps.MMWidth) then begin
    fData.Width := AAction.Props.AsInt(cProps.MMWidth);
    mChange := True;
  end;
  if fData.Height <> AAction.Props.AsInt(cProps.MMHeight) then begin
    fData.Height := AAction.Props.AsInt(cProps.MMHeight);
    mChange := True;
  end;
  if mChange then
     fRenderNotifier.Notify;
end;

constructor TSizeFunc.Create(AID: integer; AData: TFormData; ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID);
  fData := AData;
  fRenderNotifier := ARenderNotifier;
end;

end.

