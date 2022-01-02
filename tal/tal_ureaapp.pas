unit tal_ureaapp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, trl_uprops,
  trl_idifactory, flu_iflux, rea_iapp,
  trl_iExecutor,
  trl_ilog,
  Forms,  // later remove and make Application accessible through interface
  LMessages, LCLType, LCLIntf, InterfaceBase,
  trl_imetaelementfactory,
  rea_ibits, trl_imetaelement, Classes,
  rea_idesigncomponent, rea_irenderer,
  trl_ireconciler;

type

  { TReactApp }

  TReactApp = class(TInterfacedObject, IReactApp)
  protected
    // IReactApp
    procedure StartUp;
    procedure ShutDown;
  protected
    procedure IdleHandler(Sender: TObject; var Done: Boolean);
    procedure KeyDownBeforeHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    fLog: ILog;
    fExecutor: IExecutor;
  published
    property Log: ILog read fLog write fLog;
    property Executor: IExecutor read fExecutor write fExecutor;
  end;

implementation

{ TReactApp }

procedure TReactApp.StartUp;
begin
  Application.AddOnIdleHandler(@IdleHandler);
  Application.AddOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
end;

procedure TReactApp.ShutDown;
begin
  Application.RemoveOnIdleHandler(@IdleHandler);
  Application.RemoveOnKeyDownBeforeHandler(@KeyDownBeforeHandler);
end;

procedure TReactApp.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  Executor.ExecuteAll;
end;

procedure TReactApp.KeyDownBeforeHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_L) and (Shift = [ssCtrl])
  then begin
    Log.Visible := not Log.Visible;
  end;
end;

end.

