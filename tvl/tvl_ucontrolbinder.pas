(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit tvl_ucontrolbinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, LCLProc;

const
  LM_BEHAVEBINDER = LM_USER + $100;
  LM_DISCONECTWINDOWPROC = LM_BEHAVEBINDER + 01;

type

  { TControlBinder }

  TControlBinder = class(TInterfacedObject)
  private type

    PMethod = ^TMethod;

    TLMBBWINDOWPROC = record
      Msg: Cardinal;
    {$ifdef cpu64}
      UnusedMsg: Cardinal;
    {$endif}
      DisposedProc: PMethod;
      StoredProc: PMethod;
      Result: PtrInt;
    end;

    { TNotificator }

    TNotificator = class(TComponent)
    private
      fBinder: TControlBinder;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(ABinder: TControlBinder); reintroduce;
    end;

  private
    fControl: TControl;
    fControlNotificator: TNotificator;
    fOldWndProc: TWndMethod;
    procedure HookWndPproc;
    procedure UnhookWndProc;
  protected
    procedure ControlWndProc(var TheMessage: TLMessage);
    procedure DoControlWndProc(var TheMessage: TLMessage); virtual;
    property Control: TControl read fControl;
    procedure BindControl; virtual;
    procedure UnbindControl; virtual;
  public
    destructor Destroy; override;
    procedure Bind(AControl: TControl);
    procedure Unbind;
  end;

implementation

type
    { TWinControlHelper }

    TWinControlHelper = class helper for TWinControl
    public
      function H_IsControlMouseMsg(var TheMessage): Boolean;
    end;

{ TWinControlHelper }

function TWinControlHelper.H_IsControlMouseMsg(var TheMessage): Boolean;
begin
  Result := IsControlMouseMsg(TheMessage);
end;



{ TControlBinder }

procedure TControlBinder.HookWndPproc;
begin
  //if (Control is TWinControl) or (Control.Parent = nil) then
  begin
    fOldWndProc := Control.WindowProc;
    Control.WindowProc := @ControlWndProc;
  end
  //else
  //begin
  //  fOldWndProc := Control.Parent.WindowProc;
  //  Control.Parent.WindowProc := @DoControlWndProc;
  //end;
end;

procedure TControlBinder.UnhookWndProc;
var
  mMsgWP: TLMBBWINDOWPROC;
  mMsg: TLMessage absolute mMsgWP;
  mdWndProc: TWndMethod;
begin
  mdWndProc := @ControlWndProc;
  if (TMethod(Control.WindowProc).Code = TMethod(mdWndProc).Code)
     and (TMethod(Control.WindowProc).Data = TMethod(mdWndProc).Data) then
    // this object hook belongs to this binder
    Control.WindowProc := fOldWndProc
  else
  begin
    // something hooked after me, so need to notify it to adjust its fOldWndProc
    // (WndProc is method - so we need to compare code and data aswell)
    mMsgWP.Msg := LM_DISCONECTWINDOWPROC;
    mMsgWP.DisposedProc := @TMethod(mdWndProc);
    mMsgWP.StoredProc := @TMethod(fOldWndProc);
    mMsgWP.Result := 0;
    Control.WindowProc(mMsg);
    if mMsgWP.Result <> 1 then
      raise Exception.Create('Problem with unhook windowproc from chain - superior hook not responsed');
  end;
end;

procedure TControlBinder.ControlWndProc(var TheMessage: TLMessage);
var
  mMsg: TLMBBWINDOWPROC absolute TheMessage;
begin
  if TheMessage.msg = LM_DISCONECTWINDOWPROC then
  begin
    if (mMsg.DisposedProc^.Code = TMethod(fOldWndProc).Code)
       and (mMsg.DisposedProc^.Data = TMethod(fOldWndProc).Data) then
    begin
      fOldWndProc := TWndMethod(mMsg.StoredProc^);
      mMsg.Result := 1;
    end
    else
      fOldWndProc(TheMessage);
  end
  else
  begin
    DoControlWndProc(TheMessage);
  end;
end;

procedure TControlBinder.DoControlWndProc(var TheMessage: TLMessage);
begin
  fOldWndProc(TheMessage);
end;

procedure TControlBinder.BindControl;
begin
end;

procedure TControlBinder.UnbindControl;
begin
end;

destructor TControlBinder.Destroy;
begin
  Unbind;
  FreeAndNil(fControlNotificator);
  inherited Destroy;
end;

procedure TControlBinder.Bind(AControl: TControl);
begin
  fControl := AControl;
  HookWndPproc;
  FreeAndNil(fControlNotificator);
  fControlNotificator := TNotificator.Create(Self);
  BindControl;
end;

procedure TControlBinder.Unbind;
begin
  if fControl <> nil then
  begin
    FreeAndNil(fControlNotificator);
    UnbindControl;
    UnhookWndProc;
    fControl := nil;
  end;
end;

{ TControlBinder.TNotificator }

procedure TControlBinder.TNotificator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = fBinder.Control) then
  begin
    fBinder.Unbind;
  end;
end;

constructor TControlBinder.TNotificator.Create(ABinder: TControlBinder);
begin
  inherited Create(nil);
  fBinder := ABinder;
  fBinder.Control.FreeNotification(Self);
end;

end.

