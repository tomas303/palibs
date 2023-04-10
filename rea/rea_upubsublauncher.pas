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
unit rea_upubsublauncher;

{$mode delphi}{$H+}

interface

uses
  sysutils,
  trl_ilauncher, forms, trl_pubsub,
  rea_irenderer, rea_urenderer,
  rea_idesigncomponent,
  trl_imetaelement, trl_iprops, trl_udifactory;

type

  { TPubSubLauncher }

  TPubSubLauncher = class(TInterfacedObject, ILauncher)
  private
    fPSGUIChannel: IPSGUIChannel;
    fGUI: IDesignComponentApp;
    procedure StartUp;
    procedure ShutDown;
    procedure Loop;
    procedure Render;
    procedure PSGUIChannelObserver(const AData: TGUIData);
  protected
    // ILauncher
    procedure Launch;
  public
    procedure AfterConstruction; override;
  protected
    fFactory2: TDIFactory2;
    fPubSub: IPubSub;
    fRenderer: IRenderer;
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property PubSub: IPubSub read fPubSub write fPubSub;
    property Renderer: IRenderer read fRenderer write fRenderer;
  end;

implementation

{ TPubSubLauncher }

procedure TPubSubLauncher.StartUp;
begin
  fPSGUIChannel := PubSub.Factory.NewDataChannel<TGUIData>;
  fPSGUIChannel.Subscribe(PSGUIChannelObserver);
  fGUI := Factory2.Locate<IDesignComponentApp>(
    Factory2.Locate<IProps>.SetIntf('PSGUIChannel', fPSGUIChannel)
  );
  fPSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

procedure TPubSubLauncher.ShutDown;
begin
  fPSGUIChannel.Unsubscribe(PSGUIChannelObserver);
  PubSub.Factory.DropDataChannel<TGUIData>(fPSGUIChannel);
  Application.Terminate;
end;

procedure TPubSubLauncher.Loop;
var
  mStop: Boolean;
begin
  mStop := False;
  repeat
    try
      if PubSub.IsEmpty then begin
        Application.ProcessMessages;
        PubSub.PublishDebounced;
      end
      else
        PubSub.ExecEvent;
    except
      on E: ELaunchStop do begin
        mStop := True;
      end;
    end;
  until mStop;
end;

procedure TPubSubLauncher.Render;
var
  mEl: IMetaElement;
begin
  mEl := fGui.Compose;
  if mEl = nil then begin
    raise exception.create('nil element');
  end;
  Renderer.Render(mEl);
end;

procedure TPubSubLauncher.PSGUIChannelObserver(const AData: TGUIData);
begin
  if AData.Action = gaRender then begin
    Render;
  end;
end;

procedure TPubSubLauncher.Launch;
begin
  StartUp;
  try
    Loop;
  finally
    ShutDown;
  end;
end;

procedure TPubSubLauncher.AfterConstruction;
begin
  inherited AfterConstruction;
  Application.Initialize;
end;

end.

