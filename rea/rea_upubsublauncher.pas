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
  mEl := fGui.Compose{(nil, [])};
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

