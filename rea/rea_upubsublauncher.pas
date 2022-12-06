unit rea_upubsublauncher;

{$mode delphi}{$H+}

interface

uses
  sysutils,
  trl_ilauncher, forms, trl_pubsub,
  rea_irenderer, rea_urenderer,
  rea_idesigncomponent,
  trl_imetaelement;

type

  { TPubSubLauncher }

  TPubSubLauncher = class(TInterfacedObject, ILauncher)
  private
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
    fPubSub: IPubSub;
    fRenderer: IRenderer;
    fGUI: IDesignComponentApp;
    fIsRendering: Boolean;
  published
    property PubSub: IPubSub read fPubSub write fPubSub;
    property Renderer: IRenderer read fRenderer write fRenderer;
    property GUI: IDesignComponentApp read fGUI write fGUI;
  end;

implementation

{ TPubSubLauncher }

procedure TPubSubLauncher.StartUp;
begin
  GUI.PSGUIChannel.Subscribe(PSGUIChannelObserver);
  GUI.PSGUIChannel.Publish(TGUIData.Create(gaRender));
end;

procedure TPubSubLauncher.ShutDown;
begin
  GUI.PSGUIChannel.Unsubscribe(PSGUIChannelObserver);
  Application.Terminate;
end;

procedure TPubSubLauncher.Loop;
var
  mStop: Boolean;
begin
  mStop := False;
  repeat
    try
      if PubSub.IsEmpty then
        Application.ProcessMessages
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
  if fIsRendering then
    Exit;
  fIsRendering := True;
  try
    mEl := Gui.Compose(nil, []);
    if mEl = nil then begin
      raise exception.create('nil element');
    end;
    Renderer.Render(mEl);
  finally
    fIsRendering := False;
  end;
end;

procedure TPubSubLauncher.PSGUIChannelObserver(const AData: TGUIData);
begin
  if AData.Action = gaRender then
    Render;
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

