unit uapp;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}

interface

uses
  graphics, tal_uapp,
  rea_idesigncomponent, rea_udesigncomponent,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher;

type
  { TApp }

  TApp = class(TALApp)
  protected
    procedure RegisterAppServices; override;
  end;

  { TGUI }

  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    procedure CloseProgram;
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

implementation

{ TGUI }

procedure TGUI.CloseProgram;
begin
  raise ELaunchStop.Create('');
end;

function TGUI.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray
  ): IMetaElement;
var
  mFF: IDesignComponentFormFactory;
  mFE: IDesignComponentEditFactory;
  mF, mE1, mE2: IDesignComponent;
  mTxtChannel: IPSTextChannel;
  mCloseChannel: IPSCloseChannel;
begin
  mTxtChannel := PubSub.Factory.NewDataChannel<String>;
  mCloseChannel := PubSub.Factory.NewChannel;
  mCloseChannel.Subscribe(CloseProgram);

  mF := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.Caption, 'Demo app')
    .SetInt(cProps.Color, clGreen)
    .SetInt(cProps.MMWidth, 500)
    .SetInt(cProps.MMHeight, 50)
    .SetIntf('PSCloseChannel', mCloseChannel)
    );

  mE1 := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetInt(cProps.Color, clRed)
    .SetInt(cProps.MMWidth, 50)
    .SetStr(cProps.Text, 'first')
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetIntf('PSTextChannel', mTxtChannel)
    );
  mE2 := Factory2.Locate<IDesignComponentEdit>(NewProps
    .SetInt(cProps.Color, clBlue)
    .SetInt(cProps.MMWidth, 50)
    .SetStr(cProps.Text, 'second')
    .SetInt(cProps.FontColor, clBlack)
    .SetInt(cProps.TextColor,  clYellow)
    .SetIntf('PSTextChannel', mTxtChannel)
    );


  (mF as INode).AddChild(mE1 as INode);
  (mF as INode).AddChild(mE2 as INode);
  Result := mF.Compose(nil, []);
end;

{ TApp }

procedure TApp.RegisterAppServices;
var
  mReg: TDIReg;
begin
  inherited RegisterAppServices;
  RegReact.RegisterPubSubLauncher;
  RegApps.RegisterWindowLog;
  RegReact.RegisterCommon;
  mReg := RegReact.RegisterDesignComponent(TGUI, IDesignComponentApp);
end;

end.

