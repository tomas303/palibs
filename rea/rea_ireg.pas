unit rea_ireg;

{$mode objfpc}{$H+}

interface

uses
  Classes, trl_dicontainer;

const
  cR_DesktopTiler = 'desktoptiler';

type
  IReg = interface
  ['{E5558E11-D2B3-439A-AAB0-A124C82F9D1D}']
    function RegisterBitTerminus(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string; const ATilerID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; const ATilerID: string): TDIReg;
    function RegisterBitTiler(ATilerClass: TClass; ATilerInterface: TGuid; const ATilerID: string;
      AScaleClass: TClass): TDIReg;

    function RegisterMessageNotifierBinder: TDIReg;

    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReconciliator: TDIReg;

    function RegisterReactComponent(ACompositeClass: TClass; ACompositeInterface: TGuid;
      AMapStateKeys: array of string): TDIReg;

    function RegisterMachinery(AMachineryCompositeClass: TClass; AMachineryInterface: TGuid): TDIReg;
    function RegisterReact(const AID: string = ''): TDIReg;

    procedure RegisterCommon;
  end;


implementation

end.

