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
    function RegisterTerminus(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string): TDIReg;
    function RegisterContainer(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string; const ATilerID: string): TDIReg;
    function RegisterContainer(ABitClass: TClass; ABitInterface: TGuid; const ATilerID: string): TDIReg;
    function RegisterTiler(ATilerClass: TClass; ATilerInterface: TGuid; const ATilerID: string;
      AScaleClass: TClass): TDIReg;

    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReact: TDIReg;
    function RegisterReactFactory: TDIReg;
    function RegisterReactComponent: TDIReg;
    function RegisterReconciliator: TDIReg;

    function RegisterComposite(ACompositeClass: TClass; ACompositeInterface: TGuid;
      AMapStateKeys: array of string): TDIReg;

    procedure RegisterCommon;
  end;


implementation

end.

