unit rea_ibits;

{$mode objfpc}{$H+}

interface

uses
  Controls, Graphics;

type
  // wrapper for real control and its binder
  IBit = interface
  ['{479784FA-9E6B-4826-BCFE-92A676B2F7DD}']
    procedure Render;
    procedure RenderPaint(const ACanvas: TCanvas);
    procedure HookParent(const AParent: TWinControl);
  end;

  IFormBit = interface
  ['{64AAE7AE-AC24-4CBA-8430-2885CFC396AC}']
  end;

  IStripBit = interface
  ['{354C99E0-E2B2-43CF-89E7-95057DF390F0}']
  end;

  IEditBit = interface
  ['{0392A25E-1D90-4F37-9279-09F32D6F7D03}']
  end;

  ITextBit = interface
   ['{4178C1EC-AA39-4429-B48C-7058676ABA7B}']
  end;

  IButtonBit = interface
   ['{7FB3194B-62AB-44B7-8317-603D10706C71}']
  end;

implementation

end.

