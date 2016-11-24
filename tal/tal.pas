{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tal;

{$warn 5023 off : no warning about unused units}
interface

uses
  tal_uapp, tal_ilauncher, tal_uguilauncher, tal_iedit, tal_SimpleListForm, 
  tal_uwindowlog, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tal', @Register);
end.
