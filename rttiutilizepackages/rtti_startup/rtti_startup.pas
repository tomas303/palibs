{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rtti_startup;

interface

uses
  uStartForm, iStart, fList, rtti_startup_lib, fGrid, uRBApp, uStart, 
  uStartup_DIContainer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('rtti_startup', @Register);
end.
