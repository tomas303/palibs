{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit trl;

interface

uses
  trl_dicontainer, trl_irttibroker, trl_urttibroker, trl_ipersist, 
  trl_upersistxml, trl_ifactory, trl_upersist, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('trl', @Register);
end.
