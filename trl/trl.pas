{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit trl;

interface

uses
  trl_dicontainer, trl_irttibroker, trl_urttibroker, trl_ipersist, 
  trl_upersistxml, trl_ifactory, trl_upersist, trl_upersiststore, 
  trl_icryptic, trl_processrunner, trl_idifactory, trl_udifactory, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('trl', @Register);
end.
