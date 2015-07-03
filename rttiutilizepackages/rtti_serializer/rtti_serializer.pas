{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rtti_serializer;

interface

uses
  rtti_serializer_uFactory, rtti_serializer_uManager, 
  rtti_serializer_uMoniker, rtti_serializer_uXmlStore, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('rtti_serializer', @Register);
end.
