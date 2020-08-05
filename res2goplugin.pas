{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit res2goplugin;

{$warn 5023 off : no warning about unused units}
interface

uses
  res2gomain, res2goOptions, usupports, ugolang, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('res2gomain', @res2gomain.Register);
end;

initialization
  RegisterPackage('res2goplugin', @Register);
end.
