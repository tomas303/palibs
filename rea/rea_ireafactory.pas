unit rea_ireafactory;

{$mode objfpc}{$H+}

interface

uses
  rea_iflux, trl_iprops;

type
  IReaFactory = interface
  ['{1EE3D292-C792-42F2-BD5A-3484CC2AAF2B}']
    function NewProps: IProps;
    function NewNotifier(const AActionID: integer): IFluxNotifier;
  end;

implementation

end.

