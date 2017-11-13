unit rea_imaps;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops;

type

  { IMapStateToProps }

  IMapStateToProps = interface
  ['{A311ED5F-8C3C-4751-86AB-E5FCEE278024}']
    function Map(const AProps: IProps): IProps;
  end;

implementation

end.

