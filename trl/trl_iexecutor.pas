unit trl_iExecutor;

{$mode objfpc}{$H+}

interface


type

  { IExecute }

  IExecute = interface
  ['{7E959070-295B-46B1-9FE2-74AF4259A1AC}']
    procedure Execute;
  end;

  { IExecutor }

  IExecutor = interface
  ['{53D77BBD-5E38-4E0C-B2A6-3F7AA098A158}']
    procedure Add(const AExecute: IExecute);
    procedure ExecuteAll;
  end;

implementation

end.

