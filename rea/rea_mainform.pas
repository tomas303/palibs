unit rea_mainform;

{$mode objfpc}{$H+}

interface

uses
  Forms;

type

  { TMainForm }

  // just allow create mainform through constructor - unfortunately setup mainform
  // is not so easy ... to solve it, some custom Application will be needed
  TMainForm = class(TForm)
  public
    class function newinstance: tobject; override;
  end;

implementation

{ TMainForm }

class function TMainForm.newinstance: tobject;
begin
  Application.CreateForm(TForm, Result);
end;

end.

