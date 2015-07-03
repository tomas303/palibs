unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, iMain, rtti_idebinder_iBindings, uPerson,
  rtti_idebinder_Lib, rtti_broker_uData;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FirstName: TEdit;
    SurName: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fMC: IMainContext;
    fPerson: TPerson;
    fPersonB: IRBDataBinder;
  public
    procedure AttachMainContext(const AMainContext: IMainContext);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  fPerson := TPerson.Create;

  fPerson.FirstName := 'Radka';
  fPerson.Surname := 'Vondrackova';

  fPersonB := TLib.NewDataBinder;
  fPersonB.Bind(Panel1, TRBData.Create(fPerson));
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  edit1.Text := fPerson.FirstName;
  edit2.Text := fPerson.Surname;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.AttachMainContext(const AMainContext: IMainContext);
begin
  fMC := AMainContext;
end;

end.

