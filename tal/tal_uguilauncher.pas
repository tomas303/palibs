(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit tal_uguilauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_ilauncher, Forms, tvl_imainform;

type

  { TGUILauncher }

  TGUILauncher = class(TInterfacedObject, ILauncher)
  private
    fMainForm: IMainForm;
  protected
    // ILauncher
    procedure Launch;
  protected
    procedure BeforeLaunch; virtual;
    procedure AfterLaunch; virtual;
  public
    procedure AfterConstruction; override;
  published
    property MainForm: IMainForm read fMainForm write fMainForm;
  end;

implementation

{ TGUILauncher }

procedure TGUILauncher.Launch;
begin
  BeforeLaunch;
  try
    MainForm.StartUp;
    try
      Application.Run;
    finally
      MainForm.ShutDown;
    end;
  finally
    AfterLaunch;
  end;
end;

procedure TGUILauncher.BeforeLaunch;
begin

end;

procedure TGUILauncher.AfterLaunch;
begin

end;

procedure TGUILauncher.AfterConstruction;
begin
  inherited AfterConstruction;
  // since mainform could be subject of dependency injection and as such could
  // be created during create of hierarchy of object, call to inititialization
  // is put here(at least on windows must be called before main form is created)
  Application.Initialize;
end;

end.

