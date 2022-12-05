unit rea_iprops;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, rea_udesigncomponentdata, rea_iflux;

type

  { IDCProps }

  IDCProps = interface
  ['{72C96697-1BDC-458F-AFCA-E6C2F2E157C1}']
    function AsIProps: IProps;
    function MMWidth: Integer;
    function SetMMWidth(AValue: Integer): IDCProps;
    function MMHeight: Integer;
    function SetMMHeight(AValue: Integer): IDCProps;
    function Place: Integer;
    function SetPlace(AValue: Integer): IDCProps;
    function Color: Integer;
    function SetColor(AValue: Integer): IDCProps;
  end;

  { IDCLabelEditProps }

  IDCLabelEditProps = interface(IDCProps)
  ['{A505950D-6C78-47C4-A266-A9F8243CEB28}']
    function Data: TEditData;
    function SetData(const AValue: TEditData): IDCLabelEditProps;
    function Caption: String;
    function SetCaption(const AValue: String): IDCLabelEditProps;
    function CaptionEdge: Integer;
    function SetCaptionEdge(AValue: Integer): IDCLabelEditProps;
    function CaptionHeight: Integer;
    function SetCaptionHeight(AValue: Integer): IDCLabelEditProps;
  end;

  { IDCButtonProps }

  IDCButtonProps = interface(IDCProps)
  ['{815878D5-6102-4A92-A8C5-87F059CEF92A}']
    function Text: String;
    function SetText(const AValue: String): IDCButtonProps;
    function FontDirection: Integer;
    function SetFontDirection(const AValue: Integer): IDCButtonProps;
    function Border: Integer;
    function SetBorder(const AValue: Integer): IDCButtonProps;
    function BorderColor: Integer;
    function SetBorderColor(const AValue: Integer): IDCButtonProps;
  end;

implementation

end.

