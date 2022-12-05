unit rea_uprops;

{$mode objfpc}{$H+}

interface

uses
  rea_iprops, trl_iprops, trl_uprops, rea_idesigncomponent, rea_udesigncomponentdata,
  rea_iflux;

type

  { TDCProps }

  TDCProps = class(TProps, IDCProps)
  private
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

  { TDCLabelEditProps }

  TDCLabelEditProps = class(TDCProps, IDCLabelEditProps)
  private
    function Data: TEditData;
    function SetData(const AValue: TEditData): IDCLabelEditProps;
    function Caption: String;
    function SetCaption(const AValue: String): IDCLabelEditProps;
    function CaptionEdge: Integer;
    function SetCaptionEdge(AValue: Integer): IDCLabelEditProps;
    function CaptionHeight: Integer;
    function SetCaptionHeight(AValue: Integer): IDCLabelEditProps;
  end;

  { TDCButtonProps }

  TDCButtonProps = class(TDCProps, IDCButtonProps)
  private
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

{ TDCButtonProps }

function TDCButtonProps.Text: String;
begin
  Result := AsStr(cProps.Text);
end;

function TDCButtonProps.SetText(const AValue: String): IDCButtonProps;
begin
  Result := SetStr(cProps.Text, AValue) as IDCButtonProps;
end;

function TDCButtonProps.FontDirection: Integer;
begin
  Result := AsInt(cProps.FontDirection);
end;

function TDCButtonProps.SetFontDirection(const AValue: Integer): IDCButtonProps;
begin
  Result := SetInt(cProps.FontDirection, AValue) as IDCButtonProps;
end;

function TDCButtonProps.Border: Integer;
begin
  Result := AsInt(cProps.Border);
end;

function TDCButtonProps.SetBorder(const AValue: Integer): IDCButtonProps;
begin
  Result := SetInt(cProps.Border, AValue) as IDCButtonProps;
end;

function TDCButtonProps.BorderColor: Integer;
begin
  Result := AsInt(cProps.BorderColor);
end;

function TDCButtonProps.SetBorderColor(const AValue: Integer): IDCButtonProps;
begin
  Result := SetInt(cProps.BorderColor, AValue) as IDCButtonProps;
end;

{ TDCLabelEditProps }

function TDCLabelEditProps.Data: TEditData;
begin
  Result := AsObject(cProps.Data) as TEditData;
end;

function TDCLabelEditProps.SetData(const AValue: TEditData): IDCLabelEditProps;
begin
  Result := SetObject(cProps.Data, AValue) as IDCLabelEditProps;
end;

function TDCLabelEditProps.Caption: String;
begin
  Result := AsStr(cProps.Caption)
end;

function TDCLabelEditProps.SetCaption(const AValue: String): IDCLabelEditProps;
begin
  Result := SetStr(cProps.Caption, AValue) as IDCLabelEditProps;
end;

function TDCLabelEditProps.CaptionEdge: Integer;
begin
  Result := AsInt(cProps.CaptionEdge);
end;

function TDCLabelEditProps.SetCaptionEdge(AValue: Integer): IDCLabelEditProps;
begin
  Result := SetInt(cProps.CaptionEdge, AValue) as IDCLabelEditProps;
end;

function TDCLabelEditProps.CaptionHeight: Integer;
begin
  Result := AsInt(cProps.CaptionHeight);
end;

function TDCLabelEditProps.SetCaptionHeight(AValue: Integer): IDCLabelEditProps;
begin
  Result := SetInt(cProps.CaptionHeight, AValue) as IDCLabelEditProps;
end;

{ TDCProps }

function TDCProps.AsIProps: IProps;
begin
  Result := Self as IProps;
end;

function TDCProps.MMWidth: Integer;
begin
  Result := AsInt(cProps.MMWidth);
end;

function TDCProps.SetMMWidth(AValue: Integer): IDCProps;
begin
  SetInt(cProps.MMWidth, AValue);
  Result := Self;
end;

function TDCProps.MMHeight: Integer;
begin
  Result := AsInt(cProps.MMHeight);
end;

function TDCProps.SetMMHeight(AValue: Integer): IDCProps;
begin
  SetInt(cProps.MMHeight, AValue);
  Result := Self;
end;

function TDCProps.Place: Integer;
begin
  Result := AsInt(cProps.Place);
end;

function TDCProps.SetPlace(AValue: Integer): IDCProps;
begin
  SetInt(cProps.Place, AValue);
  Result := Self;
end;

function TDCProps.Color: Integer;
begin
  Result := AsInt(cProps.Color);
end;

function TDCProps.SetColor(AValue: Integer): IDCProps;
begin
  SetInt(cProps.Color, AValue);
  Result := Self;
end;

end.

