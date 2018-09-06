unit trl_imetaelementfactory;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, trl_iprops;

type

  { IMetaElementFactory }

  IMetaElementFactory = interface
  ['{64895959-43CF-43E3-A3CE-1EF69608BEBE}']
    function CreateElement(const ATypeGuid: TGuid; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AChildren: array of IMetaElement;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps;
      AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps;
      const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean = false): IMetaElement;
  end;

implementation

end.

