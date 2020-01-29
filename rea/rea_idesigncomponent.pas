unit rea_idesigncomponent;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement;

type
  IDesignComponent = interface
  ['{AD83F143-4C0A-4703-A38A-E3F175036FE6}']
    function Compose(const AParentElement: IMetaElement): IMetaElement;
  end;

  IDesignComponentApp = interface(IDesignComponent)
  ['{4035F57F-CA74-4D0E-8972-3A2162FAB714}']
  end;

  IDesignComponentForm = interface(IDesignComponent)
  ['{AE80667B-FA5E-432F-9667-402165481946}']
  end;

  IDesignComponentEdit = interface(IDesignComponent)
  ['{BBF9AEB9-28A5-4E15-840D-3483885936E1}']
  end;

  IDesignComponentButton = interface(IDesignComponent)
  ['{9EFB4F6C-F20C-4234-BE3C-C6F1F89A9314}']
  end;

  IDesignComponentHeader = interface(IDesignComponent)
  ['{B16AAF18-076F-449D-82B1-BB6AE4962B4B}']
  end;

implementation

end.

