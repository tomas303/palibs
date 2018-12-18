unit trl_ilink;

{$mode objfpc}{$H+}

interface

type
  ILink = interface
    ['{E6B28442-929C-4952-8790-D1558D14F420}']
    function Next: ILink;
    function Last: ILink;
    function Insert(const ALink: ILink): ILink;
    function Split: ILink;
  end;

implementation

end.

