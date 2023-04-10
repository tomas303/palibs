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
unit trl_ipersist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  Classes, SysUtils, trl_irttibroker, trl_urttibroker, trl_usystem, fgl, trl_pubsub;

const
  cMemoStringType = 'TMemoString';
  cIDStringType = 'TIDString';

type
  TMemoString = type string;
  TIDString = type string;

  { IPersistStoreDevice }

  IPersistStoreDevice = interface
  ['{32674407-3D99-4BF9-8BBE-99DABA186655}']
    function Select2(const AClass: string): IRBDataEnumerable;
    procedure Save2(const AData: IRBData);
    procedure Delete2(const AData: IRBData);
    procedure Open; overload;
    procedure Open(const AFile: string); overload;
    procedure Open(const AStream: TStream); overload;
    procedure Close; overload;
    procedure Close(const AStream: TStream); overload;
    procedure Flush;
    function IsOpened: Boolean;
  end;

  { IPersistFactory }

  IPersistFactory = interface
  ['{66F2248D-87D0-49D8-ACBF-DA19CC862A11}']
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;

  { PersistIDAttribute }

  PersistIDAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  { PersistAUTOAttribute }

  PersistAUTOAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TPersistPredicate = reference to function(const x: IRBData): Boolean;

  TPersistAction = (paNew, paChange, paDelete);

  { TPersistInfo }

  TPersistInfo = record
  private
    fData: IRBData;
    fAction: TPersistAction;
  public
    constructor Create(const AData: IRBData; AAction: TPersistAction);
    class operator equal(a,b: TPersistInfo): Boolean;
    property Data: IRBData read fData;
    property Action: TPersistAction read fAction;
  end;

  IPSPersistChannel = IPubSubDataChannel<TPersistInfo>;

  { IMiniDataList }

  IMiniList = interface
  ['{EEA85C61-F9C5-493E-8034-C7983355D906}']
    function GetCount: Integer;
    function GetField(AIndex: Integer; const AName: String): String;
    procedure SetField(AIndex: Integer; const AName: String; AValue: String);
    function GetList(AIndex: Integer; const AName: String): IMiniList;
    procedure SetList(AIndex: Integer; const AName: String; const AValue: IMiniList);
    function GetData(AIndex: Integer): IRBData;
    function Insert(APos: Integer): IRBData; overload;
    function Append: IRBData; overload;
    procedure Delete(APos: Integer);
    property Count: Integer read GetCount;
    property Field[AIndex: Integer; const AName: String]: String read GetField write SetField;
    property List[AIndex: Integer; const AName: String]: IMiniList read GetList write SetList;
    property Data[AIndex: Integer]: IRBData read GetData;
    function GetEnumerator: IRBDataEnumerator;
    function PSPersistChannel: IPSPersistChannel;
  end;

  IMiniList<T: TObject> = interface(IMiniList)
  ['{2F401415-D81E-41D0-83AB-063BFC4E5261}']
  end;


  IMiniDataList<T: TObject> = interface
  ['{042B2A5C-4450-4A70-A1E6-7A3020F79B0E}']
    function NewList: IMiniList<T>; overload;
    function NewList(const APredicate: TPersistPredicate): IMiniList<T>; overload;
    function GetEnumerator: IRBDataEnumerator;
    procedure Load;
    procedure Save;
  end;

function FilterInsensitiveContains(const ALowerText: String; const x: IRBData): Boolean;

implementation

function FilterInsensitiveContains(const ALowerText: String; const x: IRBData): Boolean;
var
  i: Integer;
  mList: IMiniList;
  mData: IRBData;
begin
  Result := False;
  for i := 0 to x.Count - 1 do begin
    if x.Items[i].IsObject then begin
      Result := FilterInsensitiveContains(ALowerText, TRBData.Create(x.Items[i].AsObject));
    end
    else if x.Items[i].IsInterface then begin
      if Supports(x.Items[i].AsInterface, IMiniList, mList) then begin
        for mData in mList do begin
          Result := FilterInsensitiveContains(ALowerText, mData);
          if Result then
            Break;
        end;
      end;
    end
    else begin
      Result := Pos(ALowerText, x.Items[i].AsString.ToLower) > 0;
    end;
    if Result then
      Break;
  end;
end;

{ PersistAUTOAttribute }

constructor PersistAUTOAttribute.Create;
begin

end;

{ PersistIDAttribute }

constructor PersistIDAttribute.Create;
begin

end;

{ TPersistInfo }

constructor TPersistInfo.Create(const AData: IRBData; AAction: TPersistAction);
begin
  fData := AData;
  fAction := AAction;
end;

class operator TPersistInfo.equal(a, b: TPersistInfo): Boolean;
begin
  Result := (a.fData = b.fData) and (a.fAction = b.fAction);
end;

end.

