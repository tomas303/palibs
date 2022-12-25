unit trl_funcp;

{$mode delphi}{$H+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  sysutils;

type

  TFunc<T, S> = reference to function(const x: T): S;


  { TOptional }

  TOptional<T> = record
  private
    fValue: T;
    fHasValue: Boolean;
  public
    class function New: TOptional<T>; overload; static;
    class function New(const AValue: T): TOptional<T>; overload; static;
    class operator Equal(a, b: TOptional<T>): Boolean;
    procedure Clear;
    function HasValue: Boolean;
    function Value: T;
  end;
  { Optional2 }

  Optional<T, S> = class
  public
    class function Map(f: TFunc<T, S>; x: TOptional<T>): TOptional<S>; overload;
    class function Map(f: TFunc<T, S>): TFunc<TOptional<T>, TOptional<S>>; overload;
    class function Apply(f: TOptional<TFunc<T, S>>; x: TOptional<T>): TOptional<S>; overload;
    class function Apply(f: TOptional<TFunc<T, S>>): TFunc<TOptional<T>, TOptional<S>>; overload;
    class function Bind(f: TFunc<T, TOptional<S>>; x: TOptional<T>): TOptional<S>; overload;
    class function Bind(f: TFunc<T, TOptional<S>>): TFunc<TOptional<T>, TOptional<S>>; overload;
  end;

implementation

{ TOptional }

class function TOptional<T>.New: TOptional<T>;
begin
  Result.fHasValue := False;
end;

class function TOptional<T>.New(const AValue: T): TOptional<T>;
begin
  Result.fValue := AValue;
  Result.fHasValue := True;
end;

class operator TOptional<T>.Equal(a, b: TOptional<T>): Boolean;
begin
  Result := a.fHasValue and b.fHasValue and (a.fValue = b.fValue)
    or
    not a.fHasValue and not b.fHasValue;
end;

procedure TOptional<T>.Clear;
begin
  fValue := Default(T);
  fHasValue := False;
end;

function TOptional<T>.HasValue: Boolean;
begin
  Result := fHasValue;
end;

function TOptional<T>.Value: T;
begin
  if not fHasValue then
    raise Exception.Create('optional type has no value');
  Result := fValue;
end;

{ Optional }

class function Optional<T, S>.Map(f: TFunc<T, S>; x: TOptional<T>): TOptional<S>;
begin
  if x.HasValue then
    Result := TOptional<S>.New(f(x.Value))
  else
    Result := TOptional<S>.New;
end;

class function Optional<T, S>.Map(f: TFunc<T, S>): TFunc<TOptional<T>, TOptional<S>>;
begin
  Result := function(const x: TOptional<T>): TOptional<S>
    begin
      Result := Map(f, x);
    end;
end;

class function Optional<T, S>.Apply(f: TOptional<TFunc<T, S>>; x: TOptional<T>
  ): TOptional<S>;
var
  mfunc: TFunc<T, S>;
begin
  if f.HasValue and x.HasValue then begin
    mfunc := f.Value;
    Result := mfunc(x.Value)
  end
  else
    Result := TOptional<S>.New;
end;

class function Optional<T, S>.Apply(f: TOptional<TFunc<T, S>>): TFunc<TOptional<
  T>, TOptional<S>>;
begin
  Result := function(const x: TOptional<T>): TOptional<S>
    begin
      Result := Apply(f, x);
    end;
end;

class function Optional<T, S>.Bind(f: TFunc<T, TOptional<S>>; x: TOptional<T>): TOptional<S>;
begin
  if x.HasValue then
    Result := f(x.Value)
  else
    Result := TOptional<S>.New;
end;

class function Optional<T, S>.Bind(f: TFunc<T, TOptional<S>>): TFunc<TOptional<T>, TOptional<S>>;
begin
  Result := function(const x: TOptional<T>): TOptional<S>
    begin
      Result := Bind(f, x);
    end;
end;

end.

