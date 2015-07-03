unit uPerson;

interface

type

  { TPerson }

  TPerson = class
  private
    fFirstName: string;
    fSurname: string;
  published
    property FirstName: string read fFirstName write fFirstName;
    property Surname: string read fSurname write fSurname;
  end;

implementation

{ TPerson }


end.

