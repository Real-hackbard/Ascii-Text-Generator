unit THashTable;

interface

uses
  Classes, contnrs ;

type
  HashTable = class(TStringList)
  private
    MyStrings : TStrings;
    procedure SetInteger(AIndex : string; AValue : integer);
    function GetInteger(AIndex : string) : integer;
    procedure SetString(AIndex : string; AValue : string);
    function GetString(AIndex : string) : string;
  public
    function AddInteger(const AString : string; const AValue : integer) : integer;
    property Integers[AIndex : string] : integer
      read GetInteger
      write SetInteger;
    function AddString(const AString : string; const AValue : string) : integer;
    property Strings[AIndex : string] : string
      read GetString
      write SetString;
  end;

implementation
//---------------------------------------------------------------
procedure HashTable.SetInteger(AIndex : string; AValue : integer);
begin
  Objects[IndexOf(AIndex)] := TObject(AValue);
end;
//---------------------------------------------------------------
function HashTable.GetInteger(AIndex : string) : integer;
begin
  Result := integer(Objects[IndexOf(AIndex)]);
end;
//---------------------------------------------------------------
function HashTable.AddInteger(const AString : string; const AValue : integer) : integer;
begin
  Result := AddObject(AString,TObject(AValue));
end; 
//---------------------------------------------------------------
procedure HashTable.SetString(AIndex : string; AValue : string);
begin
  Objects[IndexOf(AIndex)] := TObject(AValue);
end;
//---------------------------------------------------------------
function HashTable.GetString(AIndex : string) : string;
begin
  Result := string(Objects[IndexOf(AIndex)]);
end;
//---------------------------------------------------------------
function HashTable.AddString(const AString : string; const AValue : string) : integer;
begin

  Result := AddObject(AString,TObject(AValue));
end;


end.
