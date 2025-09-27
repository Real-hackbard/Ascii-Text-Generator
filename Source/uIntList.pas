unit uIntList;

interface

uses
  Classes;

type
  TIntegerListSortCompare = function (Item1, Item2: Pointer): Integer;
  TIntegerList = class(TList)
  private
    function Get(Index: Integer): Integer; reintroduce;
    procedure Put(Index: Integer; const Value: Integer); reintroduce;
  public
    function Add(Item: Integer): Integer; reintroduce;
    function Extract(Item: Integer): Integer; reintroduce;
    function IndexOf(Item: Integer): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    function Last: Integer; reintroduce;
    function Remove(Item: Integer): Integer; reintroduce;
    procedure Sort(Compare: TIntegerListSortCompare); reintroduce;
    property Items[Index: Integer]: Integer read Get write Put; default; 
  end;

implementation

{ TIntegerList }

function TIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TIntegerList.Extract(Item: Integer): Integer;
begin
  Result := Integer(inherited Extract(Pointer(Item)));
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin

end;

procedure TIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, Pointer(Item));
end;

function TIntegerList.Last: Integer;
begin
  Result := Integer(inherited Last);
end;

procedure TIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function TIntegerList.Remove(Item: Integer): Integer;
begin
  Result := inherited Remove(Pointer(Item));
end;

procedure TIntegerList.Sort(Compare: TIntegerListSortCompare);
begin
  inherited Sort(TListSortCompare(Compare));
end;

end.
