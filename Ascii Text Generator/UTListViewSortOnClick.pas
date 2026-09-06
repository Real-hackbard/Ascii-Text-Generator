unit UTListViewSortOnClick;

interface

uses
  ComCtrls, Types;

type
  // Class that handles "on-click" sorting of a TListView
  TListViewSortOnClick = class
  private
    listView : TCustomListView;   // Encapsulated ListView
    triInfo : TPoint;             // info sur le tri : colonne à trier et sens
    colonneImage : integer;       // indicates the index of the column for which sorting should be based on the ImageIndex of the items rather than on the column itself.
    oldColumnClick : TMethod;     // former ColumnClick event of the TListView
  public
    constructor Create(pListView : TCustomListView; pColonneImage : integer = -1);
    destructor Destroy; override;
  published
    // New ColumnClick event for the ListView
    procedure newColumnClick(Sender: TObject; Column: TListColumn);
  end;

implementation

uses
  SysUtils, TypInfo;

{ TListViewSortOnClick } 

// Function used for sorting listview items
// This function is not part of the class; it is in the part
// implementation of the unit
// ParamSort contains the address of a TPoint indicating :
//   X : sorted column (-1 to sort on image/item index)
//   Y : sort order (1=ascending, -1=descending)
function Sort(Item1, Item2: TListItem; ParamSort: integer) : integer; stdcall; 
begin 
  with PPoint(ParamSort)^ do 
  begin 
    case X of 
      // -1 : We sort on the image
      -1 : if Item1.ImageIndex = Item2.ImageIndex then 
             result := 0 
           else if Item1.ImageIndex > Item2.ImageIndex then 
             result := Y // 1 if increasing, -1 if decreasing
           else 
             result := -Y; // -1 if increasing, 1 if decreasing
      // First column: These are the captions
      0 : result := compareStr(Item1.Caption, Item2.Caption) * Y;
      // other columns, comparison of subitems
      else result := compareStr(Item1.SubItems[X - 1], Item2.SubItems[X - 1]) * Y; 
    end; 
  end; 
end; 

constructor TListViewSortOnClick.Create(pListView: TCustomListView; pColonneImage: integer); 
var 
  m : TMethod; 
begin 
  triInfo.X := -2; // because from -1 to n, these are the columns of the ListView (-1: image column)
  triInfo.Y := 1; 
  colonneImage := pColonneImage; 
  listView := pListView; 
  // ColumnClick hijacking
  oldColumnClick := GetMethodProp(listView,'OnColumnClick');
  m.Data := self; 
  m.Code := MethodAddress('newColumnClick'); 
  SetMethodProp(listView,'OnColumnClick',m); 
  // SortType property
  SetEnumProp(listView,'SortType','stData'); 
end; 

destructor TListViewSortOnClick.Destroy; 
begin 
  // ColumnClick event retrieval
  SetMethodProp(listView,'OnColumnClick',oldColumnClick); 
  inherited; 
end; 

procedure TListViewSortOnClick.newColumnClick(Sender: TObject; Column: TListColumn); 
var 
  idxCol : integer; 
begin 
  // Correcting the column index if the column to be sorted was clicked on the image.
  if (Column.Index = colonneImage) then 
    idxCol := -1 
  else 
    idxCol := Column.Index;
  // Click on the column you clicked on previously, and you will reverse the direction.
  if (triInfo.X = idxCol) then 
    triInfo.Y := -triInfo.Y 
  // Clicking on another column returns us to the ascending order.
  else 
  begin 
    triInfo.X := idxCol; 
    triInfo.Y := 1; 
  end; 
  // Call to the sorting function
  listView.CustomSort(@sort,integer(@triInfo)); 
  // Calling the previous event if it was defined
  if assigned(oldColumnClick.Code) then 
    TLVColumnClickEvent(oldColumnClick)(Sender, Column); 
end;

end.
