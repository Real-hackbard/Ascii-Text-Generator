unit UTListViewSortOnClick;

interface

uses
  ComCtrls, Types;

type
  // Classe qui gère le tri "sur click" d'un TListView
  TListViewSortOnClick = class
  private
    listView : TCustomListView;   // ListView encapsulé
    triInfo : TPoint;             // info sur le tri : colonne à trier et sens
    colonneImage : integer;       // indique l'index de la colonne pour laquelle il faut trier sur l'ImageIndex des items plutôt que sur la colonne elle-même
    oldColumnClick : TMethod;     // ancien événement ColumnClick du TListView
  public
    constructor Create(pListView : TCustomListView; pColonneImage : integer = -1);
    destructor Destroy; override;
  published
    // Nouvel événement ColumnClick pour le ListView
    procedure newColumnClick(Sender: TObject; Column: TListColumn);
  end;

implementation

uses
  SysUtils, TypInfo;

{ TListViewSortOnClick } 

// Fonction utilisée pour le tri des items du listview 
// Cette fonction ne fait partie de la classe, elle est dans la partie 
// implementation de l'unité 
// ParamSort contient l'adresse d'un TPoint indiquant : 
//   X : colonne triée (-1 pour trier sur l'imageIndex de l'item) 
//   Y : sens du tri (1=croissant, -1=décroissant) 
function Sort(Item1, Item2: TListItem; ParamSort: integer) : integer; stdcall; 
begin 
  with PPoint(ParamSort)^ do 
  begin 
    case X of 
      // -1 : On trie sur l'image 
      -1 : if Item1.ImageIndex = Item2.ImageIndex then 
             result := 0 
           else if Item1.ImageIndex > Item2.ImageIndex then 
             result := Y // 1 si croissant, -1 si décroissant 
           else 
             result := -Y; // -1 si croissant, 1 si décroissant 
      // Première colonne : Il s'agit des captions 
      0 : result := compareStr(Item1.Caption, Item2.Caption) * Y;
      // autres colonnes, comparaison des subitems 
      else result := compareStr(Item1.SubItems[X - 1], Item2.SubItems[X - 1]) * Y; 
    end; 
  end; 
end; 

constructor TListViewSortOnClick.Create(pListView: TCustomListView; pColonneImage: integer); 
var 
  m : TMethod; 
begin 
  triInfo.X := -2; // car de -1 à n, il s'agit des colonnes du ListView (-1 : colonne image) 
  triInfo.Y := 1; 
  colonneImage := pColonneImage; 
  listView := pListView; 
  // Détournement de ColumnClick 
  oldColumnClick := GetMethodProp(listView,'OnColumnClick');
  m.Data := self; 
  m.Code := MethodAddress('newColumnClick'); 
  SetMethodProp(listView,'OnColumnClick',m); 
  // Propriété SortType 
  SetEnumProp(listView,'SortType','stData'); 
end; 

destructor TListViewSortOnClick.Destroy; 
begin 
  // Restitution événement ColumnClick 
  SetMethodProp(listView,'OnColumnClick',oldColumnClick); 
  inherited; 
end; 

procedure TListViewSortOnClick.newColumnClick(Sender: TObject; Column: TListColumn); 
var 
  idxCol : integer; 
begin 
  // Correction de l'indice de la colonne si on a cliqué sur la colonne à trier sur l'image 
  if (Column.Index = colonneImage) then 
    idxCol := -1 
  else 
    idxCol := Column.Index; 
  // Click sur la colonne que l'on avait cliquée, on inverse le sens 
  if (triInfo.X = idxCol) then 
    triInfo.Y := -triInfo.Y 
  // Click sur une autre colonne, on revient au sens croissant 
  else 
  begin 
    triInfo.X := idxCol; 
    triInfo.Y := 1; 
  end; 
  // Appel de la fonction tri 
  listView.CustomSort(@sort,integer(@triInfo)); 
  // Appel de l'ancien événement s'il était défini 
  if assigned(oldColumnClick.Code) then 
    TLVColumnClickEvent(oldColumnClick)(Sender, Column); 
end;

end.
