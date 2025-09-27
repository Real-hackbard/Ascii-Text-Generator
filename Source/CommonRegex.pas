unit CommonRegex;

interface

uses
  SysUtils, Classes, Math, Graphics, ShellAPI, Windows, Dialogs,
  Controls, Forms, ComCtrls, ExtCtrls, StdCtrls, FileCtrl, StrUtils,
  Mask, Common, VBScriptRegex ;

type
  TSList = class(TList)
  protected
    function Get(Index: Integer): TStringList;
    procedure Put(Index: Integer; Item: TStringList);
  public
    function Add(Item: TStringList): Integer;
    procedure Delete(Index: Integer);
    function First: TStringList;
    procedure Insert(Index: Integer; Item: TStringList);
    function Last: TStringList;
    property Items[Index: Integer]: TStringList read Get write Put; default;
  end;

function Match(Text, Pattern:string): boolean;
function Match_hhmmss(Text:string): boolean;
function Match_email(Text:string): boolean;
function Match_url(Text:string): boolean;
function ValidRegex(Sender: TObject; Pattern:string) : boolean;
function RegReplace(Text, Pattern, Replace:string): string;
function RegReplaceI(Text, Pattern, Replace:string): string;
function GetMatch(Text, Pattern:string): TSList;
procedure ValidFloat(Sender: TObject); overload;

const
  PATTERN_EMAIL = '^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]{2,}[.][a-zA-Z]{2,3}$';
  PATTERN_URL = '^(ftp|http|https):\/\/(\w+:{0,1}\w*@)?(\S+)(:[0-9]+)?(\/|\/([\w#!:.?+=&%@!\-\/]))?$';
  PATTERN_HHMMSS = '^[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$';

implementation  
//--------------------------------------------------------------------
function TSLIst.Get(Index: Integer): TStringList;
begin
  Result := TStringList(inherited Get(Index));
end;        
//--------------------------------------------------------------------
procedure TSLIst.Put(Index: Integer; Item: TStringList);
begin
  inherited Put(Index, Item);
end;
//--------------------------------------------------------------------
function TSLIst.Add(Item: TStringList): Integer;
begin
  Result := inherited Add(Item);
end;
//--------------------------------------------------------------------
procedure TSLIst.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;
//--------------------------------------------------------------------
function TSLIst.First: TStringList;
begin
  Result := inherited First;
end;
//--------------------------------------------------------------------
procedure TSLIst.Insert(Index: Integer; Item: TStringList);
begin
  inherited Insert(Index, Item);
end;
//--------------------------------------------------------------------
function TSLIst.Last: TStringList;
begin
  Result := inherited Last;
end;
//--------------------------------------------------------------------
procedure ValidFloat(Sender: TObject); overload;
var
  Value, OldValue, v1, v2: string;
  m: TSList;
begin
  OldValue := ValidGetValue(Sender, '0');
  Value := StrReplace(OldValue, ',', '.');
  if OldValue = '' then Value := '0';
  Value := RegReplaceI(Value, '[^0-9,\.]', '');
  m := GetMatch(Value, '^([0-9]*)(?:\.([0-9]*))?');
  if m.Count>0 then begin
    if m.Items[0].Count=2 then begin
      v1 := m.Items[0][0];
      v2 := m.Items[0][1];
      v1 := RegReplaceI(v1, '^0+', ''); 
      v2 := RegReplaceI(v2, '0+$', '');
      if v1 = '' then v1 := '0';
      if v2 = '' then v2 := '0';
      Value := v1+'.'+v2;
    end;
  end;
  m.Free;
  ValidSetValue(Sender, OldValue, Value);
end;
//--------------------------------------------------------------------
function ValidRegex(Sender: TObject; Pattern:string) : boolean;
var
  SenderClass: string;
  Text : string;
begin
  SenderClass := Sender.ClassName;
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
    0: Text := TComboBox(Sender).Text;
    1: Text := TEdit(Sender).Text;
  end;
  Result := Match(Text, Pattern);
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
    0: if Result then TComboBox(Sender).Font.Color := clWindowText
                 else TComboBox(Sender).Font.Color := clRed;
    1: if Result then TEdit(Sender).Font.Color := clWindowText
                 else TEdit(Sender).Font.Color := clRed;
  end;
end; 
//--------------------------------------------------------------------
function GetMatch(Text, Pattern:string): TSList;
var
  re: IRegex;
  mc: IMatchCollection;
  sub: ISubMatches;
  i, iMatch: integer;
  list: TStringList;
begin
  Result := TSList.Create;
  re := VBScriptRegex.Regex.Create(Pattern);
  mc := re.FindAll(Text);
  for iMatch := 0 to mc.Count - 1 do begin
    sub := mc.Item[iMatch].SubMatches;
    list := TStringList.Create;
    for i := 0 to sub.Count - 1 do begin  
      List.Add(sub.Item[i]);
    end;
    Result.Add(List);
    //List.Free;
  end;
end;
//--------------------------------------------------------------------
function Match(Text, Pattern:string): boolean;
var
  re: IRegex;
begin
  re := VBScriptRegex.Regex.Create(Pattern);
  Result := re.Match(Text);
end;
//--------------------------------------------------------------------
function RegReplace(Text, Pattern, Replace:string): string;
var
  re: IRegex;
begin
  re := VBScriptRegex.Regex.Create(Pattern, [reMultiLine]);
  Result := re.ReplaceAll(Text, Replace);
end;       
//--------------------------------------------------------------------
function RegReplaceI(Text, Pattern, Replace:string): string;
var
  re: IRegex;
begin
  re := VBScriptRegex.Regex.Create(Pattern, [reMultiLine, reIgnoreCase]);
  Result := re.ReplaceAll(Text, Replace);
end;
//--------------------------------------------------------------------
function Match_hhmmss(Text:string): boolean;
begin
  Result := Match(Text, PATTERN_HHMMSS);
end;
//--------------------------------------------------------------------
function Match_email(Text:string): boolean;
begin
  Result := Match(Text, PATTERN_EMAIL);
end;                 
//--------------------------------------------------------------------
function Match_url(Text:string): boolean;
begin
  Result := Match(Text, PATTERN_URL);
end;
//--------------------------------------------------------------------

end.
