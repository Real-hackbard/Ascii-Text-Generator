unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Common, StdCtrls, ExtCtrls, FIGlet, ComCtrls, LabelLien;

type
  TForm1 = class(TForm)
    memTexte: TMemo;
    memInfosFont: TMemo;
    lstFonts: TListBox;
    memResultat: TMemo;
    btnCopy: TButton;
    trkFontSize: TTrackBar;
    lblFontSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnQuitterClick(Sender: TObject);
    procedure memTexteChange(Sender: TObject);
    procedure lstFontsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetResultat;
    procedure btnCopyClick(Sender: TObject);
    procedure trkFontSizeChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

var
  MyFont: TFIGlet;

{$R *.dfm}

//----------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  Liste: TStringList;
  i: integer;
  Nom: string;
  DefaultFound: boolean;
begin
  MyFont := TFIGlet.Create;
  memResultat.Text  := '';
  memInfosFont.Text  := '';
  trkFontSize.Position := 7;
  lstFonts.Clear;
  DefaultFound := False;
  Liste := TStringList.Create;
  SearchFiles('.', 'flf', True, Liste);
  for i := 0 to Liste.Count - 1 do begin
    Nom := BaseName(Liste[i], '.flf');
    lstFonts.AddItem(Nom, nil);
    if Nom = 'standard' then begin
      lstFonts.Selected[i] := true;
      MyFont.FontFile := Liste[i];
      DefaultFound := True;
    end;
  end;
  if not DefaultFound then begin
    if lstFonts.Count>0 then begin
      lstFonts.Selected[0] := true;
      MyFont.FontFile := Liste[0];
    end;
  end;
  Liste.Free;

  GetResultat;
end;       
//----------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyFont.Free;
end;
//----------------------------------------------------------
procedure TForm1.btnCopyClick(Sender: TObject);
begin
  memResultat.SelectAll;
  memResultat.CopyToClipboard;
  memResultat.SelLength := 0;
end;
//----------------------------------------------------------
procedure TForm1.btnQuitterClick(Sender: TObject);
begin

end;
//----------------------------------------------------------
procedure TForm1.lstFontsClick(Sender: TObject);
begin
  MyFont.FontFile := 'fonts\'+lstFonts.Items[lstFonts.ItemIndex]+'.flf';
  GetResultat;
end;
//----------------------------------------------------------
procedure TForm1.memTexteChange(Sender: TObject);
begin
  GetResultat;
end;
//----------------------------------------------------------
procedure TForm1.trkFontSizeChange(Sender: TObject);
begin
  lblFontSize.Caption := IntToStr(trkFontSize.Position);
  memResultat.Font.Size := trkFontSize.Position;
end;
//----------------------------------------------------------
procedure TForm1.GetResultat;
begin
  memResultat.Text := MyFont.getString(memTexte.Text);
  memInfosFont.Text := MyFont.CommentText;
end;
//----------------------------------------------------------

end.
