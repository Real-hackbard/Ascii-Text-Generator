unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Common, StdCtrls, ExtCtrls, FIGlet, ComCtrls, LabelLien, Vcl.XPMan,
  Vcl.Samples.Spin, Jpeg, PngImage, GifImg;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    memTexte: TMemo;
    memInfosFont: TMemo;
    Panel3: TPanel;
    btnCopy: TButton;
    Button1: TButton;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    Button2: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    Shape1: TShape;
    Button4: TButton;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure memTexteChange(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetResultat;
    procedure btnCopyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button4Click(Sender: TObject);
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
procedure BitmapFileToPNG(const Source, Dest: String);
var
  Bitmap: TBitmap;
  PNG: TPNGObject;
begin
  Bitmap := TBitmap.Create;
  PNG := TPNGObject.Create;
  {In case something goes wrong, free booth Bitmap and PNG}
  try
    Bitmap.LoadFromFile(Source);
     //Convert data into png
    PNG.Assign(Bitmap);

    if Form1.CheckBox2.Checked = true then
    begin
        // Transparent Color
        PNG.TransparentColor := clBlack;
        // Activate Transparent Png Image
        PNG.Transparent := true;
    end;

    PNG.SaveToFile(Dest);
  finally
    Bitmap.Free;
    PNG.Free;
  end
end;

function InvertBitmap(MyBitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ByteArray: PByteArray;
begin
  MyBitmap.PixelFormat := pf24Bit;
  for y := 0 to MyBitmap.Height - 1 do
  begin
    ByteArray := MyBitmap.ScanLine[y];
    for x := 0 to MyBitmap.Width * 3 - 1 do
    begin
      ByteArray[x] := 255 - ByteArray[x];
    end;
  end;
  Result := MyBitmap;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  bmp : TBitmap;
  Jpg: TJPEGImage;
  GIF : TGIFImage;
  TextRect : TRect;
  lcr : integer;
  Image : TImage;
begin
  bmp := TBitmap.Create;
  //bmp.Canvas.Brush.Color:=clBlack;
  bmp.Canvas.Rectangle(0,0,bmp.Width,bmp.Height);
  //bmp.Canvas.Draw(0,0,Image1.Picture.Graphic);
  bmp.Canvas.Brush.Style := bsClear;
  bmp.Canvas.Brush.Color := Shape1.Brush.Color;
  bmp.Canvas.Font := Memo1.Font;
  lcr := 0;

  // Align the Text into the Bitmap
  if Memo1.Alignment = taLeftJustify then lcr := DT_LEFT;
  if Memo1.Alignment = taCenter then lcr := DT_CENTER;
  if Memo1.Alignment = taRightJustify then lcr := DT_RIGHT;

  TextRect.Left := 0;
  TextRect.Top := 0;

  // Draw Memo Text to Bitmap
  DrawText(bmp.Canvas.Handle,PChar(Memo1.Lines.Text), length(Memo1.Lines.Text),
           TextRect, DT_CALCRECT or DT_NOPREFIX or lcr);

  bmp.Width :=  TextRect.Right;
  bmp.Height := TextRect.Bottom;

  // If text is bigger than image then exit
  if (TextRect.Right > bmp.Width) or
     (TextRect.Bottom > bmp.Height) then begin
    ShowMessage('Text too large for image');
    bmp.Free;
    Exit;
  end;

  try
    // Put the text in the center of the bmp
    OffsetRect(TextRect,(bmp.Width - TextRect.Right) Div 2,
                        (bmp.Height - TextRect.Bottom) Div 2);
    // Draw Text
    DrawText(bmp.Canvas.Handle,PWideChar(Memo1.Lines.Text),
        length(Memo1.Lines.Text),TextRect,
        DT_NOPREFIX or lcr);

    // Invert Bitmap
    if CheckBox1.Checked = true then InvertBitmap(bmp);

    // Create Images
    if SaveDialog1.Execute then begin
      // Create Bitmap Image
      if SaveDialog1.FilterIndex = 1 then begin
        Image := TImage.Create(self);
        Image.Picture.Bitmap.Assign(bmp);
        Image.Picture.Bitmap.SaveToFile(SaveDialog1.FileName + '.bmp');
      end;

      // Create Jpg Image
      if SaveDialog1.FilterIndex = 2 then begin
        Jpg := TJPEGImage.Create;
        Image := TImage.Create(self);
        Image.Picture.Bitmap.Assign(bmp);
        try
          // Copy Bitmap Pixel to Jpg
          Jpg.Assign(Bmp);
          Jpg.SaveToFile(SaveDialog1.FileName + '.jpg');
        finally
        Jpg.Free;
        end;
      end;

      // Create Png Image
      if SaveDialog1.FilterIndex = 3 then begin
        Image := TImage.Create(self);
        Image.Picture.Bitmap.Assign(bmp);
        try
          // Create a Bitmap to Convert in Png
          Image.Picture.Bitmap.SaveToFile(ExtractFilePath(Application.ExeName) + '_png');
          // Convert Bitmap to Png
          BitmapFileToPNG(ExtractFilePath(Application.ExeName) + '_png',
                          SaveDialog1.FileName + '.png');
          finally
          // No Data
          end;
      end;

      // Create Gif Image
      if SaveDialog1.FilterIndex = 4 then begin
        Image := TImage.Create(self);
        Image.Picture.Bitmap.Assign(bmp);
        GIF := TGIFImage.Create;
        try
          // Copy Bitmap Pixel to GIF Data
          GIF.Assign(bmp);
          // Create GIF File Image
          GIF.SaveToFile(SaveDialog1.FileName + '.gif')
          finally
          GIF.Free;
          end;
      end;

    end;
  finally
    bmp.Free;
    Image.Free;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Text := StringReplace(Memo1.Text,'#','',[rfReplaceAll])
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    Memo1.Font := FontDialog1.Font;
    StatusBar1.Panels[5].Text := FontDialog1.Font.Name;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Liste: TStringList;
  i: integer;
  Nom: string;
  DefaultFound: boolean;
begin
  MyFont := TFIGlet.Create;
  Memo1.Text  := '';
  memInfosFont.Text  := '';
  SpinEdit1.Value := 8;
  ListBox1.Clear;
  DefaultFound := False;
  Liste := TStringList.Create;
  SearchFiles('.', 'flf', True, Liste);

  for i := 0 to Liste.Count - 1 do begin
    Nom := BaseName(Liste[i], '.flf');
    ListBox1.AddItem(Nom, nil);
    if Nom = 'standard' then begin
      ListBox1.Selected[i] := true;
      MyFont.FontFile := Liste[i];
      DefaultFound := True;
    end;
  end;

  if not DefaultFound then begin
    if ListBox1.Count>0 then begin
      ListBox1.Selected[0] := true;
      MyFont.FontFile := Liste[0];
    end;
  end;

  Liste.Free;
  StatusBar1.Panels[1].Text := IntToStr(ListBox1.Items.Count);
  GetResultat;
end;       

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyFont.Free;
end;

procedure TForm1.btnCopyClick(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.SelLength := 0;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  MyFont.FontFile := 'fonts\'+ListBox1.Items[ListBox1.ItemIndex]+'.flf';
  StatusBar1.Panels[3].Text := ListBox1.Items.Strings[ListBox1.ItemIndex];
  GetResultat;
end;

procedure TForm1.memTexteChange(Sender: TObject);
begin
  GetResultat;
end;

procedure TForm1.Shape1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog1.Execute then begin
  Memo1.Color := ColorDialog1.Color;
  Shape1.Brush.Color := ColorDialog1.Color;
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  Memo1.Font.Size := SpinEdit1.Value;
end;

procedure TForm1.GetResultat;
begin
  Memo1.Text := MyFont.getString(memTexte.Text);
  memInfosFont.Text := MyFont.CommentText;
end;

end.
