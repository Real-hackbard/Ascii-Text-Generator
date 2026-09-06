unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.Shell.ShellCtrls, FIGlet, LabelLien, Vcl.Samples.Spin,
  Jpeg, PngImage, GifImg, Vcl.StdCtrls, Vcl.ExtCtrls, Common,
  UTListViewSortOnClick, Vcl.Menus, Graphic, Vcl.Buttons;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    Memo2: TMemo;
    Memo3: TMemo;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    SaveDialog1: TSaveDialog;
    Panel4: TPanel;
    ListBox1: TListBox;
    Panel5: TPanel;
    Edit1: TEdit;
    Image1: TImage;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Button2: TButton;
    ComboBox2: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    ExportPicture1: TMenuItem;
    N1: TMenuItem;
    View1: TMenuItem;
    Font1: TMenuItem;
    Remove1: TMenuItem;
    Options1: TMenuItem;
    N2: TMenuItem;
    FontInformation1: TMenuItem;
    Panel3: TMenuItem;
    BackgroundColor1: TMenuItem;
    N3: TMenuItem;
    ransparent1: TMenuItem;
    Invert1: TMenuItem;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    Label6: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Edit2: TEdit;
    Label7: TLabel;
    BitBtn5: TBitBtn;
    About1: TMenuItem;
    Edit3: TEdit;
    Edit4: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    BitBtn6: TBitBtn;
    Label10: TLabel;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure Memo2Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetResultat;
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure ExportPicture1Click(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure FontInformation1Click(Sender: TObject);
    procedure BackgroundColor1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
  private
    { Declarations privates }
  public
    { Declarations public }
  end;

var
  Form1: TForm1;
  MyFont: TFIGlet;
  LastPositionH, LastPositionV : Integer;

implementation

{$R *.dfm}

// Replace all characters with the copied string.
function ReplaceAnyChar(const Input: string; ReplacementCharacters: Char): string;
var
  i: Integer;
  s : string;
begin
  // We copy the string to preserve the length.
  Result := Input;
  // copy memo text
  S := Form1.Memo1.Text;
  // Loop through each character
  for i := 1 to Length(Result) do
  begin
    // expression checks if a specific character in a string is a line break
    if not (S[i] in [#13, #10]) then
    begin
      // Ignore the spaces in the text
      if Result[i] <> ' ' then
        // replace only one char in a loop
        Result[i] := ReplacementCharacters;
    end;
  end;
end;

// convert bitmap pixel to png graphic
procedure BitmapFileToPNG(const Source, Dest: String);
var
  Bitmap: TBitmap;
  PNG: TPNGObject;
begin
  // create memory access
  Bitmap := TBitmap.Create;
  PNG := TPNGObject.Create;
  {In case something goes wrong, free booth Bitmap and PNG}
  try
    Bitmap.LoadFromFile(Source);
     //Convert data into png
    PNG.Assign(Bitmap);

    if Form1.ransparent1.Checked = true then
    begin
        // Transparent Color
        PNG.TransparentColor := clBlack;
        // Activate Transparent Png Image
        PNG.Transparent := true;
    end;
    // compress png data
    PNG.CompressionLevel := 9;
    // save png format file
    PNG.SaveToFile(Dest);
  finally
    Bitmap.Free;
    PNG.Free;
  end
end;

// pixel invert
function InvertBitmap(MyBitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ByteArray: PByteArray;
begin
  // set bit
  MyBitmap.PixelFormat := pf24Bit;
  for y := 0 to MyBitmap.Height - 1 do
  begin
    { returns a pointer to the first byte of the y-th row, instead of
      addressing each pixel individually via slow methods. }
    ByteArray := MyBitmap.ScanLine[y];
    for x := 0 to MyBitmap.Width * 3 - 1 do
    begin
      // inverts the bits of a byte at a specific index in a byte array
      ByteArray[x] := 255 - ByteArray[x];
    end;
  end;
  Result := MyBitmap;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  MessageDlg('ASCII Text Generator v1.0.3' + char(10) +
             'Copyright © hackbard' + char(10) +
             'github.com | Release 2026',mtInformation, [mbOK], 0);
end;

procedure TForm1.BackgroundColor1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Memo1.Color := ColorDialog1.Color;
  end;
end;

// copy ascii text to clipboard
procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.SelLength := 0;
  StatusBar1.SetFocus;
end;

// create margin on right side for image export
procedure TForm1.BitBtn2Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Memo1.Lines.Count - 1 do
  begin
    // add space behind all lines
    if Length(Memo1.Text) > 0 then
      Memo1.Lines[i] := Memo1.Lines[i] + ' ';
  end;
  Memo1.SelectAll;
  Memo1.SetFocus;
end;

// reduce margin on right side
procedure TForm1.BitBtn3Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Memo1.Lines.Count - 1 do
  begin
    // remove spave behind all lines
    if Length(Memo1.Text) > 0 then
      Memo1.Lines[i] := Copy(Memo1.Lines[i], 1, Length(Memo1.Lines[i]) - 1);
  end;
  // this shows the result
  Memo1.SelectAll;
  Memo1.SetFocus;
end;

// replace any char
procedure TForm1.BitBtn4Click(Sender: TObject);
var
  letter : string;
  replace : Char;
begin
  // replace
  letter := Edit2.Text;
  // Extract the first character  from (Edit2.Text)
  replace := letter.Chars[0];
  // Replace all characters with the copied string.
  Memo1.Text := ReplaceAnyChar(Memo1.Text, replace);
  StatusBar1.SetFocus;
end;

// undo text
procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  ListBox1.OnClick(sender);
  StatusBar1.SetFocus;
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
  Memo1.Text := StringReplace(Memo1.Text,Edit3.Text,Edit4.Text,[rfReplaceAll]);
  StatusBar1.SetFocus;
end;

// Replace the hashtags with a empty character.
procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Text := StringReplace(Memo1.Text,'#','',[rfReplaceAll]);
  StatusBar1.SetFocus;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  Close();
end;

// search font in ListBox
procedure TForm1.Edit1Change(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to Listbox1.items.count-1 do
    // Determine the position
    if pos(Edit1.Text, listbox1.items[i]) > 0 then
    begin
      // Select the position and go outside.
      ListBox1.itemindex:=i;
      Exit;
    end;
end;

// search FIGlet font
procedure TForm1.Edit1Click(Sender: TObject);
begin
  Edit1.SelectAll;
end;

// export picture to different formats
procedure TForm1.ExportPicture1Click(Sender: TObject);
var
  bmp : TBitmap;
  Jpg: TJPEGImage;
  GIF : TGIFImage;
  TextRect : TRect;
  lcr : integer;
  Image : TImage;
begin
  // create memory access
  bmp := TBitmap.Create;

  bmp.Canvas.Brush.Style := bsClear;
  bmp.Canvas.Brush.Color := ColorDialog1.Color;
  bmp.Canvas.Font := Memo1.Font;

  // set alignment to zero
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
    // Draw Text ( for older compiler versions take (PChar)
    DrawText(bmp.Canvas.Handle,PWideChar(Memo1.Lines.Text),
             length(Memo1.Lines.Text),TextRect,
             DT_NOPREFIX or lcr);

    // Invert Bitmap
    if Invert1.Checked = true then InvertBitmap(bmp);

    //set bitmap pixel format
    case ComboBox2.ItemIndex of
      0 : bmp.PixelFormat := pf1bit;
      1 : bmp.PixelFormat := pf4bit;
      2 : bmp.PixelFormat := pf8bit;
      3 : bmp.PixelFormat := pf16bit;
      4 : bmp.PixelFormat := pf24bit;
      5 : bmp.PixelFormat := pf32bit;
    end;

    // Create Images
    if SaveDialog1.Execute then
    begin
      // Create Bitmap Image
      if SaveDialog1.FilterIndex = 1 then
      begin
        // create a dynamic TImage component
        Image := TImage.Create(self);
        // copy pixel to image
        Image.Picture.Bitmap.Assign(bmp);
        // export picture
        Image.Picture.Bitmap.SaveToFile(SaveDialog1.FileName + '.bmp');
      end;

      // Create Jpg Image
      if SaveDialog1.FilterIndex = 2 then
      begin
        // create jpg memory access
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
      if SaveDialog1.FilterIndex = 3 then
      begin
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
      if SaveDialog1.FilterIndex = 4 then
      begin
        Image := TImage.Create(self);
        Image.Picture.Bitmap.Assign(bmp);
        // create gif memory access
        GIF := TGIFImage.Create;
        try
          // Copy Bitmap Pixel to GIF Data
          GIF.Assign(bmp);
          // Create GIF File Image
          GIF.SaveToFile(SaveDialog1.FileName + '.gif');
        finally
          // Let go of the GIF.
          GIF.Free;
        end;
      end;
    end;
  finally
    // Free up the memory for all pixels.
    bmp.Free;
    Image.Free;
  end;
end;

// set font style
procedure TForm1.Font1Click(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    Memo1.Font := FontDialog1.Font;
    StatusBar1.Panels[5].Text := FontDialog1.Font.Name;
  end;
end;

procedure TForm1.FontInformation1Click(Sender: TObject);
var
  Rect : TRect;
begin
  Memo3.Visible := FontInformation1.Checked;
  // create fix margin to input text
  SendMessage(Memo2.Handle, EM_GETRECT, 0, LongInt(@Rect));
  Rect.Left:= 10;  // pixel (10)
  SendMessage(Memo2.Handle, EM_SETRECT, 0, LongInt(@Rect));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Liste: TStringList;
  i: integer;
  Num: string;
  DefaultFound: boolean;
  Rect: TRect;
begin
  // Prevents flickering when writing ASCII text.
  DoubleBuffered := true;
  // create FIGlet font access
  MyFont := TFIGlet.Create;
  Memo1.Text  := '';
  Memo3.Text  := '';
  ListBox1.Clear;
  // font size
  SpinEdit1.Value := 8;

  // set background color or it is black
  ColorDialog1.Color := clWhite;

  DefaultFound := False;
  // create luist access
  Liste := TStringList.Create;
  // search for the *.fif files
  SearchFiles('.', 'flf', True, Liste);

  for i := 0 to Liste.Count - 1 do
  begin
    Num := BaseName(Liste[i], '.flf');
    ListBox1.AddItem(Num, nil);

    if Num = 'standard' then
    begin
      // If found, write the ASCII text.
      ListBox1.Selected[i] := true;
      MyFont.FontFile := Liste[i];
      DefaultFound := True;
    end;
  end;

  if not DefaultFound then
  begin
    if ListBox1.Count > 0 then
    begin
      ListBox1.Selected[0] := true;
      MyFont.FontFile := Liste[0];
    end;
  end;

  // Make the list available again.
  Liste.Free;
  // count font files
  StatusBar1.Panels[1].Text := IntToStr(ListBox1.Items.Count);
  // show the result
  GetResultat;
  // Determine the position of the ASCII text.
  LastPositionH := 0;

  // create margin to input text
  SendMessage(Memo2.Handle, EM_GETRECT, 0, LongInt(@Rect));
  Rect.Left:= 10;
  SendMessage(Memo2.Handle, EM_SETRECT, 0, LongInt(@Rect));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // free up memory
  MyFont.Free;
end;

// select FIGlet font
procedure TForm1.ListBox1Click(Sender: TObject);
begin
  SpinEdit2.Value := 0;
  SpinEdit3.Value := 0;
  // pass font selection
  MyFont.FontFile := 'fonts\' + ListBox1.Items[ListBox1.ItemIndex]+'.flf';
  StatusBar1.Panels[3].Text := ListBox1.Items.Strings[ListBox1.ItemIndex];
  // get font information
  GetResultat;
end;

// type text
procedure TForm1.Memo2Change(Sender: TObject);
begin
  SpinEdit2.Value := 0;
  SpinEdit3.Value := 0;
  GetResultat;
end;

procedure TForm1.Panel3Click(Sender: TObject);
var
  Rect : TRect;
begin
  Panel4.Visible := Panel3.Checked;
  // create margin to input text
  SendMessage(Memo2.Handle, EM_GETRECT, 0, LongInt(@Rect));
  Rect.Left:= 10;
  SendMessage(Memo2.Handle, EM_SETRECT, 0, LongInt(@Rect));
end;

procedure TForm1.Remove1Click(Sender: TObject);
begin
  Button2.OnClick(self);
end;


procedure TForm1.ScrollBar2Change(Sender: TObject);

begin

end;

// pass font size
procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  Memo1.Font.Size := SpinEdit1.Value;
end;

// horizontal positioning of the text
procedure TForm1.SpinEdit2Change(Sender: TObject);
var
  i, ScrollPos: Integer;
begin
  Screen.Cursor := crHourGlass;
  ScrollPos := SpinEdit2.Value;

  if ScrollPos > LastPositionH then
  begin
    // move text to right
    for i := 0 to Memo1.Lines.Count - 1 do
    begin
      if Memo1.Lines[i] > '' then
        Memo1.Lines[i] := ' ' + Memo1.Lines[i];
    end;
  end else begin
    // move text to left
    if ScrollPos < LastPositionH then
    for i := 0 to Memo1.Lines.Count - 1 do
    begin
      if Length(Memo1.Lines[i]) > 0 then
        Memo1.Lines[i] := Copy(Memo1.Lines[i], 2, Length(Memo1.Lines[i]));
      end;
  end;
  // Update position for next time
  LastPositionH := ScrollPos;
  Memo1.Update;

  Screen.Cursor := crDefault;
end;

// vertical positioning of the text
procedure TForm1.SpinEdit3Change(Sender: TObject);
var
  i, ScrollPos: Integer;
begin
  Screen.Cursor := crHourGlass;
  ScrollPos := SpinEdit3.Value;

  if ScrollPos > LastPositionV then
  begin
    // move text down
    Memo1.Lines.Insert(0, '');
  end else begin
    // move text up
    if ScrollPos < LastPositionV then
    if Memo1.Lines.Count > 0 then
      Memo1.Lines.Delete(0);
  end;
  // Update position for next time
  LastPositionV := ScrollPos;
  Memo1.Update;

  Screen.Cursor := crDefault;
end;

// pass font information
procedure TForm1.GetResultat;
begin
  Memo1.Text := MyFont.getString(Memo2.Text);
  Memo3.Text := MyFont.CommentText;
end;

end.
