unit Common;

interface

uses
  SysUtils, Classes, Math, ShellAPI, Windows, Dialogs,
  Controls, Forms, ComCtrls, ExtCtrls, StdCtrls, FileCtrl, StrUtils,
  Mask, JPEG, pngimage, Graphics ;

procedure SelectItem(Obj: TListView; Index : integer);
procedure ImageLoad(Obj: TImage; FileName: string);
procedure ImageListAdd(Obj: TImageList; FileName: string);
function Base64_Decode(S: string): string;
function Base64_Encode(S: string): string;
function IsFloat(S: String) : Boolean;
function IsInteger(S: String) : Boolean;
function GetInteger(S: String) : Integer;
procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings) ;
function RightStr(const Str: string; Size: Word): string;
function LeftStr(const Str: string; Size: Word): string;
function MidStr(const strString: string; nBegin, nLaenge: integer): string;
function Instr(const strSource: string; const strSubStr: string): Integer;
function RInstr(const Str: string; const strSuche: string): Integer;
procedure SearchFiles(Chemin, Extension: string; Recursif: boolean; const Files: TStrings); overload;
procedure SearchFiles(Chemin: string; Recursif: boolean; const Files: TStrings); overload;
function StrPadLeft(Texte: string; n: integer): string; overload;
function StrPadLeft(Entier: integer; n: integer): string; overload;
function StrPadLeft(Texte: string; n: integer; Blank: string): string; overload;
function StrPadLeft(Entier: integer; n: integer; Blank: string): string; overload;
function StrPadRight(Texte: string; n: integer; Blank: string): string; overload;
function StrPadRight(Entier: integer; n: integer; Blank: string): string; overload;
function StrPadRight(Texte: string; n: integer): string; overload;
function StrPadRight(Entier: integer; n: integer): string; overload;
function RGBToColor(R, G, B:integer): TColor;
function BaseName(Path, ext: string): string; overload;
function BaseName(Path: string): string; overload;
function GetDirSlash(Path: string; slash:boolean): string; overload;
function GetDirSlash(Path: string): string; overload;
function DirName(Path : string): string;
function getFileContent(FileName: string): string;
procedure setFileContent(FileName, Content: string);
procedure OpenURL(URL: string);
procedure LanceCommand(bin, params: string; Visible: boolean);
function GetTempDirectory: string; overload;
function GetTempDirectory(tmp: string): String; overload;
function GetLanguageIndex: integer;
function HomeDir : string;
procedure ListBoxSelectAll(Sender: TObject);
procedure ListBoxSelectInvert(Sender: TObject);
procedure ListBoxDeselect(Sender: TObject);
procedure ValidInteger(Sender: TObject); overload;
procedure ValidInteger(Sender: TObject;Min,Max : integer); overload;
function ValidFolder(Sender: TObject): boolean;
procedure ValidTimeHMS(Sender: TObject); overload;
procedure SelectFolder(Sender: TObject; Msg : string);
function Str(Val: boolean): string; overload;
function Str(Val: integer): string; overload;
function Str(Val: extended): string; overload;
function Str(Val: string): string; overload;
function ToInt(Val: extended): integer; overload;
function ToInt(Val: integer): integer; overload;
procedure FileListRefresh(List: TFileListBox);
function TimeToSeconds(Text: string) : integer;
function SecondsToTime(sec: integer) : string;
function StrReplace(S, Old, New: string) : string;  
function StrReplaceI(S, Old, New: string) : string;
function MsgConfirm(Text, Caption: string): boolean;
procedure MsgError(Text, Caption: string);
procedure MsgExclam(Text, Caption: string);
procedure MsgWarning(Text, Caption: string);
procedure MsgInfo(Text, Caption: string);
Function DelTree(DirName : string): Boolean;
Function GetNow: string;
function ValidGetValue(Sender: TObject; Default:string): string;
procedure ValidSetValue(Sender: TObject; OldValue, NewValue:string);
procedure TextToTStrings(const List : TStrings; const FileName : String);
procedure TStringsToText(const List : TStrings; const FileName : String);
function DOSToAnsi(Text : String):string;
function AnsiToDOS(Text : String):string;
function FileExtension(FileName:string): string; overload; 
function IntToBin( value: LongInt ): string;
function IntToRoman(Value: LongInt): String;
function StrRepeat(s: string; n:integer ): string;
function StrRev(S: string): string;
function HexToInt(HexNum: string): LongInt;
function BinToInt(Value: String): LongInt;
procedure bmp2ico(Image: TImage; FileName: TFilename);
procedure TListViewUp(List: TListView);
procedure TListViewTop(List: TListView);
procedure TListViewDown(List: TListView);
procedure TListViewBottom(List: TListView);

type
  TThreadFunc = class(TThread)
    private
      MyFunc : TNotifyEvent;   
      procedure Lance;
    protected
      procedure Execute();override;
    public
      constructor Create(Func: TNotifyEvent);
  end;

const
  CRLF = #13#10;

implementation
uses CommonRegex;
//---------------------------------------------------------------
constructor TThreadFunc.Create(Func: TNotifyEvent);
begin
  inherited Create(false);
  FreeOnTerminate := True;
  if Assigned(Func) then MyFunc := Func;
end;
//---------------------------------------------------------------
procedure TThreadFunc.Execute;
begin
  Synchronize(Lance);
end;       
//---------------------------------------------------------------
procedure TThreadFunc.Lance;
begin
  if Assigned(MyFunc) then MyFunc(Self);
end;
//-------------------------------------------------------------
procedure TListViewTop(List: TListView);
var
  Item : tlistitem;
  Index, i : integer;
begin
  Index := List.ItemIndex;
  if Index>0 then begin
    item := List.Items.Insert(0);
    for I := 0 to List.Items[Index+1].SubItems.Count - 1 do begin
      item.SubItems.Add(List.Items[Index+1].SubItems[i]);
      item.SubItemImages[i] := List.Items[Index+1].SubItemImages[i];
    end;
    item.Caption := List.Items[Index+1].Caption;
    item.ImageIndex := List.Items[index+1].ImageIndex;
    SelectItem(List, 0);
    List.Items[index+1].Delete;
  end;
end;
//-------------------------------------------------------------
procedure TListViewUp(List: TListView);
var
  Item : tlistitem;
  Index, i : integer;
begin
  Index := List.ItemIndex;
  if Index>0 then begin
    item := List.Items.Insert(Index-1);
    for I := 0 to List.Items[Index+1].SubItems.Count - 1 do begin
      item.SubItems.Add(List.Items[Index+1].SubItems[i]);
      item.SubItemImages[i] := List.Items[Index+1].SubItemImages[i];
    end;
    item.Caption := List.Items[Index+1].Caption;
    item.ImageIndex := List.Items[index+1].ImageIndex;
    SelectItem(List, Index-1);
    List.Items[index+1].Delete;
  end;
end;     
//-------------------------------------------------------------
procedure TListViewBottom(List: TListView);
var
  Item : tlistitem;
  Index, i : integer;
begin
  Index := List.ItemIndex;
  if Index<List.Items.Count-1 then begin
    item := List.Items.Insert(List.Items.Count);
    for I := 0 to List.Items[Index].SubItems.Count - 1 do begin
      item.SubItems.Add(List.Items[Index].SubItems[i]);
      item.SubItemImages[i] := List.Items[Index].SubItemImages[i];
    end;
    item.Caption := List.Items[Index].Caption;
    item.ImageIndex := List.Items[index].ImageIndex;
    SelectItem(List, List.Items.Count-1);
    List.Items[index].Delete;
  end;
end;
//-------------------------------------------------------------
procedure TListViewDown(List: TListView);
var
  Item : tlistitem;
  Index, i : integer;
begin
  Index := List.ItemIndex;
  if Index<List.Items.Count-1 then begin
    item := List.Items.Insert(Index+2);
    for I := 0 to List.Items[Index].SubItems.Count - 1 do begin
      item.SubItems.Add(List.Items[Index].SubItems[i]);
      item.SubItemImages[i] := List.Items[Index].SubItemImages[i];
    end;
    item.Caption := List.Items[Index].Caption;
    item.ImageIndex := List.Items[index].ImageIndex;
    SelectItem(List, Index+2);
    List.Items[index].Delete;
  end;
end;
//--------------------------------------------------------------------
procedure bmp2ico(Image: TImage; FileName: TFilename);
var
  Bmp: TBitmap;
  Icon: TIcon;
  ImageList: TImageList;
begin
  Bmp  := TBitmap.Create;
  Icon := TIcon.Create;
  try
    Bmp.Assign(Image.Picture);
    ImageList := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    try
      ImageList.AddMasked(Bmp, Bmp.TransparentColor);
      ImageList.GetIcon(0, Icon);
      // Save it to a file
      Icon.SaveToFile(FileName);
    finally
      ImageList.Free;
    end;
  finally
    Bmp.Free;
    Icon.Free;
  end;
end;
//--------------------------------------------------------------------
function DOSToAnsi(Text : String):string;
begin
  Result:= Text;
  OemToAnsi(PAnsiChar(Text), PAnsiChar(Result));
end;
//--------------------------------------------------------------------
function AnsiToDOS(Text : String):string;
begin
  Result:= Text;
  AnsiToOem(PAnsiChar(Text), PAnsiChar(Result));
end;
//--------------------------------------------------------------------
procedure TextToTStrings(const List : TStrings; const FileName : String);
begin
  with List do begin
    Clear;
    LoadFromFile(FileName);
  end;
end;
//--------------------------------------------------------------------
procedure TStringsToText(const List : TStrings; const FileName : String);
begin
  with List do
    SaveToFile(FileName);
end;
//--------------------------------------------------------------------
Function GetNow: string;
begin
  DateTimeToString(Result, 'yyyy/dd/mm hh:nn:ss', Now);
end;
//--------------------------------------------------------------------
Function DelTree(DirName : string): Boolean;
var
  SHFileOpStruct : TSHFileOpStruct;
  DirBuf : array [0..255] of char;
begin
  try
   Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0) ;
   FillChar(DirBuf, Sizeof(DirBuf), 0 ) ;
   StrPCopy(DirBuf, DirName) ;
   with SHFileOpStruct do begin
    Wnd := 0;
    pFrom := @DirBuf;
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO;
    fFlags := fFlags or FOF_NOCONFIRMATION;
    fFlags := fFlags or FOF_SILENT;
   end;
    Result := (SHFileOperation(SHFileOpStruct) = 0) ;
   except
    Result := False;
  end;
end;
//--------------------------------------------------------------------
procedure MsgError(Text, Caption: string);
begin
  MessageBox(Application.Handle, PAnsichar(Text), PAnsichar(Caption), MB_ICONERROR or MB_OK);
end;     
//--------------------------------------------------------------------
procedure MsgExclam(Text, Caption: string);
begin
  MessageBox(Application.Handle, PAnsichar(Text), PAnsichar(Caption), MB_ICONEXCLAMATION or MB_OK);
end;
//--------------------------------------------------------------------
procedure MsgWarning(Text, Caption: string);
begin
  MessageBox(Application.Handle, PAnsichar(Text), PAnsichar(Caption), MB_ICONWARNING or MB_OK);
end;
//--------------------------------------------------------------------
procedure MsgInfo(Text, Caption: string);
begin
  MessageBox(Application.Handle, PAnsichar(Text), PAnsichar(Caption), MB_ICONINFORMATION or MB_OK);
end;
//--------------------------------------------------------------------
function MsgConfirm(Text, Caption: string): boolean;
var i: integer;
begin
  i := MessageBox(Application.Handle, PAnsichar(Text), PAnsichar(Caption), MB_ICONQUESTION or MB_OKCANCEL);
  Result := (i=1);
end;
//--------------------------------------------------------------------
function StrReplace(S, Old, New: string) : string;
begin
  Result := StringReplace(S, Old, New, [rfReplaceAll]);
end;
//--------------------------------------------------------------------
function StrReplaceI(S, Old, New: string) : string;
begin
  Result := StringReplace(S, Old, New, [rfReplaceAll, rfIgnoreCase]);
end;     
//--------------------------------------------------------------------
function SecondsToTime(sec: integer) : string;
var
  H, M, S: integer;
begin
  H := floor(sec/3600);
  M := floor((sec-3600*H)/60);
  S := sec-3600*H-60*M;
  If H < 10 then Result := '0'+Str(H) else Result := Str(H);
  If M < 10 then Result := Result+':0'+Str(M) else Result := Result+':'+Str(M);
  If S < 10 then Result := Result+':0'+Str(S) else Result := Result+':'+Str(S);
end;
//--------------------------------------------------------------------
function TimeToSeconds(Text: string) : integer;
var
  txt : string;
  Items: TStringList;
begin
  Items := TStringList.Create;
  txt := StringReplace(Text, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  txt := StringReplace(Txt, '_', '', [rfReplaceAll, rfIgnoreCase]);
  Split(':', txt, Items);
  If Items[0] = '' then Items[0] := '0';
  If Items[1] = '' then Items[1] := '0';
  If Items[2] = '' then Items[2] := '0';
  result:= getInteger(Items[0])*3600+getInteger(Items[1])*60+getInteger(Items[2]);
  Items.Free;
end;
//--------------------------------------------------------------------
procedure FileListRefresh(List: TFileListBox);
var
  tmp: string;
begin
  tmp := List.Directory;
  List.Directory := '.';
  List.Directory := tmp;
  List.Refresh;
end;    
//--------------------------------------------------------------------
procedure ImageListAdd(Obj: TImageList; FileName: string);
var
  ext: string; 
  JPG: TJpegImage;
  PNG: TPNGobject;
  BMP: TBitmap;
begin
  if FileExists(FileName) then begin
    ext := LowerCase(FileExtension(FileName));
    if ext='png' then begin
      PNG := TPNGobject.Create;
      BMP := TBitmap.Create;
      try
        PNG.LoadFromFile(FileName);
        BMP.Assign(PNG);
        Obj.AddMasked(BMP, BMP.TransparentColor);
      finally
        PNG.Free; 
        BMP.Free;
      end;
    end else if ext='jpg' then begin
      JPG := TJpegImage.Create;   
      BMP := TBitmap.Create;
      try
        JPG.LoadFromFile(FileName);
        BMP.Assign(JPG);          
        Obj.AddMasked(BMP, BMP.TransparentColor);
      finally
        JPG.Free;
        BMP.Free;
      end;
    end;  
  end;
end;
//--------------------------------------------------------------------
procedure ImageLoad(Obj: TImage; FileName: string);
var
  ext: string; 
  JPG: TJpegImage;
  PNG: TPNGobject;
begin
  if FileExists(FileName) then begin
    ext := LowerCase(FileExtension(FileName));
    if ext='png' then begin
      PNG := TPNGobject.Create;
      try
        PNG.LoadFromFile(FileName);
        Obj.Picture.Bitmap.Assign(PNG);
      finally
        PNG.Free;
      end;
    end else if ext='jpg' then begin
      JPG := TJpegImage.Create;
      try
        JPG.LoadFromFile(FileName);
        Obj.Picture.Bitmap.Assign(JPG);
      finally
        JPG.Free;
      end;
    end;  
  end;
end;
//--------------------------------------------------------------------
function Str(Val: string): string; overload;
begin
  Result := Val;
end;
//--------------------------------------------------------------------
function Str(Val: boolean): string; overload;
begin
  Result := BoolToStr(Val);
end;
//--------------------------------------------------------------------
function Str(Val: integer): string; overload;
begin
  Result := IntToStr(Val);
end;
//--------------------------------------------------------------------
function Str(Val: extended): string; overload;
begin
  Result := FloatToStr(Val);
end;          
//--------------------------------------------------------------------
function ToInt(Val: integer): integer; overload;
begin
  Result := Val;
end;
//--------------------------------------------------------------------
function ToInt(Val: extended): integer; overload;
begin
  Result := GetInteger(FloatToStr(Int(Val)));
end;     
//--------------------------------------------------------------------
procedure SelectItem(Obj: TListView; Index : integer);
begin
  if (Index>=0) and (Index<Obj.Items.count) then begin
    Obj.Items[Index].Selected := True;
    Obj.ItemFocused := Obj.Items[Index];
    Obj.Selected := Obj.Items[Index];
  end;
end;
//--------------------------------------------------------------------
procedure SelectFolder(Sender: TObject; Msg : string);
var
  SenderClass: string;
  sFolder: string;
begin         
  SenderClass := Sender.ClassName;
  sFolder := '';
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
    0: sFolder := TComboBox(Sender).Text;
    1: sFolder := TEdit(Sender).Text;
  end;
  if SelectDirectory(Msg, '', sFolder) then
    if sFolder <> '' then
      case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
        0: TComboBox(Sender).Text := sFolder;
        1: TEdit(Sender).Text := sFolder;
      end;
end;
//--------------------------------------------------------------------
function ValidFolder(Sender: TObject) : boolean;
var
  SenderClass: string;
  Text : string;
begin
  SenderClass := Sender.ClassName;
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
    0: Text := TComboBox(Sender).Text;
    1: Text := TEdit(Sender).Text;
  end;
  Result := DirectoryExists(Text);
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit']) of
    0: if Result then TComboBox(Sender).Font.Color := clWindowText
                 else TComboBox(Sender).Font.Color := clRed;
    1: if Result then TEdit(Sender).Font.Color := clWindowText
                 else TEdit(Sender).Font.Color := clRed;
  end;
end;
//--------------------------------------------------------------------
function ValidGetValue(Sender: TObject; Default:string): string;
var SenderClass: string;
begin
  SenderClass := Sender.ClassName;
  Result := default;
  case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit', 'TLabeledEdit', 'TMaskEdit']) of
    0: Result := TComboBox(Sender).Text;
    1: Result := TEdit(Sender).Text;
    2: Result := TLabeledEdit(Sender).Text;    
    3: Result := TMaskEdit(Sender).Text;
  end;
end;            
//--------------------------------------------------------------------
procedure ValidSetValue(Sender: TObject; OldValue, NewValue:string);
var
  SenderClass: string;
  SelStart, SelLength : integer;
begin      
  SenderClass := Sender.ClassName;
  if NewValue <> OldValue then begin
    case AnsiIndexStr(SenderClass, ['TComboBox', 'TEdit', 'TLabeledEdit', 'TMaskEdit']) of
      0: begin
        SelStart := TComboBox(Sender).SelStart;
        SelLength := TComboBox(Sender).SelLength;
        TComboBox(Sender).Text := NewValue;
        TComboBox(Sender).SelStart := SelStart;
        TComboBox(Sender).SelLength := SelLength;
      end;
      1: begin
        SelStart := TEdit(Sender).SelStart;
        SelLength := TEdit(Sender).SelLength;
        TEdit(Sender).Text := NewValue;
        TEdit(Sender).SelStart := SelStart;
        TEdit(Sender).SelLength := SelLength;
      end; 
      2: begin
        SelStart := TLabeledEdit(Sender).SelStart;
        SelLength := TLabeledEdit(Sender).SelLength;
        TLabeledEdit(Sender).Text := NewValue;
        TLabeledEdit(Sender).SelStart := SelStart;
        TLabeledEdit(Sender).SelLength := SelLength;
      end;  
      3: begin
        SelStart := TMaskEdit(Sender).SelStart;
        SelLength := TMaskEdit(Sender).SelLength;
        TMaskEdit(Sender).Text := NewValue;
        TMaskEdit(Sender).SelStart := SelStart;
        TMaskEdit(Sender).SelLength := SelLength;
      end;
    end;
  end;
end;
//--------------------------------------------------------------------
procedure ValidTimeHMS(Sender: TObject); overload;
var
  Value, OldValue: string;
  Items: TStringList;
  H, M, S : integer;
begin
  OldValue := ValidGetValue(Sender, '0:0:0');
  OldValue := StrReplace(OldValue, ' ', '');
  OldValue := StrReplace(OldValue, '_', '');
  Items := TStringList.Create;
  Split(':', OldValue, Items);
  H := GetInteger(Items[0]);
  M := GetInteger(Items[1]);
  S := GetInteger(Items[2]);
  Items.Free;
  If M > 60 then M := 59;
  If S > 60 then S := 59;
  If H < 10 then Value := '0'+Str(H) else Value := Str(H);
  If M < 10 then Value := Value+':0'+Str(M) else Value := Value+':'+Str(M);
  If S < 10 then Value := Value+':0'+Str(S) else Value := Value+':'+Str(S);
  ValidSetValue(Sender, OldValue, Value);
end;
//--------------------------------------------------------------------
procedure ValidInteger(Sender: TObject;Min,Max : integer); overload;
var
  Value, OldValue: string;
begin
  OldValue := ValidGetValue(Sender, '0');
  Value := IntToStr(GetInteger(OldValue));
  if StrToInt(Value) < Min then  Value := IntToStr(Min)
  else if StrToInt(Value) > Max then Value := IntToStr(Max);
  ValidSetValue(Sender, OldValue, Value);
end;
//--------------------------------------------------------------------
procedure ValidInteger(Sender: TObject); overload;
var
  Value, OldValue: string;
begin
  OldValue := ValidGetValue(Sender, '0');
  Value := IntToStr(GetInteger(OldValue));
  ValidSetValue(Sender, OldValue, Value);
end;
//--------------------------------------------------------------------
procedure ListBoxSelectAll(Sender: TObject);
var i : integer;
begin
  case AnsiIndexStr(Sender.ClassName, ['TFileListBox', 'TListBox']) of
    0: begin
      for I := 0 to TFileListBox(Sender).Items.Count - 1 do
         TFileListBox(Sender).Selected[i] := True;
      TFileListBox(Sender).OnChange(Sender);
    end;
    1: for I := 0 to TListBox(Sender).Items.Count - 1 do
         TListBox(Sender).Selected[i] := True;
  end;     
end;
//--------------------------------------------------------------------
procedure ListBoxDeselect(Sender: TObject);
var i : integer;
begin
  case AnsiIndexStr(Sender.ClassName, ['TFileListBox', 'TListBox']) of
    0: begin
      for I := 0 to TFileListBox(Sender).Items.Count - 1 do
        TFileListBox(Sender).Selected[i] := False;
      TFileListBox(Sender).OnChange(Sender);
    end;
    1: begin
      for I := 0 to TListBox(Sender).Items.Count - 1 do
        TListBox(Sender).Selected[i] := False;
    end;
  end;
end;
//--------------------------------------------------------------------
procedure ListBoxSelectInvert(Sender: TObject);
var i : integer;
begin          
  case AnsiIndexStr(Sender.ClassName, ['TFileListBox', 'TListBox']) of
    0: begin
      for I := 0 to TFileListBox(Sender).Items.Count - 1 do
         TFileListBox(Sender).Selected[i] := Not(TFileListBox(Sender).Selected[i]);
      TFileListBox(Sender).OnChange(Sender);
    end;
    1: for I := 0 to TListBox(Sender).Items.Count - 1 do
         TListBox(Sender).Selected[i] := Not(TListBox(Sender).Selected[i]);
  end;
end;
//--------------------------------------------------------------------
function HomeDir : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0))));
end;
//--------------------------------------------------------------------
function GetLanguageIndex: integer;
begin
  Result := GetUserDefaultLCID() AND 1023;
end;
//--------------------------------------------------------------------
function GetTempDirectory: String; overload;
var
  tempFolder: array[0..4096] of Char;
begin
  GetTempPath(4096, @tempFolder);
  result := StrPas(tempFolder);
end;
//--------------------------------------------------------------------
function GetTempDirectory(tmp: string): String; overload;
var
  tempFolder: array[0..4096] of Char;
begin
  GetTempPath(4096, @tempFolder);
  result := StrPas(tempFolder)+tmp+str(DateTimeToTimeStamp(Now).time);
end;
//--------------------------------------------------------------------
function getFileContent(FileName: string): string;
var
  F: TFileStream;
begin
  Result := '';
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    F.Position := 0;
    if (F.Size < 1024*1024*50) then begin
      setLength(Result, F.Size);
      F.Read(Result[1], F.Size);
    end
    else
      ShowMessage('Too big file (> 50 Mb)');
  finally
    F.Free;
  end;
end;
//--------------------------------------------------------------------
procedure setFileContent(FileName, Content: string);
var
  F: TFileStream;
begin
  if FileExists(FileName) then DeleteFile(PAnsiChar(FileName));
  try
    F := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    F.Write(Content[1], Length(Content));
  finally
    F.Free;
  end;
end;
//--------------------------------------------------------------------
procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings) ;
var
  i: integer;
  buffer: string;
begin
  Assert(Assigned(Strings)) ;
  Strings.Clear;
  buffer := '';
  for i := 1 to length(Input) do begin
    if Delimiter = Input[i] then begin
      Strings.Add(buffer);
      buffer := '';
    end else
      buffer := buffer + Input[i];
  end;
  Strings.Add(buffer);
end; 
//--------------------------------------------------------------------
function DirName(Path: string): string;
var
  k : integer;
begin
  Result := Path;
  k := RInstr(Result, '/');
  if k>0 then Result := LeftStr(Result, k);
  k := RInstr(Result, '\');
  if k>0 then Result := LeftStr(Result, k);
end;
//--------------------------------------------------------------------
function GetDirSlash(Path: string; slash:boolean): string; overload;
begin
  Result := Path;
  while Result[Length(Result)] = '/' do
    Result := LeftStr(Result, Length(Result) - 1);
  while Result[Length(Result)] = '\' do
    Result := LeftStr(Result, Length(Result) - 1);
  if slash then Result := IncludeTrailingPathDelimiter(Result);
end;                      
//--------------------------------------------------------------------
function GetDirSlash(Path: string): string; overload;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;       
//--------------------------------------------------------------------
function FileExtension(FileName:string): string; overload;
var
  k : integer;
begin
  Result := '';
  k := RInstr(FileName, '.');
  if k>0 then Result := RightStr(FileName, Length(FileName)-k);
end;
//--------------------------------------------------------------------
function BaseName(Path, ext: string): string; overload;
var
  k : integer;
begin
  Result := Path;
  if RightStr(LowerCase(Path), Length(ext)) = LowerCase(ext) then
    Result := LeftStr(Result, Length(Path)-Length(ext));
  k := RInstr(Result, '/');
  if k>0 then Result := RightStr(Result, Length(Result)-k);
  k := RInstr(Result, '\');
  if k>0 then Result := RightStr(Result, Length(Result)-k);
end;
//--------------------------------------------------------------------
function BaseName(Path: string): string; overload;
begin
  Result := BaseName(Path, '');
end;
//--------------------------------------------------------------------
function RGBToColor(R, G, B:integer): TColor;
var
  C : integer;
begin
  C := (B*256 + G)*256 + R;
  Result := TColor(C);
end;                 
//--------------------------------------------------------------------
function StrPadRight(Entier: integer; n: integer): string; overload;
begin
  Result := StrPadRight(IntToStr(Entier), n, ' ');
end;
//--------------------------------------------------------------------
function StrPadRight(Entier: integer; n: integer; Blank: string): string; overload;
begin
  Result := StrPadRight(IntToStr(Entier), n, Blank);
end;   
//--------------------------------------------------------------------
function StrPadRight(Texte: string; n: integer): string; overload;
begin
  Result := StrPadRight(Texte, n, ' ');
end;
//--------------------------------------------------------------------
function StrPadRight(Texte: string; n: integer; Blank: string): string; overload;
var
  i, nb: integer;
begin
  Result := Texte;
  nb := Ceil((n - Length(Texte))/Length(Blank));
  for i := 1 to nb do begin
    Result := Result+Blank;
  end;
  Result := RightStr(Result, n);
end;
//--------------------------------------------------------------------
function StrPadLeft(Texte: string; n: integer): string; overload;
begin
  Result := StrPadLeft(Texte, n, ' ');
end;
//--------------------------------------------------------------------
function StrPadLeft(Entier: integer; n: integer): string; overload;
begin
  Result := StrPadLeft(IntToStr(Entier), n, ' ');
end;
//--------------------------------------------------------------------
function StrPadLeft(Entier: integer; n: integer; Blank: string): string; overload;
begin
  Result := StrPadLeft(IntToStr(Entier), n, Blank);
end;
//--------------------------------------------------------------------
function StrPadLeft(Texte: string; n: integer; Blank: string): string; overload;
var
  i, nb: integer;
begin
  Result := Texte;
  nb := Ceil((n - Length(Texte))/Length(Blank));
  for i := 1 to nb do begin
    Result := Blank+Result;
  end;
  Result := RightStr(Result, n);
end;
//--------------------------------------------------------------------
function RightStr(const Str: string; Size: Word): string;
var
  len: LongInt;
begin
  len := Length(str);
  if Size > len then Size := len;
  RightStr := Copy(Str, len - Size + 1, Size)
end;
//--------------------------------------------------------------------
function LeftStr(const Str: string; Size: Word): string;
begin
  LeftStr := Copy(Str, 1, Size)
end;
//--------------------------------------------------------------------
function MidStr(const strString: string; nBegin, nLaenge: integer): string;
var
  strStr: string;
begin
  strStr := Copy(strString, nBegin, nLaenge);
  Result := strStr;
end;
//--------------------------------------------------------------------
function Instr(const strSource: string; const strSubStr: string): Integer;
begin
  Result := Pos(strSubstr, strSource);
end;
//--------------------------------------------------------------------
function RInstr(const Str: string; const strSuche: string): Integer;
var
  i: integer;
  l: integer;
begin
  l := Length(strsuche);
  for i := length(Str) downto 1 do
    begin
      if midStr(Str, i, l) = strSuche then
        begin
          Break;
        end;
    end;
  Result := i;
end;                
//--------------------------------------------------------------------
procedure SearchFiles(Chemin: string; Recursif: boolean; const Files: TStrings); overload;
var
  S: TSearchRec;
begin
  Assert(Assigned(Files)) ;
  Chemin:=IncludeTrailingPathDelimiter(Chemin);
  // fichiers
  if FindFirst(Chemin+'*', faAnyFile, S)=0 then
  begin
    repeat
      if (S.Name<>'.') And (S.Name<>'..') then
        If (S.Attr And faDirectory)=0 then
          Files.Add(Chemin+S.FindData.cFileName);
    until FindNext(S)<>0;
    SysUtils.FindClose(S);
  end;
  // dossiers
  if Recursif then
    if FindFirst(Chemin+'*.*', faDirectory, S)=0 then begin
      repeat
        if (S.Name<>'.') And (S.Name<>'..') then
          If (S.Attr And faDirectory)<>0 then
              SearchFiles(Chemin+S.FindData.cFileName, Recursif, Files);
      until FindNext(S)<>0;
      SysUtils.FindClose(S);
    end;
end;
//--------------------------------------------------------------------
procedure SearchFiles(Chemin, Extension: string; Recursif: boolean; const Files: TStrings); overload;
var
  S: TSearchRec;
begin
  Assert(Assigned(Files)) ;
  Chemin:=IncludeTrailingPathDelimiter(Chemin);
  // fichiers
  if FindFirst(Chemin+'*.'+Extension, faAnyFile, S)=0 then
  begin
    repeat
      if (S.Name<>'.') And (S.Name<>'..') then
        If (S.Attr And faDirectory)=0 then
          Files.Add(Chemin+S.FindData.cFileName);
    until FindNext(S)<>0;
    SysUtils.FindClose(S);
  end;
  // dossiers
  if Recursif then
    if FindFirst(Chemin+'*.*', faDirectory, S)=0 then begin
      repeat
        if (S.Name<>'.') And (S.Name<>'..') then
          If (S.Attr And faDirectory)<>0 then
              SearchFiles(Chemin+S.FindData.cFileName, Extension, Recursif, Files);
      until FindNext(S)<>0;
      SysUtils.FindClose(S);
    end;
end;
//--------------------------------------------------------------------
function IsFloat(S: String) : Boolean;
var
  aNo:real;
  err:integer;
begin
  val(S,aNo,err);
  if err=0 then result:=true else
  result:=false;
end;
//--------------------------------------------------------------------
function IsInteger(S: String) : Boolean;
var
  aNo,err:integer;
begin
  val(S,aNo,err);
  if err=0 then result:=true else
  result:=false;
end;
//--------------------------------------------------------------------
function GetInteger(S: String) : Integer;
var
  val : string;
  i : integer;
begin
  val := '';
  for i:=1 to length(S) do
    if S[i] in ['1','2','3','4','5','6','7','8','9','0'] then
      val := val+S[i];
  if val = '' then
    val := '0';
  result := StrToInt(val);
end;
//--------------------------------------------------------------------
procedure OpenURL(URL: string);
begin
  try
    ShellExecute(GetDesktopWindow,'open',PChar(URL),nil,nil,SW_SHOWNORMAL);
  except
    showMessage('Impossible d''ouvrir le lien "'+URL+'"');
  end;
end;        
//--------------------------------------------------------------------
procedure LanceCommand(bin, params: string; Visible: boolean);
begin
  try
    if Visible then    
      ShellExecute(GetDesktopWindow,'open',PChar(bin),PChar(params),nil,SW_SHOWNORMAL)
    else
      ShellExecute(GetDesktopWindow,'open',PChar(bin),PChar(params),nil,SW_HIDE);
  except
    showMessage('Impossible de lancer la commande "'+bin+' '+params+'"');
  end;
end;
//--------------------------------------------------------------------
function Base64_Encode(S: string): string;
const
  Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;
//--------------------------------------------------------------------
function base64_Decode(S: string): string;
const
  Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + chr(x);
      end;
    end
    else
      Exit;
  end;
end;   
//--------------------------------------------------------------------
function BinToInt(Value: String): LongInt;
var i, n, p: Integer;
begin
  Result:=0;
  n := length(value);
  p := 1;
  for i:=n downto 1 do begin
   if value[i]='1' then
     Result:=Result + p ;
   p := p*2;
  end;
end;
//--------------------------------------------------------------------
function IntToBin ( value: LongInt): string;
begin
    result := '' ;
    while value > 0 do begin
      if ( value and 1 ) = 1 then
        result := result+'1'
      else                       
        result := result+'0';
      value := value shr 1;
    end;
    result := reversestring(result);
end;            
//--------------------------------------------------------------------
function IntToRoman(Value: LongInt): String;
const
Arabics: Array[1..13] of Integer = (1,4,5,9,10,40,50,90,100,400,500,900,1000) ;
Romans: Array[1..13] of String = ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M') ;
var
   j: Integer;
begin
  for j := 13 downto 1 do
  while (Value >= Arabics[j]) do begin
   Value := Value - Arabics[j];
   Result := Result + Romans[j];
  end;
end;       
//--------------------------------------------------------------------
function StrRepeat(s: string; n:integer ): string;
begin
   Result:=DupeString(s, n) ;
end;
//--------------------------------------------------------------------
function StrRev(S: string): string;
begin
   Result:=ReverseString(s) ;
end;
//--------------------------------------------------------------------
function HexToInt(HexNum: string): LongInt;
begin
   Result:=StrToInt('$' + HexNum) ;
end;


end.
