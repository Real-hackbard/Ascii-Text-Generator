unit FIGlet;

interface

uses SysUtils, Classes;

type
  TFIGlet = class
    private
      sFontFile: string;
      Lines: TStringList;
      function getFontFile: string;     
      function getCommentText: string;
      procedure setFontFile(FileName:string);
      procedure LoadFont;
      procedure Split (const Delimiter: Char; Input: string;
        const Strings: TStrings) ;
      procedure Init;
      function getCharacter(car: string): TStringList;
      function fetchLine(Line: string): string;
      function GetInteger(S: String): Integer;
    protected
    public
      Signature: string;
      HardBlank: string;
      Height: integer;
      BaseLine: integer;
      MaxLength: integer;
      OldLayout: integer;
      CommentLines: integer;
      PrintDirection: integer;
      FullLayout: integer;
      CodeTagCount: string;    
      property CommentText: string
        read getCommentText;
      property FontFile: string
        read getFontFile
        write setFontFile;
      function getString(texte:string): string;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

const
  CR   = #13;
  LF   = #10;
  CRLF = CR+LF;

//-------------------------------------------------------
function TFIGlet.getCommentText: string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to CommentLines do
    Result := Result + Lines[i] + CRLF;
end;
//-------------------------------------------------------
function TFIGlet.fetchLine(Line: string): string;
var
  Character: TStringList; 
  subLines: TStringList;
  i, c: integer;
begin
  subLines := TStringList.Create;
  for i := 0 to Height - 1 do begin
    subLines.Add('');
  end;
  for c := 1 to length(Line) do begin
    Character := getCharacter(Line[c]);
    for i := 0 to Height - 1 do begin
      subLines[i] := subLines[i] + Character[i];
    end;
  end;
  Result := '';
  for i := 0 to Height - 1 do begin
    Result := Result + subLines[i] + CRLF;
  end;
  subLines.Free;
end;
//-------------------------------------------------------
function TFIGlet.getCharacter(car: string): TStringList;
var
  AsciiValue: integer;
  Start: integer;
  a, i: integer;
  tmp : string;
begin
  Result := TStringList.Create;
  AsciiValue := Ord(Car[1]);
  if AsciiValue - 32 < 102 then begin
    // caractère "standard"
  	Start := CommentLines + (AsciiValue - 32) * Height + 1;
  	for a := 0 to Height - 1 do begin
      if Lines.Count > Start + a then begin
  			tmp := Lines[Start + a];
  			tmp := StringReplace(tmp, '@', '', [rfReplaceAll]);
  			tmp := StringReplace(tmp, Hardblank, ' ', [rfReplaceAll]);
  		end else begin
        tmp := '';
      end;
      Result.Add(tmp);
    end;
  end else begin
    // caractère étendu
    Start := CommentLines + 102 * Height + 1;
    for i := Start to Lines.Count - 1 do begin
      if Pos('@', Lines[i]) = 0 then begin
        if Pos(' ', Lines[i]) > 0 then begin

          tmp := Trim(Copy(Lines[i], 1, Pos(' ', Lines[i])));
          if StrToIntDef(tmp, 0) = AsciiValue then begin
          	for a := 1 to Height do begin
              if Lines.Count > i + a then begin
          			tmp := Lines[i + a];
          			tmp := StringReplace(tmp, '@', '', [rfReplaceAll]);
          			tmp := StringReplace(tmp, Hardblank, ' ', [rfReplaceAll]);
          		end else begin
                tmp := '';
              end;
              Result.Add(tmp);
            end; // for
          end;

        end;
      end;
    end; // for
  end;
  if Result.Count <> Height then begin
    Result.Clear;
  	for a := 0 to Height - 1 do Result.Add('');
  end;
end;
//-------------------------------------------------------
procedure TFIGlet.Split (const Delimiter: Char;
  Input: string; const Strings: TStrings) ;
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
//-------------------------------------------------------
procedure TFIGlet.Init;
begin
  Signature      := '';
  HardBlank      := '';
  Height         := 0;
  BaseLine       := 0;
  MaxLength      := 0;
  OldLayout      := 0;
  CommentLines   := 0;
  PrintDirection := 0;
  FullLayout     := 0;
  CodeTagCount   := '';
  Lines.Clear;
end;
//-------------------------------------------------------
constructor TFIGlet.Create;
begin         
  sFontFile  := '';
  Lines      := TStringList.Create;
  Init;
  inherited Create;
end;
//-------------------------------------------------------
destructor TFIGlet.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;
//-------------------------------------------------------
procedure TFIGlet.LoadFont;
var
  Liste: TStringList;
begin         
  if FileExists(sFontFile) then begin
    // init                            
    Init;
    // load file
    Lines.LoadFromFile(sFontFile);
    // analyse    
    Liste := TStringList.Create;
    try
      Split(' ', Lines[0], Liste);
      if Liste.Count>0 then Signature      := Copy(Liste[0], 1, Length(Liste[0])-1);
      if Liste.Count>0 then HardBlank      := Copy(Liste[0], Length(Liste[0]), 1);
      if Liste.Count>1 then Height         := StrToInt(Liste[1]);
      if Liste.Count>2 then BaseLine       := GetInteger(Liste[2]);
      if Liste.Count>3 then MaxLength      := GetInteger(Liste[3]);
      if Liste.Count>4 then OldLayout      := GetInteger(Liste[4]);
      if Liste.Count>5 then CommentLines   := GetInteger(Liste[5]);
      if Liste.Count>6 then PrintDirection := GetInteger(Liste[6]);
      if Liste.Count>7 then FullLayout     := GetInteger(Liste[7]);
      if Liste.Count>8 then CodeTagCount   := Liste[8];
    finally       
      Liste.Free;
    end;
    if Signature <> 'flf2a' then Init;    
  end;
end;         
//-------------------------------------------------------
function TFIGlet.GetInteger(S: String) : Integer;
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
//-------------------------------------------------------
function TFIGlet.getString(texte:string): string;
var
  i: integer; 
  Liste: TStringList;
begin
  Result := '';
  Liste  := TStringList.Create;
  Split(CR, StringReplace(texte, LF, '', [rfReplaceAll]), Liste);
  for i := 0 to Liste.Count - 1 do begin
    Result := Result+fetchLine(Liste[i])+CRLF;
  end;
  Liste.Free;
end;
//-------------------------------------------------------
function TFIGlet.getFontFile: string;
begin
  Result := sFontFile;
end;
//-------------------------------------------------------
procedure TFIGlet.setFontFile(FileName:string);
begin
  if FileExists(FileName) then begin
    sFontFile := FileName;
    LoadFont;
  end;
end;
end.
