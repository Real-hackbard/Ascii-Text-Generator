unit RegTools;

interface

uses Registry, Windows, SysUtils, Classes, Forms, StrUtils, Common;
// - - - - - - - - - - - - - - - - - - - - - - - -
type
  TRegTool = class(TRegistry)
    private
    protected
    public
      function DeleteKeyTree(Key: string) : boolean;
  end;
// - - - - - - - - - - - - - - - - - - - - - - - -
procedure RegFormSave(Form: TForm; HKRoot: Cardinal; Path: string);
procedure RegFormSaveCU(Form: TForm; Path: string);
procedure RegFormSaveLM(Form: TForm; Path: string);

procedure RegFormLoad(Form: TForm; HKRoot: Cardinal; Path: string);
procedure RegFormLoadCU(Form: TForm; Path: string);
procedure RegFormLoadLM(Form: TForm; Path: string);

procedure RegWriteKey(HKRoot: Cardinal; Path, Key, Value: string); overload;
procedure RegWriteKeyCU(Path, Key, Value: string); overload;
procedure RegWriteKeyLM(Path, Key, Value: string); overload;
procedure RegWriteKey(HKRoot: Cardinal; Path, Key: string; Value: integer); overload;
procedure RegWriteKeyCU(Path, Key: string; Value: integer); overload;
procedure RegWriteKeyLM(Path, Key: string; Value: integer); overload;

function RegReadKey(HKRoot: Cardinal; Path, Key: string): string; overload;
function RegReadKeyCU(Path, Key: string): string; overload;
function RegReadKeyLM(Path, Key: string): string; overload;

function RegReadSubKeys(HKRoot: Cardinal; Path: string) : TStringList;
function RegReadSubKeysCU(Path: string) : TStringList;
function RegReadSubKeysLM(Path: string) : TStringList;

function RegReadSubValues(HKRoot: Cardinal; Path: string) : TStringList;
function RegReadSubValuesCU(Path: string) : TStringList;
function RegReadSubValuesLM(Path: string) : TStringList;

implementation
     
//-----------------------------------------------------------------------------
function RegReadSubValuesCU(Path: string) : TStringList;
begin
  Result := RegReadSubValues(HKEY_CURRENT_USER, Path);
end;
//-----------------------------------------------------------------------------
function RegReadSubValuesLM(Path: string) : TStringList;
begin
  Result := RegReadSubValues(HKEY_LOCAL_MACHINE, Path);
end;
//-----------------------------------------------------------------------------
function RegReadSubValues(HKRoot: Cardinal; Path: string) : TStringList;
var
  Reg: TRegTool;
begin
  Result := TStringList.Create;
  Reg := TRegTool.Create;
  with Reg Do try      
    RootKey := HKRoot;
    if OpenKey(Path, False) then begin
      GetValueNames(Result);
      CloseKey;
    end;
    Free;
  except
    Free
  end;
end;
//-----------------------------------------------------------------------------
function RegReadSubKeysCU(Path: string) : TStringList;
begin
  Result := RegReadSubKeys(HKEY_CURRENT_USER, Path);
end;
//-----------------------------------------------------------------------------
function RegReadSubKeysLM(Path: string) : TStringList;
begin
  Result := RegReadSubKeys(HKEY_LOCAL_MACHINE, Path);
end;
//-----------------------------------------------------------------------------
function RegReadSubKeys(HKRoot: Cardinal; Path: string) : TStringList;
var
  Reg: TRegTool;
begin
  Result := TStringList.Create;
  Reg := TRegTool.Create;
  with Reg Do try      
    RootKey := HKRoot;
    if OpenKey(Path, False) then begin
      GetKeyNames(Result);
      CloseKey;
    end;
    Free;
  except
    Free
  end;
end;
//-----------------------------------------------------------------------------
procedure RegFormSaveLM(Form: TForm; Path: string);
begin
  RegFormSave(Form, HKEY_LOCAL_MACHINE, Path);
end;
//-----------------------------------------------------------------------------
procedure RegFormSaveCU(Form: TForm; Path: string);
begin
  RegFormSave(Form, HKEY_CURRENT_USER, Path);
end;
//-----------------------------------------------------------------------------
procedure RegFormSave(Form: TForm; HKRoot: Cardinal; Path: string);
var
  Reg: TRegTool;
begin
  Reg := TRegTool.Create;
  with Reg Do try
    RootKey := HKRoot;
    if OpenKey(Path, True) then begin
      case Form.WindowState of
        wsNormal: RegWriteKey(HKRoot, Path, 'WindowState', 'Normal');
        wsMinimized: RegWriteKey(HKRoot, Path, 'WindowState', 'Minimized');
        wsMaximized: RegWriteKey(HKRoot, Path, 'WindowState', 'Maximized');
      end;
      RegWriteKey(HKRoot, Path, 'WindowTop', IntToStr(Form.Top));
      RegWriteKey(HKRoot, Path, 'WindowLeft', IntToStr(Form.Left));  
      RegWriteKey(HKRoot, Path, 'WindowWidth', IntToStr(Form.Width));
      RegWriteKey(HKRoot, Path, 'WindowHeight', IntToStr(Form.Height));
      CloseKey;
    end;
    Free;
  except
    Free;
  end;
end;        
//-----------------------------------------------------------------------------
procedure RegFormLoadLM(Form: TForm; Path: string);
begin
  RegFormLoad(Form, HKEY_LOCAL_MACHINE, Path);
end;
//-----------------------------------------------------------------------------
procedure RegFormLoadCU(Form: TForm; Path: string);
begin
  RegFormLoad(Form, HKEY_CURRENT_USER, Path);
end;
//-----------------------------------------------------------------------------
procedure RegFormLoad(Form: TForm; HKRoot: Cardinal; Path: string);
var
  Val: string;
begin         
  Val := RegReadKey(HKRoot, Path, 'WindowState');
  case AnsiIndexStr(Val, ['Normal', 'Minimized', 'Maximized']) of
    0: Form.WindowState := wsNormal;
    1: Form.WindowState := wsMinimized;
    2: Form.WindowState := wsMaximized;
  end;
  if Val = 'Normal' then begin   
    Val := RegReadKey(HKRoot, Path, 'WindowTop');
    if IsInteger(Val) then Form.Top := StrToInt(Val);
    Val := RegReadKey(HKRoot, Path, 'WindowLeft');
    if IsInteger(Val) then Form.Left := StrToInt(Val);
    Val := RegReadKey(HKRoot, Path, 'WindowWidth');
    if IsInteger(Val) then Form.Width := StrToInt(Val);
    Val := RegReadKey(HKRoot, Path, 'WindowHeight');
    if IsInteger(Val) then Form.Height := StrToInt(Val);
    Val := RegReadKey(HKRoot, Path, 'WindowState');
  end;
end;
//-----------------------------------------------------------------------------
procedure RegWriteKeyLM(Path, Key: string; Value: integer); overload;
begin
  RegWriteKey(HKEY_LOCAL_MACHINE, Path, Key, IntToStr(Value));
end;
//-----------------------------------------------------------------------------
procedure RegWriteKeyCU(Path, Key: string; Value: integer); overload;
begin
  RegWriteKey(HKEY_CURRENT_USER, Path, Key, IntToStr(Value));
end;
//-----------------------------------------------------------------------------
procedure RegWriteKey(HKRoot: Cardinal; Path, Key: string; Value: integer); overload;
begin
  RegWriteKey(HKRoot, Path, Key, IntToStr(Value));
end;
//-----------------------------------------------------------------------------
procedure RegWriteKeyLM(Path, Key, Value: string); overload;
begin
  RegWriteKey(HKEY_LOCAL_MACHINE, Path, Key, Value);
end;
//-----------------------------------------------------------------------------
procedure RegWriteKeyCU(Path, Key, Value: string); overload;
begin
  RegWriteKey(HKEY_CURRENT_USER, Path, Key, Value);
end;
//-----------------------------------------------------------------------------
procedure RegWriteKey(HKRoot: Cardinal; Path, Key, Value: string); overload;
var
  Reg: TRegTool;
begin
  Reg := TRegTool.Create;
  with Reg Do try
    RootKey := HKRoot;
    if OpenKey(Path, True) then begin
      WriteString(Key, Value);
      CloseKey;
    end; 
    Free;
  except
    Free;
  end;
end;
//-----------------------------------------------------------------------------
function RegReadKeyLM(Path, Key: string): string; overload;
begin
  Result := RegReadKey(HKEY_LOCAL_MACHINE, Path, Key);
end;
//-----------------------------------------------------------------------------
function RegReadKeyCU(Path, Key: string): string; overload;
begin
  Result := RegReadKey(HKEY_CURRENT_USER, Path, Key);
end;
//-----------------------------------------------------------------------------
function RegReadKey(HKRoot: Cardinal; Path, Key: string): string; overload;
var
  Reg: TRegTool;
begin
  Reg := TRegTool.Create;
  Result := '';
  with Reg Do try
    RootKey := HKRoot;
    if OpenKey(Path, True) then begin
      if ValueExists(Key) then Result := ReadString(Key);
      CloseKey;
    end;
    Free;
  except
    Free;
  end;
end;
//-----------------------------------------------------------------------------
function TRegTool.DeleteKeyTree(Key: string) : boolean;
var
  list: TStringList;
  i: Integer;
begin
  Result:= True;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if KeyExists(key) then
        begin
          OpenKey(key, false); // False -> ne pas créer la clé si elle n'existe pas.
          if HasSubKeys then
            begin
              list:= TStringList.Create;
              try
                GetKeyNames(list);
                CloseKey;
                for i:=0 to list.Count-1 do
                  Result:= Result and DeleteKeyTree(key+'\'+list.Strings[i]);
                Result:= Result and DeleteKey(key);
                list.Free;
              except
                list.Free;
                CloseKey;
                raise;
              end;
            end
          else
            begin
              CloseKey;
              Result:= DeleteKey(key);
            end;
          CloseKey; // Juste au cas où.
        end
      else
        Result:= False; // La clé n'existe pas.
    end
  else // Win32s, Win95, Win98, Win00?
    DeleteKey(key);
end;

end.
