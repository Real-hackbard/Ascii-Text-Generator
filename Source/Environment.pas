unit Environment;

interface
uses
  Classes;
        
{ GetCurrentProcessEnvVar retourne la valeur de la variable d'environnement }
{ VariableName du processus courant ou une chaîne vide si cette variable    }
{ n'existe pas.                                                             }
function GetCurrentProcessEnvVar(const VariableName: string): string;

{ SetCurrentProcessEnvVar créé ou change la valeur de la variable     }
{ d'environnement VariableName du processus courant en VariableValue. }
procedure SetCurrentProcessEnvVar(const VariableName, VariableValue: string);

{ DeleteCurrentProcessEnvVar supprime la variable d'environnement VariableName }
{ de l'environnement du processus courant.                                     }
procedure DeleteCurrentProcessEnvVar(const VariableName: string);

{ ReadCurrentProcessEnvVars rempli la liste Vars avec l'ensemble des variables }
{  d'environnement du processus courant. Chaque ligne est de la forme          }
{  variable=valeur                                                             }
procedure ReadCurrentProcessEnvVars(Vars: TStrings);

{ BuildEnvironmentBlock construit un bloc d'environnement en utilisant une    }
{ liste de variables d'environnement fournie par Vars. Le bloc est copié dans }
{ Buffer et le nombre de caractères copiés est retourné comme résultat.       }
{ L'argument Len est utilisé à l'appel pour spécifier la taille du buffer en  }
{ nombre de caratères et contient après l'appel la taille minimale du buffer  }
{ pour contenir le bloc en entier.                                            }
{ Cette fonction est utilisée en 2 appels: le premier permet d'obtenir la     }
{ taille du buffer à allouer et le deuxième appel rempli le buffer.           }
function BuildEnvironmentBlock(Vars: TStrings; Buffer: PAnsiChar;
  var Len: Longword): LongWord;

{ ReadSystemVars rempli la liste Vars avec l'ensemble des variables système. }
procedure ReadSystemEnvVars(Vars: TStrings);

{ ReadSystemVars rempli la liste Vars avec l'ensemble des variables utilisateur. }
procedure ReadUserEnvVars(Vars: TStrings);

{ ReadSystemVars rempli la liste Vars avec l'ensemble des variables par défaut. }
procedure ReadDefaultEnvVars(Vars: TStrings);

{ SetSystemEnvVar modifie la valeur d'une variable système et informe          }
{ l'ensemble des applications que l'environnement a changé si BroadcastChange  }
{ vaut vrai.                                                                   }
procedure SetSystemEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);

{ DeleteSystemEnvVar supprime une variable système et informe l'ensemble des }
{ applications que l'environnement a changé si BroadcastChange vaut vrai.    }
procedure DeleteSystemEnvVar(const VariableName: string;
  const BroadCastChange: boolean);

{ SetUserEnvVar modifie la valeur d'une variable utilisateur et informe       }
{ l'ensemble des applications que l'environnement a changé si BroadcastChange }
{ vaut vrai.                                                                  }
procedure SetUserEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);

{ DeleteUserEnvVar supprime une variable utilisateur et informe l'ensemble des }
{ applications que l'environnement a changé si BroadcastChange vaut vrai.      }
procedure DeleteUserEnvVar(const VariableName: string;
  const BroadcastChange: boolean);

{ SetDefaultEnvVar modifie la valeur d'une variable par défaut et informe     }
{ l'ensemble des applications que l'environnement a changé si BroadcastChange }
{  vaut vrai.                                                                 }
procedure SetDefaultEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);

{ DeleteDefaultEnvVar supprime une variable par défaut et informe l'ensemble  }
{ des applications que l'environnement a changé si BroadcastChange vaut vrai. }
procedure DeleteDefaultEnvVar(const VariableName: string;
  const BroadcastChange: boolean);

{ BroadcastEnvironmentChange informe l'ensemble des applications que }
{ l'environnement a changé.                                          }
procedure BroadcastEnvironmentChange(TimeOut: Cardinal = 2000);


function ExpandEnvVars(const Str: string): string;
function GetEnvVarValue(const VarName: string): string;
function SetEnvVarValue(const VarName,
  VarValue: string): Integer;

implementation
uses
  Windows, SysUtils, Registry, Messages;

function SetEnvVarValue(const VarName,
  VarValue: string): Integer;
begin
  // Simply call API function
  if SetEnvironmentVariable(PChar(VarName),
    PChar(VarValue)) then
    Result := 0
  else
    Result := GetLastError;
end;

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  // Get required buffer size (inc. terminal #0)
  BufSize := GetEnvironmentVariable(
    PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName),
      PChar(Result), BufSize);
  end
  else
    // No such environment variable
    Result := '';
end;

function ExpandEnvVars(const Str: string): string;
var
  BufSize: Integer; // size of expanded string
begin
  // Get required buffer size 
  BufSize := ExpandEnvironmentStrings(
    PChar(Str), nil, 0);
  if BufSize > 0 then begin
    // Read expanded string into result string
    SetLength(Result, BufSize); 
    ExpandEnvironmentStrings(PChar(Str), 
      PChar(Result), BufSize);
  end
  else
    // Trying to expand empty string
    Result := '';
end;

function GetCurrentProcessEnvVar(const VariableName: string): string;
var
  nSize: DWord;
begin
  nSize:= 0;
  nSize:= GetEnvironmentVariable(PChar(VariableName), nil, nSize);
  if nSize = 0 then
    result:= ''
  else
  begin
    SetLength(result, nSize - 1);
    if GetEnvironmentVariable(PChar(VariableName), PChar(result), nSize) <> nSize - 1 then
      raise Exception.Create(SysErrorMessage(GetlastError))
  end;
end;

procedure SetCurrentProcessEnvVar(const VariableName, VariableValue: string);
begin
  SetEnvironmentVariable(PChar(VariableName), PChar(VariableValue));
end;

procedure DeleteCurrentProcessEnvVar(const VariableName: string);
begin
  SetCurrentProcessEnvVar(VariableName, '');
end;

procedure ReadEnvironmentBlock(const Block: PAnsiChar; Vars: TStrings);
var
  i: Integer;
  s: string;
begin
  Vars.Clear;
  i:= 0;
  while Block[i] <> #0 do
  begin
    s:= '';
    while Block[i] <> #0 do
    begin
      s:= s + Block[i];
      Inc(i);
    end;
    Vars.Add(s);
    Inc(i);
  end;
end;
        
procedure ReadCurrentProcessEnvVars(Vars: TStrings);
var
  block: PAnsiChar;
begin
  block:= GetEnvironmentStrings;
  try
    ReadEnvironmentBlock(block, Vars);
  finally
    FreeEnvironmentStrings(block);
  end;
end;
        
function BuildEnvironmentBlock(Vars: TStrings; Buffer: PAnsiChar;
  var Len: Longword): LongWord;
var
  line, i: Integer;
  p, l: LongWord;
begin
  p:= 0;
  l:= p;
  for line:= 0 to Vars.Count - 1 do
  begin
    for i:= 1 to Length(Vars[line]) do
    begin
      if p < Len then
      begin
        Buffer[p]:= Vars[line][i];
        Inc(l);
      end;
      Inc(p);
    end;
    if p < Len then
    begin
      Buffer[p]:= #0;
      Inc(l);
    end;
    Inc(p);
  end;
  if p < Len then
  begin
    Buffer[p]:= #0;
    Inc(l);
  end;
  result:= l;
  Len:= p;
end;
        
procedure ReadSystemEnvVars(Vars: TStrings);
var
  i: Integer;
begin
  Vars.Clear;
  with TRegistry.Create do
  try
    Access:= KEY_READ;
    RootKey:= HKEY_LOCAL_MACHINE;
    if OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment',
      false) then
    begin
      GetValueNames(Vars);
      for i:= Vars.Count - 1 downto 0 do
        try
          Vars[i]:= Vars[i] + '=' + ReadString(Vars[i]);
        except
          Vars.Delete(i);
        end;
      CloseKey;
    end
    else
      raise Exception.Create('Impossible d''ouvrir la clé.');
  finally
    Free;
  end;
end;

procedure ReadUserEnvVars(Vars: TStrings);
var
  i: Integer;
begin
  Vars.Clear;
  with TRegistry.Create do
  try
    Access:= KEY_READ;
    RootKey:= HKEY_CURRENT_USER;
    if OpenKey('Environment', false) then
    begin
      GetValueNames(Vars);
      for i:= Vars.Count - 1 downto 0 do
        try
          Vars[i]:= Vars[i] + '=' + ReadString(Vars[i]);
        except
          Vars.Delete(i);
        end;
      CloseKey;
    end
    else
      raise Exception.Create('Impossible d''ouvrir la clé.');
  finally
    Free;
  end;
end;
        
procedure ReadDefaultEnvVars(Vars: TStrings);
var
  i: Integer;
begin
  Vars.Clear;
  with TRegistry.Create do
  try
    Access:= KEY_READ;
    RootKey:= HKEY_USERS;
    if OpenKey('.DEFAULT\Environment', false) then
    begin
      GetValueNames(Vars);
      for i:= Vars.Count - 1 downto 0 do
        try
          Vars[i]:= Vars[i] + '=' + ReadString(Vars[i]);
        except
          Vars.Delete(i);
        end;
      CloseKey;
    end
    else
      raise Exception.Create('Impossible d''ouvrir la clé.');
  finally
    Free;
  end;
end;
        
procedure SetSystemEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);
begin
  with TRegistry.Create do
  try
    Access:= KEY_WRITE;
    RootKey:= HKEY_LOCAL_MACHINE;
    if OpenKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment',
      false) then
    begin
      if (VariableValue = '') then
        DeleteValue(VariableName)
      else
        WriteString(VariableName, VariableValue);
      if BroadcastChange then
        BroadcastEnvironmentChange;
    end;
  finally
    Free;
  end;
end;
        
procedure SetUserEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);
begin
  with TRegistry.Create do
  try
    Access:= KEY_WRITE;
    RootKey:= HKEY_CURRENT_USER;
    if OpenKey('Environment', false) then
    begin
      if (VariableValue = '') then
        DeleteValue(VariableName)
      else
        WriteString(VariableName, VariableValue);
      if BroadcastChange then
        BroadcastEnvironmentChange;
    end;
  finally
    Free;
  end;
end;
        
procedure SetDefaultEnvVar(const VariableName, VariableValue: string;
  const BroadcastChange: boolean);
begin
  with TRegistry.Create do
  try
    Access:= KEY_WRITE;
    RootKey:= HKEY_USERS;
    if OpenKey('.DEFAULT\Environment', false) then
    begin
      if (VariableValue = '') then
        DeleteValue(VariableName)
      else
        WriteString(VariableName, VariableValue);
      if BroadcastChange then
        BroadcastEnvironmentChange;
    end;
  finally
    Free;
  end;
end;

procedure BroadcastEnvironmentChange(TimeOut: Cardinal = 2000);
var
  lParam: PChar;
  dwResult: Cardinal;
begin
  lParam:= 'Environment';
  SendMessageTimeOut(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
    Integer(lParam), SMTO_NORMAL	, TimeOut, dwResult);
  if dwResult <> 0 then
    raise Exception.Create(SysErrorMessage(dwResult));
end;

procedure DeleteSystemEnvVar(const VariableName: string;
  const BroadCastChange: boolean);
begin
  SetSystemEnvVar(VariableName, '', BroadCastChange);
end;

procedure DeleteUserEnvVar(const VariableName: string;
  const BroadCastChange: boolean);
begin
  SetUserEnvVar(VariableName, '', BroadCastChange);
end;

procedure DeleteDefaultEnvVar(const VariableName: string;
  const BroadCastChange: boolean);
begin
  SetDefaultEnvVar(VariableName, '', BroadCastChange);
end;

end.

