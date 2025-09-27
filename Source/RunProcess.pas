unit RunProcess;

interface

uses
  Windows, Classes, Dialogs, SysUtils, Forms, SyncObjs;

type
  TWindowStyle = (wNormal, wMinimize, wMaximize, wHide,
    wMinNoActivate, wShowNoActivate);
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TStdInfo = record
      FileName: string;
      Enabled: boolean;
      Interval: integer;   
      CaptureDatas : boolean;
      OnInputLine: TNotifyEvent;
      OnRead: TNotifyEvent;
      OnEnd: TNotifyEvent;
  end;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TStd = class(TThread)
    private
      CurrentLine: string;
      procedure EvtOnInputLine;
      procedure EvtOnRead;
      procedure EvtOnEnd;
      function Traite: cardinal;
    protected
      procedure Execute; override;
      procedure Terminate;
    public
      CaptureDatas : boolean;
      Interval: integer;
      FileName: string;
      Datas: string;
      Line: string;
      IsRunning: boolean;
      HandleR: THandle;
      HandleW: THandle;
      OnInputLine: TNotifyEvent;
      OnRead: TNotifyEvent;
      OnEnd: TNotifyEvent;
      constructor Create;
  end;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TRunProcess = class(TThread)
    private
      FStartInfo: TStartupInfo;           // Info de démarrage du processus
      FProcessInfo: TProcessInformation;  // Infos sur le processus
      StdInThread: TStd;
      StdOutThread: TStd;
      StdErrThread: TStd;
      procedure EvtOnStart;
      procedure EvtOnEnd;
    protected
    public
      ExitCode: DWORD;
      IsRunning: boolean;
      StdIn: TStdInfo;
      StdOut: TStdInfo;
      StdErr: TStdInfo;
      OnStart: TNotifyEvent;
      OnEnd: TNotifyEvent;
      WindowStyle: TWindowStyle;
      DetachedProcess: boolean;
      Commands: TStringList;
      Index: integer;
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
      procedure Terminate;
      procedure RunThread;
      property ProcessInfo: TProcessInformation read FProcessInfo;
  end;

implementation

//####################################################################
constructor TStd.Create;
begin
  FileName     := '';
  Datas        := '';
  CurrentLine  := '';
  Line         := '';
  IsRunning    := false;
  CaptureDatas := true;

  inherited Create(True);
end;
//--------------------------------------------------------------------
procedure TStd.EvtOnInputLine;
var EOL, i, N :integer;
begin
  if Assigned(OnInputLine) then begin        
    N := length(CurrentLine);
    EOL := 0;
    try
      for i:=1 to N do
        if (i > EOL) AND ((CurrentLine[i] = #13) OR (CurrentLine[i] = #10)) then begin
          Line := Copy(CurrentLine, EOL+1, i-EOL-1);
          OnInputLine(Self);
          EOL := i;
          if (EOL < N) AND (CurrentLine[EOL+1] = #10) then inc(EOL);
        end;
      if EOL<>0 then Delete(CurrentLine, 1, EOL);
    except

    end;
  end;
end;
//--------------------------------------------------------------------
procedure TStd.EvtOnRead;
begin
  if Assigned(OnRead) then OnRead(Self);
end;
//--------------------------------------------------------------------
procedure TStd.EvtOnEnd;
begin
  if Assigned(OnEnd) then OnEnd(Self);
end;
//--------------------------------------------------------------------
procedure TStd.Terminate;
begin
  Traite;                           
//  if Assigned(OnEnd) then Synchronize(EvtOnEnd);
//  if Assigned(OnInputLine) then Synchronize(EvtOnInputLine);
//  if Assigned(OnRead) then Synchronize(EvtOnRead);
  IsRunning := False;
  inherited Terminate;
end;                  
//--------------------------------------------------------------------
function TStd.Traite: cardinal;
const
  BufSize = 1024;
var
  Buffer: array[0..BufSize] of char;
begin      
  Result := 0;
  if PeekNamedPipe(HandleR,@Buffer,BufSize,nil,@Result,nil) then begin
    if (Result>0) then begin
      ReadFile(HandleR,Buffer[0],BufSize,Result,nil);
      Buffer[Result] := #0;
      OemToAnsi(Buffer, Buffer);
      if CaptureDatas then Datas := Datas + Buffer;
      if Assigned(OnInputLine) then begin
        CurrentLine := CurrentLine + Buffer;
        Synchronize(EvtOnInputLine);
      end;
      if Assigned(OnRead) then Synchronize(EvtOnRead);
    end else begin
      Result := 1;
      Sleep(Interval);
    end;
    if ((Result = 0) or Terminated) and Assigned(OnEnd) then Synchronize(EvtOnEnd);
  end;
end;
//--------------------------------------------------------------------
procedure TStd.Execute;
var
  BytesRead: cardinal;
begin
  IsRunning := True;
  Datas := '';
  repeat
    BytesRead := Traite;
  until (BytesRead = 0) or Terminated;
  CloseHandle(HandleR) ;
  CloseHandle(HandleW) ;
  IsRunning := False;
end;

//####################################################################
procedure TRunProcess.RunThread;
begin
  Resume;
end;
//--------------------------------------------------------------------
constructor TRunProcess.Create;
begin
  WindowStyle := wNormal;
  DetachedProcess := False;

  StdOut.CaptureDatas := True;
  StdErr.CaptureDatas := True;
  StdIn.CaptureDatas := True;

  StdOut.Interval := 30;
  StdErr.Interval := 30;
  StdIn.Interval := 30;
  
  Commands := TStringList.Create;

  IsRunning := False;
  ExitCode := 0;
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpNormal;
end;
//--------------------------------------------------------------------
destructor TRunProcess.Destroy;
begin
  StdOutThread.Free;
  StdInThread.Free;
  StdErrThread.Free;
  Commands.Free;
  inherited Destroy;
end;
//--------------------------------------------------------------------
procedure TRunProcess.Terminate;
begin
  TerminateProcess(FProcessInfo.hProcess, 1);
  try
  if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.Terminate;
  if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.Terminate;
  if StdIn.Enabled and StdInThread.IsRunning   then StdInThread.Terminate;

  if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.WaitFor;
  if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.WaitFor;
  if StdIn.Enabled and StdInThread.IsRunning   then StdInThread.WaitFor;

  if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.Free;
  if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.Free;
  if StdIn.Enabled and StdInThread.IsRunning   then StdInThread.Free;
  finally

  end;
  IsRunning := False;
  inherited Terminate;
end;
//--------------------------------------------------------------------
procedure TRunProcess.EvtOnStart;
begin
  if Assigned(OnStart) then OnStart(Self);
end;
//--------------------------------------------------------------------
procedure TRunProcess.EvtOnEnd;
begin
  if Assigned(OnEnd) then OnEnd(Self);
end;
//--------------------------------------------------------------------
procedure TRunProcess.Execute;
var
  Options: DWord;
  i: integer;    
  Security: TSecurityAttributes;
begin
  if Commands.Count = 0 then
    raise Exception.Create('You lust have at least one command !');  

  {>> Ne pas executer plus d'une chose à la fois par composant }
  if IsRunning then
    raise Exception.Create('Process is already running');

  {>> flag de run ou pas }
  IsRunning := True;

  {>> Remplit les infos }
  FillChar(FStartInfo, SizeOf(TStartupInfo), 0);
  FillChar(FProcessInfo, sizeOf(TProcessInformation), 0);
  FStartInfo.cb := SizeOf(TStartupInfo);

  {>> Window Style }
  FStartInfo.dwFlags := STARTF_USESHOWWINDOW;
  Case WindowStyle of
    wNormal:         FStartInfo.wShowWindow := SW_SHOWNORMAL;
    wMinimize:       FStartInfo.wShowWindow := SW_SHOWMINIMIZED;
    wMaximize:       FStartInfo.wShowWindow := SW_SHOWMAXIMIZED;
    wHide:           FStartInfo.wShowWindow := SW_HIDE;
    wMinNoActivate:  FStartInfo.wShowWindow := SW_SHOWMINNOACTIVE;
    wShowNoActivate: FStartInfo.wShowWindow := SW_SHOWNA;
    else
      FStartInfo.wShowWindow := SW_HIDE;
  end;

  {>> Priority }
  case Priority of
    tpIdle:         Options := 64;
    tpLowest:       Options := 16384;
    tpLower:        Options := 16384;
    tpNormal:       Options := 32;
    tpHigher:       Options := 32768;
    tpHighest:      Options := 128;
    tpTimeCritical: Options := 256;
    else
      Options := 32;
  end;

  {>> Detached Process }
  if DetachedProcess then Options := Options + DETACHED_PROCESS;

  {>> Init Pipes }
  FStartInfo.hStdOutput := 0;
  FStartInfo.hStdError := 0;
  FStartInfo.hStdInput := 0;

  {>> init pour les redirections stdin, ... }
  with Security do begin
    nlength := SizeOf(TSecurityAttributes) ;
    binherithandle := true;
    lpsecuritydescriptor := nil;
  end;

  if StdOut.Enabled or StdErr.Enabled or StdIn.Enabled then
    FStartInfo.dwFlags := FStartInfo.dwFlags + STARTF_USESTDHANDLES;

  {>> Démarre les processus }
  for i := 0 to Commands.Count - 1 do begin
    Index := i;

    {>> Pipe Std OUT }
    with StdOut do
    if Enabled then begin
      StdOutThread := TStd.Create;
      StdOutThread.Interval := Interval;
      StdOutThread.CaptureDatas := CaptureDatas;
      CreatePipe(StdOutThread.HandleR, StdOutThread.HandleW, @Security, 0);
      FStartInfo.hStdOutput := StdOutThread.HandleW;
      if Assigned(OnInputLine) then StdOutThread.OnInputLine := OnInputLine;
      if Assigned(OnRead)      then StdOutThread.OnRead      := OnRead;
      if Assigned(OnEnd)       then StdOutThread.OnEnd       := OnEnd;
    end;

    {>> Pipe Std ERR }
    with StdErr do
    if Enabled then begin
      StdErrThread := TStd.Create;
      StdErrThread.Interval := Interval;
      StdErrThread.CaptureDatas := CaptureDatas;
      CreatePipe(StdErrThread.HandleR, StdErrThread.HandleW, @Security, 0);
      FStartInfo.hStdError := StdErrThread.HandleW;
      if Assigned(OnInputLine) then StdErrThread.OnInputLine := OnInputLine;
      if Assigned(OnRead)      then StdErrThread.OnRead      := OnRead;
      if Assigned(OnEnd)       then StdErrThread.OnEnd       := OnEnd;
    end;

    {>> Pipe Std IN }
    with StdIn do
    if Enabled then begin
      StdInThread := TStd.Create;
      StdInThread.Interval := Interval;
      StdInThread.CaptureDatas := CaptureDatas;
      CreatePipe(StdInThread.HandleW, StdInThread.HandleR, @Security, 0);
      FStartInfo.hStdInput := StdInThread.HandleR;
      if Assigned(OnInputLine) then StdInThread.OnInputLine := OnInputLine;
      if Assigned(OnRead)      then StdInThread.OnRead      := OnRead;
      if Assigned(OnEnd)       then StdInThread.OnEnd       := OnEnd;
    end;
                     
    {>> Event de début }
    if Assigned(OnStart) then Synchronize(EvtOnStart);

    if CreateProcess(nil, PChar(Commands[i]), @Security, @Security, True,
      Options, nil, nil, FStartInfo, FProcessInfo) then
    begin
      {>> Lance les threads si besoin }
      if StdOut.Enabled then StdOutThread.Resume;
      if StdErr.Enabled then StdErrThread.Resume;
      if StdIn.Enabled  then StdInThread.Resume;

      {>> attend la fin du processus }
      WaitForSingleObject(FProcessInfo.hProcess, INFINITE) ;

      {>> code retour }
      GetExitCodeProcess(FProcessInfo.hProcess, ExitCode);
    end;

    {>> ferme les sous-threads }                   
    if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.Terminate;
    if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.Terminate;
    if StdIn.Enabled  and StdInThread.IsRunning  then StdInThread.Terminate;

    if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.WaitFor;
    if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.WaitFor;
    if StdIn.Enabled  and StdInThread.IsRunning  then StdInThread.WaitFor;

    if StdOut.Enabled and StdOutThread.IsRunning then StdOutThread.Free;
    if StdErr.Enabled and StdErrThread.IsRunning then StdErrThread.Free;
    if StdIn.Enabled  and StdInThread.IsRunning  then StdInThread.Free;

    {>> Event de fin }
    if Assigned(OnEnd) then Synchronize(EvtOnEnd);

    if Terminated then Break;
  end; // for 
  IsRunning := False;
end;

end.


