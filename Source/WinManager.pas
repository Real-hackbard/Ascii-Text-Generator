unit WinManager;

interface
  uses Windows, Forms;

procedure WinCreate(hWnd: HWND);
procedure WinTaskbarShow(hWnd: HWND);    
procedure ShowModal(frm, self:TForm);

function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): Integer; stdcall;
function GetWindowLongPtrA(hWnd: HWND; nIndex: Integer): Integer; stdcall;
function GetWindowLongPtrW(hWnd: HWND; nIndex: Integer): Integer; stdcall;
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: Integer): Integer; stdcall;
function SetWindowLongPtrA(hWnd: HWND; nIndex: Integer; dwNewLong: Integer): Integer; stdcall;
function SetWindowLongPtrW(hWnd: HWND; nIndex: Integer; dwNewLong: Integer): Integer; stdcall;

const
  GWLP_WNDPROC = -4;
  GWLP_HINSTANCE = -6;
  GWLP_HWNDPARENT = -8;
  GWLP_USERDATA = -21;
  GWLP_ID = -12;

implementation             
//--------------------------------------------------------------------
procedure ShowModal(frm, self:TForm);
begin
    frm.Top     := self.Top;
    frm.Left    := self.Left;
    frm.Width   := self.Width;
    frm.Height  := self.Height;
    ShowWindow( Application.handle, SW_HIDE );
    self.Visible := false;
    //frmEncode.PopupParent := Self;
    frm.Showmodal;          
    self.Top     := frm.Top;
    self.Left    := frm.Left;
    self.Width   := frm.Width;
    self.Height  := frm.Height;
    self.Visible := true;
    ShowWindow( Application.handle, SW_RESTORE );
end;
//-------------------------------------------------------------
procedure WinCreate(hWnd: HWND);
begin
  ShowWindow(hWnd, SW_HIDE);
  SetWindowLongPtr(hWnd, GWL_EXSTYLE,
    GetWindowLongPtr(hWnd, GWL_EXSTYLE) and not WS_EX_APPWINDOW
    or WS_EX_TOOLWINDOW);
  ShowWindow(hWnd, SW_SHOW);
end;
//-------------------------------------------------------------
procedure WinTaskbarShow(hWnd: HWND);
begin
  SetWindowLongPtr(hWnd, GWL_EXSTYLE, WS_EX_APPWINDOW);
end;
//-------------------------------------------------------------
{$IFNDEF _WIN64}
// In a 32-bit build, these are simply aliases for the non-Ptr versions.
// (WinUser.h uses macros, e.g.: #define GetWindowLongPtrA GetWindowLongA)
function GetWindowLongPtr; external user32 name 'GetWindowLongA';
function GetWindowLongPtrA; external user32 name 'GetWindowLongA';
function GetWindowLongPtrW; external user32 name 'GetWindowLongW';
function SetWindowLongPtr; external user32 name 'SetWindowLongA';
function SetWindowLongPtrA; external user32 name 'SetWindowLongA';
function SetWindowLongPtrW; external user32 name 'SetWindowLongW';
{$ELSE}
// In a 64-bit build, use the real Ptr functions.
function GetWindowLongPtr; external user32 name 'GetWindowLongPtrA';
function GetWindowLongPtrA; external user32 name 'GetWindowLongPtrA';
function GetWindowLongPtrW; external user32 name 'GetWindowLongPtrW';
function SetWindowLongPtr; external user32 name 'SetWindowLongPtrA';
function SetWindowLongPtrA; external user32 name 'SetWindowLongPtrA';
function SetWindowLongPtrW; external user32 name 'SetWindowLongPtrW';
{$ENDIF}
end.
