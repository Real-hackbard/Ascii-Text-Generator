{**************************************************************}
{ Name: Transparent Components for Delphi 32 }
{ Author: Medialight Software Solutions }
{ E-Mail: Medialight@yahoo.com }
{ Home Page: http://www.geocities.com/medialight }
{ Created: December, 02, 2000 }
{ Legal: Copyright (c) 2000, Medialight Software Solutions }
{**************************************************************}
{ PUBLISHED PROPERTIES: }
{ Transparent - Sets a memo or an edit box transparent }
{**************************************************************}
{ Transparent Components currently includes 2 components: Memo }
{ and Edit box. All these components have "Transparent" }
{ property, which gives you window transparency. }
{**************************************************************}
{ IMPORTANT NOTE: }
{ This software is provided 'as-is', without any express or }
{ implied warranty. In no event will the author be held }
{ liable for any damages arising from the use of this }
{ software. }
{ Permission is granted to anyone to use this software for }
{ any purpose, including commercial applications, and to }
{ alter it and redistribute it freely, subject to the }
{ following restrictions: }
{ 1. The origin of this software must not be misrepresented, }
{ you must not claim that you wrote the original software. }
{ If you use this software in a product, an acknowledgment }
{ in the product documentation would be appreciated but is }
{ not required. }
{ 2. Altered source versions must be plainly marked as such, }
{ and must not be misrepresented as being the original }
{ software. }
{ 3. This notice may not be removed or altered from any }
{ source distribution. }
{**************************************************************}

unit TranComp;

interface

uses
Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
StdCtrls, ComCtrls;

type
TCtrl = class(TWinControl);

TTransEdit = class(TEdit)
private
FAlignText: TAlignment;
FTransparent: Boolean;
FPainting: Boolean;
procedure SetAlignText(Value: TAlignment);
procedure SetTransparent(Value: Boolean);
procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
procedure WMSize(var Message: TWMSize); message WM_SIZE;
procedure WMMove(var Message: TWMMove); message WM_MOVE;
protected
procedure RepaintWindow;
procedure CreateParams(var Params: TCreateParams); override;
procedure Change; override;
procedure SetParent(AParent: TWinControl); override;
public
constructor Create(AOwner: TComponent); override;
destructor Destroy; override;
published
property AlignText: TAlignment read FAlignText write SetAlignText default taLeftJustify;
property Transparent: Boolean read FTransparent write SetTransparent default false;
end;

// Transparent Memo
TTransMemo = class(TMemo)
private
FAlignText: TAlignment;
FTransparent: Boolean;
FPainting: Boolean;
procedure SetAlignText(Value: TAlignment);
procedure SetTransparent(Value: Boolean);
procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
procedure WMSize(var Message: TWMSize); message WM_SIZE;
procedure WMMove(var Message: TWMMove); message WM_MOVE;
protected
procedure RepaintWindow;
procedure CreateParams(var Params: TCreateParams); override;
procedure Change; override;
procedure SetParent(AParent: TWinControl); override;
public
constructor Create(AOwner: TComponent); override;
destructor Destroy; override;
published
property AlignText: TAlignment read FAlignText write SetAlignText default taLeftJustify;
property Transparent: Boolean read FTransparent write SetTransparent default false;
end;

procedure Register;

implementation

const
BorderRec: array[TBorderStyle] of Integer = (1, -1);

procedure Register;
begin
RegisterComponents('Transparent Components', [TTransEdit, TTransMemo]);
end;

function GetScreenClient(Control: TControl): TPoint;
var
p: TPoint;
begin
p := Control.ClientOrigin;
ScreenToClient(Control.Parent.Handle, p);
Result := p;
end;

constructor TTransEdit.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FAlignText := taLeftJustify;
FTransparent := false;
FPainting := false;
end;

destructor TTransEdit.Destroy;
begin
inherited Destroy;
end;

procedure TTransEdit.SetAlignText(Value: TAlignment);
begin
if FAlignText <> Value then
begin
FAlignText := Value;
RecreateWnd;
Invalidate;
end;
end;

procedure TTransEdit.SetTransparent(Value: Boolean);
begin
if FTransparent <> Value then
begin
FTransparent := Value;
Invalidate;
end;
end;

procedure TTransEdit.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
DC: hDC;
i: integer;
p: TPoint;
begin
if FTransparent then
begin
if Assigned(Parent) then
begin
DC := Message.DC;
i := SaveDC(DC);
p := GetScreenClient(self);
p.x := -p.x;
p.y := -p.y;
MoveWindowOrg(DC, p.x, p.y);
SendMessage(Parent.Handle, $0014, DC, 0);
TCtrl(Parent).PaintControls(DC, nil);
RestoreDC(DC, i);
end;
end else inherited;
end;

procedure TTransEdit.WMPaint(var Message: TWMPaint);
begin
inherited;
if FTransparent then
if not FPainting then
RepaintWindow;
end;

procedure TTransEdit.WMNCPaint(var Message: TMessage);
begin
inherited;
end;

procedure TTransEdit.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
inherited;
if FTransparent then
SetBkMode(Message.ChildDC, 1);
end;

procedure TTransEdit.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
inherited;
if FTransparent then
SetBkMode(Message.ChildDC, 1);
end;

procedure TTransEdit.CMParentColorChanged(var Message: TMessage);
begin
inherited;
if FTransparent then
Invalidate;
end;

procedure TTransEdit.WMSize(var Message: TWMSize);
begin
inherited;
Invalidate;
end;

procedure TTransEdit.WMMove(var Message: TWMMove);
begin
inherited;
Invalidate;
end;

procedure TTransEdit.RepaintWindow;
var
DC: hDC;
TmpBitmap, Bitmap: hBitmap;
begin
if FTransparent then
begin
FPainting := true;
HideCaret(Handle);
DC := CreateCompatibleDC(GetDC(Handle));
TmpBitmap := CreateCompatibleBitmap(GetDC(Handle), Succ(ClientWidth), Succ(ClientHeight));
Bitmap := SelectObject(DC, TmpBitmap);
PaintTo(DC, 0, 0);
BitBlt(GetDC(Handle), BorderRec[BorderStyle], BorderRec[BorderStyle], ClientWidth, ClientHeight, DC, 1, 1, SRCCOPY);
SelectObject(DC, Bitmap);
DeleteDC(DC);
ReleaseDC(Handle, GetDC(Handle));
DeleteObject(TmpBitmap);
ShowCaret(Handle);
FPainting := false;
end;
end;

procedure TTransEdit.CreateParams(var Params: TCreateParams);
const
Alignments: array [TAlignment] of DWord = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
inherited CreateParams(Params);
Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignText];
end;

procedure TTransEdit.Change;
begin
RepaintWindow;
inherited Change;
end;

procedure TTransEdit.SetParent(AParent: TWinControl);
begin
inherited SetParent(AParent);
end;

// Transparent Memo
constructor TTransMemo.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FAlignText := taLeftJustify;
FTransparent := false;
FPainting := false;
end;

destructor TTransMemo.Destroy;
begin
inherited Destroy;
end;

procedure TTransMemo.SetAlignText(Value: TAlignment);
begin
if FAlignText <> Value then
begin
FAlignText := Value;
RecreateWnd;
Invalidate;
end;
end;

procedure TTransMemo.SetTransparent(Value: Boolean);
begin
if FTransparent <> Value then
begin
FTransparent := Value;
Invalidate;
end;
end;

procedure TTransMemo.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
DC: hDC;
i: integer;
p: TPoint;
begin
if FTransparent then
begin
if Assigned(Parent) then
begin
DC := Message.DC;
i := SaveDC(DC);
p := GetScreenClient(self);
p.x := -p.x;
p.y := -p.y;
MoveWindowOrg(DC, p.x, p.y);
SendMessage(Parent.Handle, $0014, DC, 0);
TCtrl(Parent).PaintControls(DC, nil);
RestoreDC(DC, i);
end;
end else inherited;
end;

procedure TTransMemo.WMPaint(var Message: TWMPaint);
begin
inherited;
if FTransparent then
if not FPainting then
RepaintWindow;
end;

procedure TTransMemo.WMNCPaint(var Message: TMessage);
begin
inherited;
end;

procedure TTransMemo.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
inherited;
if FTransparent then
SetBkMode(Message.ChildDC, 1);
end;

procedure TTransMemo.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
inherited;
if FTransparent then
SetBkMode(Message.ChildDC, 1);
end;

procedure TTransMemo.CMParentColorChanged(var Message: TMessage);
begin
inherited;
if FTransparent then
Invalidate;
end;

procedure TTransMemo.WMSize(var Message: TWMSize);
begin
inherited;
Invalidate;
end;

procedure TTransMemo.WMMove(var Message: TWMMove);
begin
inherited;
Invalidate;
end;

procedure TTransMemo.RepaintWindow;
var
DC: hDC;
TmpBitmap, Bitmap: hBitmap;
begin
if FTransparent then
begin
FPainting := true;
HideCaret(Handle);
DC := CreateCompatibleDC(GetDC(Handle));
TmpBitmap := CreateCompatibleBitmap(GetDC(Handle), Succ(ClientWidth), Succ(ClientHeight));
Bitmap := SelectObject(DC, TmpBitmap);
PaintTo(DC, 0, 0);
BitBlt(GetDC(Handle), BorderRec[BorderStyle], BorderRec[BorderStyle], ClientWidth, ClientHeight, DC, 1, 1, SRCCOPY);
SelectObject(DC, Bitmap);
DeleteDC(DC);
ReleaseDC(Handle, GetDC(Handle));
DeleteObject(TmpBitmap);
ShowCaret(Handle);
FPainting := false;
end;
end;

procedure TTransMemo.CreateParams(var Params: TCreateParams);
const
Alignments: array [TAlignment] of DWord = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
inherited CreateParams(Params);
Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignText];
end;

procedure TTransMemo.Change;
begin
RepaintWindow;
inherited Change;
end;

procedure TTransMemo.SetParent(AParent: TWinControl);
begin
inherited SetParent(AParent);
end;

end.