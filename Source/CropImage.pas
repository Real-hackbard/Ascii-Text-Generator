(**************************************************

TCropImage component v0.9

Component is designed to help users select a region from an image.
The image is displayed scaled to fit, but not stretched.
If image is present, crop tool lines can be moved whith common mouse action.
When ready,  call GetRegion  method  and  get a  BitMap from original  image
corresponding  to the region between the lines. Since selection is performed
on a thumbnail, resizing compoent may cause a slite difference in the region
boundaries. When NoFlicker is true, component looses transparency.
The component is based on TImage but is no TImage descendant.

Suggestions and comments are welcome. Add contribution records below.
{version; status; contributor; e-mail; release date; comments}

* 0.9; original; Zoltan Zörgõ; zorgoz@inf.unideb.hu; 08.27.2004; Beta tests needed

***************************************************)
unit CropImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type
TCropImage = class(TGraphicControl)
  private
    FPicture  : TPicture;
    FDrawing  : Boolean;
    FThumbRect: TRect;
    FThumbBmp : TBitMap;
    FToolColor: TColor;
    FToolWidth: Integer;
    FBorderWidth: Integer;
    FToolRect : TRect; //stores distance from thumbnail edges
    FBorder   : Boolean;
    FScale    : Double;
    FNoFlicker: Boolean;
    FOnMouseMove: TMouseMoveEvent;
    FSquare   : Boolean;  
    FGrid     : Integer;


    MouseIsOver   :Integer;
    MouseIsDown   :Boolean;
    MouseIsDraging:Integer;
    MarkX,MarkY   :Integer;

    function  GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(Value: TPicture);
    procedure CreateThumbnail;
    procedure setBorder(Value: Boolean);
    procedure setNoFlicker(Value: Boolean);
    procedure setBorderWidth(Value: Integer);
    procedure setToolColor(value: TColor);
    procedure setToolWidth(value: Integer);
    procedure DrawTool(color:Tcolor);
  protected
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function  CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function  DestRect: TRect;
    function  DoPaletteChange: Boolean;
    function  GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure FOnResize(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
    procedure ResetToolRect;
    function GetRegion:TBitMap;//This is the reason for this component
    function RealRect:TRect;
  published
    property Align;
    property Anchors;
    property Border: Boolean read FBorder write SetBorder;
    property BorderWidth: Integer read FBorderWidth write setBorderWidth default 3;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Square: Boolean read FSquare write FSquare;    
    property Grid: Integer read FGrid write FGrid;
    property ToolRect: TRect read FToolRect write FToolRect;
    property ThumbRect: TRect read FThumbRect write FThumbRect;
    property NoFlicker: Boolean read FNoFlicker write setNoFlicker default false;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property ShowHint;
    property Scale:Double read Fscale;
    property ToolColor: TColor read FToolColor write SetToolColor default clRed;
    property ToolWidth: Integer read FToolWidth write SetToolWidth default 1;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
  end;

procedure Register;

implementation
{$R *.dcr}
uses forms, consts, Math, Types;

//////////////////////////////////////////////////

//------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Samples', [TCropImage]);
end;
//------------------------------------------------------------
constructor TCropImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  OnResize := FOnResize;
  Height := 100;
  Width := 100;
  FThumbBmp:=TBitmap.Create;
  FBorderWidth:=3;
  FThumbRect:=Rect(0,0,0,0);
  FToolRect:=Rect(20,20,20,20);
  FToolColor:=clRed;
  FToolWidth:=1;
  MouseIsOver:=0;
  MouseIsDown:=false;
  NoFlicker:=false;
  FSquare:=False;
  FGrid:=1;
end;
//------------------------------------------------------------
destructor TCropImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;
//------------------------------------------------------------
function TCropImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
	Result := FPicture.Graphic.Palette;
end;
//------------------------------------------------------------
function TCropImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if (w > cw) or (h > ch) then
  begin
	if (w > 0) and (h > 0) then
	begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  if FBorder then
     begin
     dec(h,BorderWidth*2);
     dec(w,BorderWidth*2);
     end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;
	OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;
//------------------------------------------------------------
procedure TCropImage.DrawTool(color:TColor);
var i:Integer;
begin
with inherited Canvas do
	begin
    Pen.Color:=Color;
    Pen.Width:=FToolWidth;
    Pen.Style:=psSolid;
    brush.Style:=bsClear;
    if FBorder
       then i:=FBorderWidth
       else i:=0;
    MoveTo(i,FThumbRect.Top+FToolRect.Top);LineTo(Width-i,FThumbRect.Top+FToolRect.Top);//Top
    MoveTo(i,FThumbRect.Bottom-FToolRect.Bottom);LineTo(Width-i,FThumbRect.Bottom-FToolRect.Bottom);//Bottom
    MoveTo(FThumbRect.Left+FToolRect.Left,i);LineTo(FThumbRect.Left+FToolRect.Left,Height-i);//Left
    MoveTo(FThumbRect.Right-FToolRect.Right,i);LineTo(FThumbRect.Right-FToolRect.Right,Height-i);//Right
  end;  
end;
//------------------------------------------------------------
procedure TCropImage.Paint;
var
  Save: Boolean;
  tr:Trect;
  i:Integer;
begin
if FNoFlicker then
  if MouseIsDraging=0 then
  with inherited Canvas do
	begin
    Pen.Color:=clBlack;
	  Pen.Style := psClear;
	  Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
	  Rectangle(0, 0, Width, Height);
	end;

  if (csDesigning in ComponentState)and(not FBorder) then
	with inherited Canvas do
	begin
    Pen.Color:=clBlack;
	  Pen.Style := psDash;
	  Brush.Style := bsClear;
	  Rectangle(0, 0, Width, Height);
	end;

  Save := FDrawing;
  FDrawing := True;
  try
   with inherited Canvas do
    begin
    if (FBorder)then
       begin
       Pen.Style:=psSolid;
       tr:=rect(0,0,Width, Height);
       Frame3D(inherited Canvas,tr,clBtnHighlight,clBtnShadow,1);
       tr:=rect(BorderWidth-1,BorderWidth-1,Width-BorderWidth+1, Height-BorderWidth+1);
       Frame3D(inherited Canvas,tr,clBtnShadow,clBtnHighlight,1);
       if not FNoFlicker
          then begin
               pen.Color:=clBtnFace;
               Brush.Style := bsClear;
               for i:=1 to BorderWidth-2
                   do Rectangle(i,i,Width-i, Height-i);
               end;
       end;
    FOnResize(self);
    Draw(FThumbRect.Left,FThumbRect.Top,FThumbBmp);
    DrawTool(FToolColor);
    end
  finally
	FDrawing := Save;
  end;
end;
//------------------------------------------------------------
function TCropImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
	(Tmp.PaletteModified) then
  begin
	if (Tmp.Palette = 0) then
	  Tmp.PaletteModified := False
	else
	begin
	  ParentForm := GetParentForm(Self);
	  if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
	  begin
		if FDrawing then
		  ParentForm.Perform(wm_QueryNewPalette, 0, 0)
		else
		  PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
		Result := True;
		Tmp.PaletteModified := False;
	  end;
	end;
  end;
end;
//------------------------------------------------------------
function TCropImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
	Bitmap := TBitmap.Create;
	try
	  Bitmap.Width := Width;
	  Bitmap.Height := Height;
	  Picture.Graphic := Bitmap;
	finally
	  Bitmap.Free;
	end;
  end;
  if Picture.Graphic is TBitmap then
	Result := TBitmap(Picture.Graphic).Canvas
  else
	raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;
//------------------------------------------------------------
procedure TCropImage.SetPicture(Value: TPicture);
begin
  if value.Graphic is TMetafile
     then raise EInvalidGraphic.Create('This component does not support vector graphic.');
  FPicture.Assign(Value);
  OnResize(self);
  ResetToolRect;
end;
//------------------------------------------------------------
procedure TCropImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
	SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
	if not ((G is TMetaFile) or (G is TIcon)) then
	  G.Transparent := False;
        D := DestRect;
	if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
	   (D.Right >= Width) and (D.Bottom >= Height) then
	  ControlStyle := ControlStyle + [csOpaque]
	else  // picture might not cover entire clientrect
	  ControlStyle := ControlStyle - [csOpaque];
	if DoPaletteChange and FDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing
     then begin
     CreateThumbnail;
     ResetToolRect;
     Invalidate;
     end;
end;
//------------------------------------------------------------
function TCropImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;
//------------------------------------------------------------
function TCropImage.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
result:=true;
if NewWidth<50 then NewWidth:=50;
if NewHeight<50 then NewHeight:=50;
end;
//------------------------------------------------------------
procedure TCropImage.FOnResize(sender:TObject);
var tDestRect:TRect;
    oldScale,ratio:Double;
begin
 TDestRect:=DestRect;
 if ((tDestRect.Bottom-tDestRect.Top)<>(FThumbRect.Bottom-FThumbRect.Top))
    or
    ((tDestRect.Right-tDestRect.Left)<>(FThumbRect.Right-FThumbRect.Left))
    then begin
     FThumbRect:=TDestRect;
     if Assigned(Picture.Graphic) and (not Picture.Graphic.Empty)
       then begin
            OldScale:=Fscale;
            Fscale:= (((tDestRect.Right-tDestRect.Left) / Picture.Graphic.Width)+
                      ((tDestRect.Bottom-tDestRect.Top) / Picture.Graphic.Height))/2;
            if OldScale<>0 then
            with FToolRect do begin
                 ratio :=Fscale/oldScale;
                 Right :=Round(Right*ratio);
                 Bottom:=Round(Bottom*ratio);
                 Left  :=Round(Left*ratio);
                 Top   :=Round(Top*ratio);
                 end
            else ResetToolRect;
            end
       else FScale:=0;
     CreateThumbnail;
    end;
  if (not Assigned(Picture.Graphic)) or Picture.Graphic.Empty
     then begin
          FToolRect.Left:=Round(Width*0.4);
          FToolRect.Right:=Round(Width*0.4);
          FToolRect.Top:=Round(Height*0.4);
          FToolRect.Bottom:=Round(Height*0.4);
          end;
  FThumbRect:=TDestRect;
end;
//------------------------------------------------------------
procedure TCropImage.CreateThumbnail;
var TBMP:TBitMap;
begin
FThumbBmp.Height:=0;
FThumbBmp.Width:=0;
if Assigned(Picture.Graphic) then
if not Picture.Graphic.Empty then
 try
  FThumbBmp.Height:=abs(FThumbRect.Bottom-FThumbRect.Top);
  FThumbBmp.Width:=abs(FThumbRect.Right-FThumbRect.Left);
  tbmp:=TBitmap.Create;
  tbmp.Height:=Picture.Graphic.Height;
  tbmp.Width:=Picture.Graphic.Width;
  tbmp.Canvas.Draw(0,0,Picture.Graphic);
  FThumbBmp.Canvas.StretchDraw(Rect(0,0,FThumbBmp.Width,FThumbBmp.Height),tbmp);
 finally
  tbmp.Free;
 end;
end;
//------------------------------------------------------------
procedure TCropImage.setBorder(Value: Boolean);
begin
if FBorder<>Value
   then begin
   FBorder:=Value;
   FOnResize(self);
   InValidate;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.setBorderWidth(Value: Integer);
begin
if (Value<3) or (Value>10)
   then raise EInvalidArgument.Create('Border width should have value between three and ten.');
if FBorderWidth<>Value
   then begin
   FBorderWidth:=Value;
   FOnResize(self);
   InValidate;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.setToolWidth(Value: Integer);
begin
if (Value<1)or(Value>5)or(not odd(Value))
   then raise EInvalidArgument.Create('Crop tool width must have odd value between one and five.');
if Value<>FToolWidth
   then begin
   FToolWidth:=value;
   Invalidate;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.setToolColor(Value: TColor);
begin
if Value<>FToolColor
   then begin
   FToolColor:=value;
   Invalidate;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.setNoFlicker(Value: Boolean);
begin
if Value<>FNoFlicker
   then begin
   FNoFlicker:=value;
   Invalidate;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.ResetToolRect;
begin
FToolRect:=DestRect;
if (not Assigned(Picture.Graphic)) or Picture.Graphic.Empty
     then with FToolRect do begin
               Left:=Round(Width*0.4);
               Right:=Round(Width*0.4);
               Top:=Round(Height*0.4);
               Bottom:=Round(Height*0.4);
               end
//     else FToolRect:=Rect(2*FToolWidth,2*FToolWidth,2*FToolWidth,2*FToolWidth);
     else FToolRect:=Rect(0,0,0,0);
end;
//------------------------------------------------------------
procedure TCropImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if (MouseIsOver>0)and(Button=mbLeft)and(FScale>0)
   then begin
   MouseIsDown:=true;
   MouseIsDraging:=MouseIsOver;
   MarkX:=x;
   MarkY:=y;
   end;
end;
//------------------------------------------------------------
procedure TCropImage.MouseMove(Shift: TShiftState; X, Y: Integer);
 function isOver(t,v:Integer):Boolean;
  begin
   result:=abs(t-v)<=3
  end;
var io:Byte;
  W, H: integer;    
  lastW, lastH: integer;
begin
if not MouseIsDown
   then begin
   MouseIsOver:=0;
   if IsOver(x,FThumbRect.Left+FToolRect.Left) then MouseIsOver:=MouseIsOver or 1;
   if IsOver(x,FThumbRect.Right-FToolRect.Right) then MouseIsOver:=MouseIsOver or 2;
   if IsOver(y,FThumbRect.Top+FToolRect.Top) then MouseIsOver:=MouseIsOver or 4;
   if IsOver(y,FThumbRect.Bottom-FToolRect.Bottom) then MouseIsOver:=MouseIsOver or 8;
   case MouseIsOver of
     1,2:Cursor:=crSizeWE;
     4,8:Cursor:=crSizeNS;
     5,10:Cursor:=crSizeNWSE;
     6,9:Cursor:=crSizeNESW;
     else Cursor:=crDefault;
   end;
   end else begin 
     lastW := FThumbRect.Right-FThumbRect.Left - (FToolRect.Right+FToolRect.Left);
     lastH := FThumbRect.Bottom-FThumbRect.Top - (FToolRect.Top+FToolRect.Bottom);
     if FNoFlicker then DrawTool(clBtnFace);
     if FGrid > 1 then begin
       if (1 and MouseIsDraging)>0 then FToolRect.Left:=Floor((x-FThumbRect.Left)/FGrid)*FGrid;
       if (2 and MouseIsDraging)>0 then FToolRect.Right:=Floor((FThumbRect.Right-x)/FGrid)*FGrid;
       if (4 and MouseIsDraging)>0 then FToolRect.Top:=Floor((y-FThumbRect.Top)/FGrid)*FGrid;
       if (8 and MouseIsDraging)>0 then FToolRect.Bottom:=Floor((FThumbRect.Bottom-y)/FGrid)*FGrid;
     end else begin
       if (1 and MouseIsDraging)>0 then FToolRect.Left:=x-FThumbRect.Left;
       if (2 and MouseIsDraging)>0 then FToolRect.Right:=FThumbRect.Right-x;
       if (4 and MouseIsDraging)>0 then FToolRect.Top:=y-FThumbRect.Top;
       if (8 and MouseIsDraging)>0 then FToolRect.Bottom:=FThumbRect.Bottom-y;
     end;
     W := FThumbRect.Right-FThumbRect.Left - (FToolRect.Right+FToolRect.Left);
     H := FThumbRect.Bottom-FThumbRect.Top - (FToolRect.Top+FToolRect.Bottom);
     if FGrid > 1 then begin
       W := Floor(W/Fgrid)*FGrid;
       H := Floor(H/Fgrid)*FGrid;
     end;
     if Square then begin
       if (Cursor=crSizeWE) or (Cursor=crSizeNS) then begin
         if (1 and MouseIsDraging)>0 then FToolRect.Bottom:=FThumbRect.Bottom-FThumbRect.Top-(FToolRect.Top+W);
         if (2 and MouseIsDraging)>0 then FToolRect.Bottom:=FThumbRect.Bottom-FThumbRect.Top-(FToolRect.Top+W);
         if (4 and MouseIsDraging)>0 then FToolRect.Right:=FThumbRect.Right-FThumbRect.Left-(FToolRect.Left+H);
         if (8 and MouseIsDraging)>0 then FToolRect.Right:=FThumbRect.Right-FThumbRect.Left-(FToolRect.Left+H);
       end else begin
         case MouseIsOver of
          5: begin // Top Left
            FToolRect.Bottom:= FThumbRect.Bottom-FThumbRect.Top-FToolRect.Top-Min(lastW, lastH);
            FToolRect.Right:= FThumbRect.Right-FThumbRect.Left-FToolRect.Left-Min(lastW, lastH);
          end;
          10: begin // Bottom Right
            FToolRect.Top:= FThumbRect.Bottom-FThumbRect.Top-FToolRect.Bottom-Min(lastW, lastH);
            FToolRect.Left:= FThumbRect.Right-FThumbRect.Left-FToolRect.Right-Min(lastW, lastH);
          end;
          6:begin  // Top Right
            FToolRect.Bottom:= FThumbRect.Bottom-FThumbRect.Top-FToolRect.Top-Min(lastW, lastH);
            FToolRect.Left:= FThumbRect.Right-FThumbRect.Left-FToolRect.Right-Min(lastW, lastH);
          end;
          9: begin // Bottom Left
            FToolRect.Top:= FThumbRect.Bottom-FThumbRect.Top-FToolRect.BOttom-Min(lastW, lastH);
            FToolRect.Right:= FThumbRect.Right-FThumbRect.Left-FToolRect.Left-Min(lastW, lastH);
          end;
         end;
       end;
     end else begin
       if FGrid > 1 then begin
       if (1 and MouseIsDraging)>0 then FToolRect.Left:=FThumbRect.Right-FThumbRect.Left-(FToolRect.Right+W);
       if (2 and MouseIsDraging)>0 then FToolRect.Right:=FThumbRect.Right-FThumbRect.Left-(FToolRect.Left+W);
       if (4 and MouseIsDraging)>0 then FToolRect.Top:=FThumbRect.Bottom-FThumbRect.Top-(FToolRect.Bottom+H);
       if (8 and MouseIsDraging)>0 then FToolRect.Bottom:=FThumbRect.Bottom-FThumbRect.Top-(FToolRect.Top+H);
       end;
     end;
     if FNoFlicker
        then Paint
        else Invalidate;
   end;
   if Assigned(FOnMouseMove) and Assigned(Picture.Graphic) and (not Picture.Graphic.Empty) then FOnMouseMove(Self, Shift, X, Y);
end;
//------------------------------------------------------------
procedure TCropImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tr:TRect;
    h,w:Integer;
begin
MouseIsDown:=false;
MouseIsOver:=0;
MouseIsDraging:=0;
w:=abs(FThumbRect.Right-FThumbRect.Left);
h:=abs(FThumbRect.Bottom-FThumbRect.Top);
if FToolRect.Left+FToolRect.Right>w
   then begin tr.Left:=w-FToolRect.Right;tr.Right:=w-FToolRect.Left end
   else begin tr.Left:=FToolRect.Left;tr.Right:=FToolRect.Right end;
if FToolRect.Top+FToolRect.Bottom>h
   then begin tr.Top:=h-FToolRect.Bottom;tr.Bottom:=h-FToolRect.Top end
   else begin tr.Top:=FToolRect.Top;tr.Bottom:=FToolRect.Bottom end;
if tr.Left<0 then tr.Left:=0;
if tr.Right<0 then tr.Right:=0;
if tr.Top<0 then tr.Top:=0;
if tr.Bottom<0 then tr.Bottom:=0;
FToolRect:=tr;
invalidate;   
end;
//------------------------------------------------------------ 
function TCropImage.RealRect:TRect;
var
  W, H: integer;
begin
  Result.Left:=Round(FToolRect.Left/FScale);
  Result.Right:=Round((FThumbRect.Right-FThumbRect.Left-FToolRect.Right)/FScale);
  Result.Top:=Round(FToolRect.Top/FScale);
  Result.Bottom:=Round((FThumbRect.Bottom-FThumbRect.Top-FToolRect.Bottom)/FScale);
  W := Result.Right - Result.Left;
  H := Result.Bottom - Result.Top;
  if FGrid > 1 then begin
    W := Floor(W/Fgrid)*FGrid;
    H := Floor(H/Fgrid)*FGrid;
  end;
  if Square then begin     
    Result.Right := Result.Left + Min(W, H);
    Result.Bottom := Result.Top + Min(W, H);
  end else begin
    if FGrid > 1 then begin
      Result.Right := Result.Left + W;
      Result.Bottom := Result.Top + H;
    end;
  end;
end;  
//------------------------------------------------------------
function TCropImage.GetRegion:TBitmap;
var bmp,tbmp:TBitmap;
    drect,srect:Trect;
begin
if FScale=0 then
   begin
   result:=NIL;
   exit;
   end;
try
 bmp:=TBitmap.Create;
 srect := RealRect;
 bmp.Height:=srect.Bottom-srect.Top;
 bmp.Width:=srect.Right-srect.Left;
 drect.Left:=0;
 drect.Top:=0;
 drect.Bottom:=bmp.Height;
 drect.Right:=bmp.Width;
 try
  tbmp:=TBitmap.Create;
  tbmp.Height:=picture.Graphic.Height;
  tbmp.Width:=picture.Graphic.Width;
  tbmp.Canvas.Draw(0,0,picture.Graphic);
  bmp.Canvas.CopyRect(drect,tbmp.Canvas,srect);
  result:=bmp;
 finally
  tbmp.Free;
 end;

except
 raise
end;
end;

end.