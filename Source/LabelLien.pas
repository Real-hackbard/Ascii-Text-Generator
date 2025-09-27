unit LabelLien;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellAPI;

type
  TTypeLien = (URL,MAIL);
  // - - - - - - - - - - - - - - - - - - - - - - - -
  TLabelLien = class(TLabel)
  private
    { Déclarations privées }
    FAdresse:string;
    FTypeLien:TTypeLien;
  protected
    { Déclarations protégées }
    procedure Click; override;
  public
    { Déclarations publiques }
    constructor Create(AOwner:TComponent); override;
  published
    { Déclarations publiées }
    property Adresse:string read FAdresse write FAdresse nodefault;
    property TypeLien:TTypeLien read FTypeLien write FTypeLien default URL;
  end;
  // - - - - - - - - - - - - - - - - - - - - - - - -

procedure Register;

implementation
//------------------------------------------------------
constructor TLabelLien.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Font.Color:=clBlue;
  Font.Style:=[fsUnderline];
  Cursor:=crHandPoint;
end;
//------------------------------------------------------
procedure TLabelLien.Click;
var St:string;
begin
  inherited;
  case TypeLien of
    URL: St:=FAdresse;
    MAIL: St:='mailto:'+FAdresse;
  end;
  ShellExecute(GetDesktopWindow,'open',PChar(St),nil,nil,SW_SHOWNORMAL);
end;
//------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Standard', [TLabelLien]);
end;
//------------------------------------------------------

end.

