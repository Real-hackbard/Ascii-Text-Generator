unit Graphiques;

interface
uses Windows, Graphics, Classes, SysUtils, ShellApi;


function GetIcon(const FileName: TFileName; const Large: Boolean): TIcon;

implementation
//-------------------------------------------------------------------------
function GetIcon(const FileName: TFileName; const Large: Boolean): TIcon;
var
  sfi: TSHFileInfo;
  i: Integer;
begin
  Result := nil;
  try
   Result := TIcon.Create;
   if Large then
     i := SHGFI_LARGEICON
   else
     i := SHGFI_SMALLICON;
   SHGetFileInfo(PChar(FileName), FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES or i);
   if sfi.hIcon = 0 then
     FreeAndNil(Result)
   else
     Result.Handle := sfi.hIcon;
  except
   FreeAndNil(Result);
  end;
end;
//-------------------------------------------------------------------------
end.
