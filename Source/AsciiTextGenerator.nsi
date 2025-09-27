!include "..\Common.nsh"

;-----------------------------------------------------------------------
; MAIN SCRIPT

SetCompressor /SOLID lzma

!include "MUI.nsh"

Name "Arnapou Ascii Text Generator"
OutFile "AsciiTextGenerator_v1.0.exe"

InstallDir "$PROGRAMFILES\Arnapou\Ascii Text Generator"
InstallDirRegKey HKCU "Software\Arnapou\Ascii Text Generator" ""

!define MUI_ABORTWARNING

;!insertmacro MUI_PAGE_LICENSE "licence.txt"
;!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English" ;first language is the default language
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Spanish"
!insertmacro MUI_LANGUAGE "Italian"
!insertmacro MUI_LANGUAGE "Portuguese"

InstType "Defaut"

Section "Program" Sec_Program
	SectionIn 1 RO
	SetOutPath $INSTDIR 
	File "Ascii_Text_Generator.exe"
	File /r "fonts"
	SetShellVarContext current
	CreateDirectory "$SMPROGRAMS\Arnapou"
	CreateDirectory "$SMPROGRAMS\Arnapou\Ascii Text Generator"
	CreateShortCut "$SMPROGRAMS\Arnapou\Ascii Text Generator\Ascii Text Generator.lnk" "$INSTDIR\Ascii_Text_Generator.exe"
	CreateShortCut "$SMPROGRAMS\Arnapou\Ascii Text Generator\Uninstall.lnk" "$INSTDIR\uninstall.exe"
	WriteUninstaller "$INSTDIR\uninstall.exe"
	WriteRegStr HKCU "Software\Arnapou\Ascii Text Generator" "" '"$INSTDIR"'
	!insertmacro AddRemovePanel_ADD "Arnapou / Ascii Text Generator" "1.0" "1650" "$INSTDIR\uninstall.exe"
SectionEnd 

UninstallText "Uninstall Arnapou Ascii Text Generator ?"

Section "Uninstall" Sec_Uninstall
	SetOutPath $TEMP
	SetShellVarContext current
	RMDir /r /REBOOTOK "$INSTDIR"
	RMDir /r /REBOOTOK "$SMPROGRAMS\Arnapou\Ascii Text Generator"
	!insertmacro AddRemovePanel_REMOVE "Arnapou / Ascii Text Generator"
SectionEnd 
