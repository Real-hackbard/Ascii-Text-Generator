// $Id: VBScriptRegex.pas,v 1.2 2003/11/02 20:58:54 Renato Exp $

{ http://www.renatomancuso.com/software/delphi_vbscript/delphi_vbscript.htm }

{**************************************************************}
{                                                              }
{            Borland Delphi wrappers for the                   }
{            MS VBScript Engine 5.5 Regex classes              }
{                                                              }
{  Author: Renato Mancuso <mancuso@renatomancuso.com>          }
{                                                              }
{==============================================================}
{                                                              }
{   DISCLAIMER                                                 }
{   ----------                                                 }
{   This software is distributed in the hope that it will be   }
{   useful, but WITHOUT ANY WARRANTY; without even the implied }
{   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    }
{   PURPOSE.                                                   }
{                                                              }
{**************************************************************}

unit VBScriptRegex;

//
// Enable the following compiler directive if you want to be able
// to observe object creation/destruction in the Debug EventLog
//

{.$DEFINE DBG_TRACK_OBJECT_CREATION_DESTRUCTION}

interface

uses
  SysUtils,
  ComObj;

type
  //
  // library specific exception class
  //
  ERegexException = class(Exception)
  private
    FErrorCode : HRESULT;

  public
    constructor Create(Exc: EOleSysError);

    property ErrorCode: HRESULT
        read FErrorCode;
  end;

type
  //
  // forward declarations
  //
  IRegex            = interface;
  IMatch            = interface;
  IMatchCollection  = interface;
  ISubMatches       = interface;

  //
  // Regex class
  //
  TRegexOptions = set of (reIgnoreCase, reMultiLine);

  Regex = class
  public
    class function Create    (const thePattern      : WideString;
                                    theOptions      : TRegexOptions = []): IRegex;

    class function Match     (const thePattern      : WideString;
                              const theSourceString : WideString): Boolean; overload;

    class function Match     (const thePattern      : WideString;
                                    theOptions      : TRegexOptions;
                              const theSourceString : WideString): Boolean; overload;

    class function Find      (const thePattern      : WideString;
                              const theSourceString : WideString): IMatchCollection; overload;

    class function Find      (const thePattern      : WideString;
                                    theOptions      : TRegexOptions;
                              const theSourceString : WideString): IMatchCollection; overload;

    class function Replace   (const thePattern      : WideString;
                              const theSourceString : WideString;
                              const theReplaceVar   : WideString): WideString; overload;

    class function Replace   (const thePattern      : WideString;
                                    theOptions      : TRegexOptions;
                              const theSourceString : WideString;
                              const theReplaceVar   : WideString): WideString; overload;
    class function FindAll   (const thePattern      : WideString;
                              const theSourceString : WideString): IMatchCollection; overload;

    class function FindAll   (const thePattern      : WideString;
                                    theOptions      : TRegexOptions;
                              const theSourceString : WideString): IMatchCollection; overload;

    class function ReplaceAll(const thePattern      : WideString;
                              const theSourceString : WideString;
                              const theReplaceVar   : WideString): WideString; overload;

    class function ReplaceAll(const thePattern      : WideString;
                                    theOptions      : TRegexOptions;
                              const theSourceString : WideString;
                              const theReplaceVar   : WideString): WideString; overload;
  end;

  //
  //  IRegex
  //
  IRegex = interface
  ['{7F3A84DA-31A0-45FC-BB6A-390300FBC439}']
    function GetPattern     : WideString;
    function GetIgnoreCase  : Boolean;
    function GetMultiLine   : Boolean;

     property Pattern: WideString
        read GetPattern;

    property IgnoreCase: Boolean
        read GetIgnoreCase;

    property MultiLine: Boolean
        read GetMultiLine;

    function Match      (const theSourceString: WideString): Boolean;

    function Find       (const theSourceString: WideString): IMatchCollection;
    function FindAll    (const theSourceString: WideString): IMatchCollection;

    function Replace    (const theSourceString: WideString;
                         const theReplaceVar  : WideString): WideString;
                         
    function ReplaceAll (const theSourceString: WideString;
                         const theReplaceVar  : WideString): WideString;
  end;

  //
  // IMatchCollection
  //
  IMatchCollection = interface
  ['{A765779A-0EDA-42B5-938D-576BDCD7BA22}']
    function GetCount: Integer;
    function GetItem(index: Integer): IMatch;

    property Count: Integer
        read GetCount;

    property Item[index: Integer]: IMatch
        read GetItem; default;
  end;

  //
  // IMatch
  //
  IMatch = interface
  ['{4BF1FA3E-A27D-4E93-A659-38CB256059B3}']
    function GetValue     : WideString;
    function GetFirstIndex: Integer;
    function GetLength    : Integer;
    function GetSubMatches: ISubMatches;

    property Value: WideString
        read GetValue;

    property FirstIndex: Integer
        read GetFirstIndex;

    property Length: Integer
        read GetLength;

    property SubMatches: ISubMatches
        read GetSubMatches;
  end;

  //
  //  ISubMatches
  //
  ISubMatches = interface
  ['{80A9ADFB-0020-4F50-88D6-CF2725BAE525}']
    function GetCount: Integer;
    function GetItem(index: Integer): OleVariant;

    property Count: Integer
        read GetCount;

    property Item[index: Integer]: OleVariant
        read GetItem; default;
  end;

//==========================================================================
//==========================================================================
//==========================================================================

implementation

//==========================================================================
//==========================================================================
//==========================================================================

uses
  Windows;

//==========================================================================

{ VBScript 5.5 types }

const
  // CLSID of VBScript Regex CoClass
  CLSID_RegExp: TGUID = '{3F4DACA4-160D-11D2-A8E9-00104B365C9F}';

type
  //
  // forward declaration of dispatch interfaces defined in VBScript RegExp TypeLibrary
  //
  VBScript_IRegExp2           = interface;
  VBScript_IMatch2            = interface;
  VBScript_IMatchCollection2  = interface;
  VBScript_ISubMatches        = interface;

  // 
  // Interface: VBScript_IRegExp2
  // Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
  //
  VBScript_IRegExp2 = interface(IDispatch)
    ['{3F4DACB0-160D-11D2-A8E9-00104B365C9F}']
    function Get_Pattern: WideString; safecall;
    procedure Set_Pattern(const pPattern: WideString); safecall;
    function Get_IgnoreCase: WordBool; safecall;
    procedure Set_IgnoreCase(pIgnoreCase: WordBool); safecall;
    function Get_Global: WordBool; safecall;
    procedure Set_Global(pGlobal: WordBool); safecall;
    function Get_Multiline: WordBool; safecall;
    procedure Set_Multiline(pMultiline: WordBool); safecall;
    function Execute(const sourceString: WideString): IDispatch; safecall;
    function Test(const sourceString: WideString): WordBool; safecall;
    function Replace(const sourceString: WideString; replaceVar: OleVariant): WideString; safecall;
    property Pattern: WideString read Get_Pattern write Set_Pattern;
    property IgnoreCase: WordBool read Get_IgnoreCase write Set_IgnoreCase;
    property Global: WordBool read Get_Global write Set_Global;
    property Multiline: WordBool read Get_Multiline write Set_Multiline;
  end;

  //
  // Interface: VBScript_IMatch2
  // Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
  //
  VBScript_IMatch2 = interface(IDispatch)
    ['{3F4DACB1-160D-11D2-A8E9-00104B365C9F}']
    function Get_Value: WideString; safecall;
    function Get_FirstIndex: Integer; safecall;
    function Get_Length: Integer; safecall;
    function Get_SubMatches: IDispatch; safecall;
    property Value: WideString read Get_Value;
    property FirstIndex: Integer read Get_FirstIndex;
    property Length: Integer read Get_Length;
    property SubMatches: IDispatch read Get_SubMatches;
  end;

  //
  // Interface: VBScript_IMatchCollection2
  // Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
  //
  VBScript_IMatchCollection2 = interface(IDispatch)
    ['{3F4DACB2-160D-11D2-A8E9-00104B365C9F}']
    function Get_Item(index: Integer): IDispatch; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[index: Integer]: IDispatch read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

  //
  // Interface: VBScript_ISubMatches
  // Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
  //
  VBScript_ISubMatches = interface(IDispatch)
    ['{3F4DACB3-160D-11D2-A8E9-00104B365C9F}']
    function Get_Item(index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

//==========================================================================

{ VBScript runtime error codes and messages [from MSDN] }

const
  MAX_VBSCRIPT_ERR = 43;

type
  TVBScriptErrRec = record
    Code: Integer;
    Msg : string;
  end;

  TVBScriptErrorList = array[1..MAX_VBSCRIPT_ERR] of TVBScriptErrRec;

const
  VBScriptRuntimeErrors : TVBScriptErrorList = (
    (Code:  429; Msg: 'ActiveX component can''t create object'),
    (Code:  507; Msg: 'An exception occurred'),
    (Code:  449; Msg: 'Argument not optional'),
    (Code:   17; Msg: 'Can''t perform requested operation'),
    (Code:  430; Msg: 'Class doesn''t support Automation'),
    (Code:  506; Msg: 'Class not defined'),
    (Code:   11; Msg: 'Division by zero'),
    (Code:   48; Msg: 'Error in loading DLL'),
    (Code: 5020; Msg: 'Expected '')'' in regular expression'),
    (Code: 5019; Msg: 'Expected '']'' in regular expression'),
    (Code:  432; Msg: 'File name or class name not found during Automation operation'),
    (Code:   92; Msg: 'For loop not initialized'),
    (Code: 5008; Msg: 'Illegal assignment'),
    (Code:   51; Msg: 'Internal error'),
    (Code:  505; Msg: 'Invalid or unqualified reference'),
    (Code:  481; Msg: 'Invalid picture'),
    (Code:    5; Msg: 'Invalid procedure call or argument'),
    (Code: 5021; Msg: 'Invalid range in character set'),
    (Code:   94; Msg: 'Invalid use of Null'),
    (Code:  448; Msg: 'Named argument not found'),
    (Code:  447; Msg: 'Object doesn''t support current locale setting'),
    (Code:  445; Msg: 'Object doesn''t support this action'),
    (Code:  438; Msg: 'Object doesn''t support this property or method'),
    (Code:  451; Msg: 'Object not a collection'),
    (Code:  504; Msg: 'Object not safe for creating'),
    (Code:  503; Msg: 'Object not safe for initializing'),
    (Code:  502; Msg: 'Object not safe for scripting'),
    (Code:  424; Msg: 'Object required'),
    (Code:   91; Msg: 'Object variable not set'),
    (Code:    7; Msg: 'Out of Memory'),
    (Code:   28; Msg: 'Out of stack space'),
    (Code:   14; Msg: 'Out of string space'),
    (Code:    6; Msg: 'Overflow'),
    (Code:   35; Msg: 'Sub or function not defined'),
    (Code:    9; Msg: 'Subscript out of range'),
    (Code: 5017; Msg: 'Syntax error in regular expression'),
    (Code:  462; Msg: 'The remote server machine does not exist or is unavailable'),
    (Code:   10; Msg: 'This array is fixed or temporarily locked'),
    (Code:   13; Msg: 'Type mismatch'),
    (Code: 5018; Msg: 'Unexpected quantifier'),
    (Code:  500; Msg: 'Variable is undefined'),
    (Code:  458; Msg: 'Variable uses an Automation type not supported in VBScript'),
    (Code:  450; Msg: 'Wrong number of arguments or invalid property assignment')
  );

//==========================================================================

function VBScript_GetErrorMessage(theErrorCode: Integer): string;

var
  i : Integer;

begin
  Result := '';

  for i := 1 to MAX_VBSCRIPT_ERR do
    if VBScriptRuntimeErrors[i].Code = theErrorCode then
    begin
      Result := VBScriptRuntimeErrors[i].Msg;
      break;
    end;
end;

//==========================================================================

{ ERegexException }

constructor ERegexException.Create(Exc: EOleSysError);

var
  theErrorCode    : Integer;
  theErrorMessage : string;

begin
  theErrorCode    := Exc.ErrorCode and $FFFF;
  theErrorMessage := VBScript_GetErrorMessage(theErrorCode);

  if theErrorMessage = '' then
    theErrorMessage := Exc.Message;

  inherited Create(theErrorMessage);
  FErrorCode      := Exc.ErrorCode;
end;

//==========================================================================
//==========================================================================
//==========================================================================

{ Debug Helpers }

procedure DBG_Output(const theMessage: string);
begin
  Windows.OutputDebugString(PChar(theMessage));
end;

procedure DBG_NotifyCreation(const theClass: string; theInstancePtr: Pointer);
begin
{$IFDEF DBG_TRACK_OBJECT_CREATION_DESTRUCTION}
  DBG_Output(Format('Created %s instance at %p', [theClass, theInstancePtr]));
{$ENDIF}
end;

procedure DBG_NotifyDestruction(const theClass: string; theInstancePtr: Pointer);
begin
{$IFDEF DBG_TRACK_OBJECT_CREATION_DESTRUCTION}
  DBG_Output(Format('Destroying %s instance at %p', [theClass, theInstancePtr]));
{$ENDIF}
end;

//==========================================================================

{ TSubMatches }

type
  TSubMatches = class(TInterfacedObject, ISubMatches)
  private
    FSubMatches : VBScript_ISubMatches;

  public
    constructor Create(theSubMatches: IDispatch);
    destructor  Destroy; override;

    function GetCount: Integer;
    function GetItem(index: Integer): OleVariant;
  end;

//==========================================================================

constructor TSubMatches.Create(theSubMatches: IDispatch);

begin
  DBG_NotifyCreation('TSubMatches', Self);
  FSubMatches := theSubMatches as VBScript_ISubMatches;
end;

//==========================================================================

destructor TSubMatches.Destroy;

begin
  DBG_NotifyDestruction('TSubMatches', Self);
end;

//==========================================================================

function TSubMatches.GetCount: Integer;

begin
  Result := FSubMatches.Count;
end;

//==========================================================================

function TSubMatches.GetItem(index: Integer): OleVariant;

begin
  Result := FSubMatches[index];
end;

//==========================================================================
//==========================================================================
//==========================================================================

{ TMatch }

type
  TMatch = class(TInterfacedObject, IMatch)
  private
    FMatch  : VBScript_IMatch2;

  public
    constructor Create(theMatch: IDispatch);
    destructor  Destroy; override;

    function GetValue: WideString;
    function GetFirstIndex: Integer;
    function GetLength: Integer;
    function GetSubMatches: ISubMatches;
  end;

//==========================================================================

constructor TMatch.Create(theMatch: IDispatch);

begin
  DBG_NotifyCreation('TMatch', Self);
  FMatch := theMatch as VBScript_IMatch2;
end;

//==========================================================================

destructor TMatch.Destroy;
begin
  DBG_NotifyDestruction('TMatch', Self);
end;

//==========================================================================

function TMatch.GetFirstIndex: Integer;

begin
  Result := FMatch.FirstIndex;
end;

//==========================================================================

function TMatch.GetLength: Integer;

begin
  Result := FMatch.Length;
end;

//==========================================================================

function TMatch.GetSubMatches: ISubMatches;

begin
  Result := TSubMatches.Create(FMatch.SubMatches);
end;

//==========================================================================

function TMatch.GetValue: WideString;

begin
  Result := FMatch.Value;
end;

//==========================================================================
//==========================================================================
//==========================================================================

{ TMatchCollection }

type
  TMatchCollection = class(TInterfacedObject, IMatchCollection)
  private
    FMatchCollection : VBScript_IMatchCollection2;

  public
    constructor Create(theMatchCollection: IDispatch);
    destructor  Destroy; override;

    function GetCount: Integer;
    function GetItem(index: Integer): IMatch;
  end;

//==========================================================================

constructor TMatchCollection.Create(theMatchCollection: IDispatch);

begin
  DBG_NotifyCreation('TMatchCollection', Self);
  FMatchCollection := theMatchCollection as VBScript_IMatchCollection2;
end;

//==========================================================================

destructor TMatchCollection.Destroy;

begin
  DBG_NotifyDestruction('TMatchCollection', Self);
end;

//==========================================================================

function TMatchCollection.GetCount: Integer;

begin
  Result := FMatchCollection.Count;
end;

//==========================================================================

function TMatchCollection.GetItem(index: Integer): IMatch;

begin
  Result := TMatch.Create(FMatchCollection[index]);
end;

//==========================================================================
//==========================================================================
//==========================================================================

{ TRegex }

type
  TRegex = class(TInterfacedObject, IRegex)
  private
    FRegex  : VBScript_IRegExp2;

  public
    constructor Create(theRegex: VBScript_IRegExp2);
    destructor  Destroy; override;

    function GetPattern: WideString;
    function GetIgnoreCase: Boolean;
    function GetMultiline: Boolean;

    function Match(const theSourceString: WideString): Boolean;

    function Find(const theSourceString: WideString): IMatchCollection;
    function FindAll(const theSourceString: WideString): IMatchCollection;

    function Replace(const theSourceString: WideString; const theReplaceVar: WideString): WideString;
    function ReplaceAll(const theSourceString: WideString; const theReplaceVar: WideString): WideString;
  end;

//==========================================================================

constructor TRegex.Create(theRegex: VBScript_IRegExp2);

begin
  DBG_NotifyCreation('TRegex', Self);
  FRegex := theRegex;
end;

//==========================================================================

destructor TRegex.Destroy;

begin
  DBG_NotifyDestruction('TRegex', Self);
end;

//==========================================================================

function TRegex.GetIgnoreCase: Boolean;

begin
  Result := FRegex.IgnoreCase;
end;

//==========================================================================

function TRegex.GetMultiLine: Boolean;

begin
  Result := FRegex.MultiLine;
end;

//==========================================================================

function TRegex.GetPattern: WideString;

begin
  Result := FRegex.Pattern;
end;

//==========================================================================

function TRegex.FindAll(const theSourceString: WideString): IMatchCollection;

begin
  try
    FRegex.Global := true;
    Result := TMatchCollection.Create(FRegex.Execute(theSourceString));
  except
    on E: EOleSysError do
      raise ERegexException.Create(E) at ExceptAddr;
  end;
end;

//==========================================================================

function TRegex.ReplaceAll(const theSourceString, theReplaceVar: WideString): WideString;

begin
  try
    FRegex.Global := true;
    Result := FRegex.Replace(theSourceString, theReplaceVar);
  except
    on E: EOleSysError do
      raise ERegexException.Create(E) at ExceptAddr;
  end;
end;

//==========================================================================

function TRegex.Match(const theSourceString: WideString): Boolean;

begin
  try
    FRegex.Global := false;
    Result := FRegex.Test(theSourceString);
  except
    on E: EOleSysError do
      raise ERegexException.Create(E) at ExceptAddr;
  end;
end;

//==========================================================================

function TRegex.Find(const theSourceString: WideString): IMatchCollection;
begin
  try
    FRegex.Global := false;
    Result := TMatchCollection.Create(FRegex.Execute(theSourceString));
  except
    on E: EOleSysError do
      raise ERegexException.Create(E) at ExceptAddr;
  end;
end;

//==========================================================================

function TRegex.Replace(const theSourceString: WideString; const theReplaceVar: WideString): WideString;

begin
  try
    FRegex.Global := false;
    Result := FRegex.Replace(theSourceString, theReplaceVar);
  except
    on E: EOleSysError do
      raise ERegexException.Create(E) at ExceptAddr;
  end;
end;

//==========================================================================

{ Regex }

class function Regex.Create(const thePattern: WideString; theOptions: TRegexOptions): IRegex;

var
  theRegex : VBScript_IRegExp2;

begin
  theRegex := CreateComObject(CLSID_RegExp) as VBScript_IRegExp2;

  theRegex.Pattern    := thePattern;
  theRegex.IgnoreCase := reIgnoreCase in theOptions;
  theRegex.MultiLine  := reMultiLine  in theOptions;

  Result := TRegex.Create(theRegex);
end;

//==========================================================================

class function Regex.Find(const thePattern, theSourceString: WideString): IMatchCollection;

begin
  Result := Regex.Create(thePattern).Find(theSourceString);
end;

//==========================================================================

class function Regex.Find   (const thePattern: WideString;
                                   theOptions: TRegexOptions;
                             const theSourceString: WideString): IMatchCollection;

begin
  Result := Regex.Create(thePattern, theOptions).Find(theSourceString);
end;

//==========================================================================

class function Regex.FindAll(const thePattern, theSourceString: WideString): IMatchCollection;

begin
  Result := Regex.Create(thePattern).FindAll(theSourceString);
end;

//==========================================================================

class function Regex.FindAll(const thePattern: WideString;
                                   theOptions: TRegexOptions;
                             const theSourceString: WideString): IMatchCollection;

begin
  Result := Regex.Create(thePattern, theOptions).FindAll(theSourceString);
end;

//==========================================================================

class function Regex.Match(const thePattern, theSourceString: WideString): Boolean;

begin
  Result := Regex.Create(thePattern).Match(theSourceString);
end;

//==========================================================================

class function Regex.Match(const thePattern: WideString;
                                 theOptions: TRegexOptions;
                           const theSourceString: WideString): Boolean;

begin
  Result := Regex.Create(thePattern, theOptions).Match(theSourceString);
end;

//==========================================================================

class function Regex.Replace(const thePattern, theSourceString: WideString;
                             const theReplaceVar : WideString): WideString;

begin
  Result := Regex.Create(thePattern).Replace(theSourceString, theReplaceVar);
end;

//==========================================================================

class function Regex.Replace   (const thePattern: WideString;
                                      theOptions: TRegexOptions;
                                const theSourceString: WideString;
                                const theReplaceVar  : WideString): WideString;
begin
  Result := Regex.Create(thePattern, theOptions).Replace(theSourceString, theReplaceVar);
end;

//==========================================================================

class function Regex.ReplaceAll(const thePattern, theSourceString: WideString;
                                const theReplaceVar : WideString): WideString;

begin
  Result := Regex.Create(thePattern).ReplaceAll(theSourceString, theReplaceVar);
end;

//==========================================================================

class function Regex.ReplaceAll(const thePattern: WideString;
                                      theOptions: TRegexOptions;
                                const theSourceString: WideString;
                                const theReplaceVar  : WideString): WideString;
begin
  Result := Regex.Create(thePattern, theOptions).ReplaceAll(theSourceString, theReplaceVar);
end;

//==========================================================================

end.

