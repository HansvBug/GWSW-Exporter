{$region 'license'}
{ <begin bsd 3 clause license>
  Copyright (c) 2024-2025, Benny Christensen a.k.a. cdbc
  Portions used from the FreePascal RTL, are Copyright their respective owners.

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the names of Interfaces,Classes or Enumerators nor the names
      of its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
<end of bsd 3 license> }
{$endregion 'license'}
unit istrlist;
{$mode ObjFPC}{$H+}
{-$define useistreams}
{$interfaces com}
{$WARN 5024 off : Parameter "$1" not used}
interface
uses classes,sysutils{$if fpc_fullversion >= 30301},sortbase{$ifend},types
  {$ifdef useistreams},istreams{$endif};
const
  islVersion = '12.25.11.2025'; ///// for public domain \\\\\
  SGUIDIStrings = '{5656C03B-C051-4CD7-A10D-35E2F910AE98}';
  SGUIDIStringList = '{B21ED2E0-58F4-4CBA-8F60-D6D85CBDE506}';

type
  { IStringList = COM interface + implementation }
  IslStringsEnumerator = specialize IEnumerator<string>;
  IslStringsEnumerable = specialize IEnumerable<string>;
  { Callback that lets the user send arbitrary 'aData' along in the 'ForEach' call,
    which gets passed untouched through, to be at the user's disposal in callback.
    sig: procedure(const aValue: string; const anIdx: ptrint; anObj: TObject; aData: pointer) of object; }
  TStringsForEachMethodExObjData = procedure(const aValue: string; const anIdx: ptrint; anObj: TObject; aData: pointer) of object;
  { IStrings is a com interface for the standard TStrings in FPC's RTL,
    usage:
      st:= CreateStrings; // in this case you can instantiate istrings :o)
      ... use st here ...
      st:= nil; // or just let fpc manage it }
  IStrings = interface(IInterface)[SGUIDIStrings]
    { added getters & setters not found in TStrings }
    function get_Adapter: IStringsAdapter;
    function get_AlwaysQuote: boolean; 
    function get_AsBytes: TBytes;
    function get_CommaText: string;
    function get_DefaultEncoding: TEncoding;
    function get_Delimiter: Char;
    function get_Encoding: TEncoding;
    function get_LBS: TTextLineBreakStyle;
    function get_LineBreak: string;
    function get_MNVSAction: TMissingNameValueSeparatorAction;
    function get_Name(Index: Integer): string;
    function get_NameValueSeparator: Char;
    function get_Options: TStringsOptions;
    function get_QuoteChar: Char;
    function get_SkipLastLineBreak: boolean;
    function get_StrictDelimiter: boolean;
    function get_TrailingLineBreak: boolean;
    function get_UseLocale: boolean;
    function get_Value(const Name: string): string;
    procedure set_AlwaysQuote(aValue: boolean); 
    procedure set_AsBytes(aValue: TBytes);
    procedure set_CommaText(aValue: string);
    procedure set_Debug(aValue: boolean);
    procedure set_DefaultEncoding(aValue: TEncoding);
    procedure set_Delimiter(aValue: Char);
    procedure set_LBS(aValue: TTextLineBreakStyle);
    procedure set_LineBreak(aValue: string);
    procedure set_MNVSAction(aValue: TMissingNameValueSeparatorAction);
    procedure set_NameValueSeparator(aValue: Char);
    procedure set_Options(aValue: TStringsOptions);
    procedure set_QuoteChar(aValue: Char);
    procedure set_SkipLastLineBreak(aValue: boolean);
    procedure set_StrictDelimiter(aValue: boolean);
    procedure set_StringsAdapter(aValue: IStringsAdapter);
    procedure set_TrailingLineBreak(aValue: boolean);
    procedure set_UseLocale(aValue: boolean);
    procedure set_Value(const Name: string; aValue: string);
    { methods & properties surfaced from TStrings }
    function Add(const S: string): Integer; overload;
    function AddObject(const S: string; AObject: TObject): Integer; overload;
    function Add(const Fmt : string; const Args : Array of const): Integer; overload;
    function AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer; overload;
    function AddPair(const AName, AValue: string): TStrings; overload;
    function AddPair(const AName, AValue: string; AObject: TObject): TStrings; overload;
    procedure AddStrings(TheStrings: TStrings); overload;
    procedure AddStrings(TheStrings: TStrings; ClearFirst : Boolean); overload;
    procedure AddStrings(const TheStrings: array of string); overload;
    procedure AddStrings(const TheStrings: array of string; ClearFirst : Boolean); overload;
    procedure AddText(Const S : String);
    procedure AddCommaText(const S: String);
    procedure AddDelimitedText(const S: String; ADelimiter: Char; AStrictDelimiter: Boolean); overload;
    procedure AddDelimitedtext(const S: String); overload;
    function AddOrSet(const aValue: string; anObj: TObject = nil): ptrint; overload;
    function AddOrSet(const aName,aValue: string; anObj: TObject = nil): ptrint; overload;
    procedure Append(const S: string);
    procedure Assign(Source: TPersistent); 
    procedure AssignEx(aSrc: IStrings;aClearFirst: boolean = true);
    procedure AssignFrom(aSrc: TStrings;aClearFirst: boolean = false);
    procedure AssignToEx(aDest: TStrings;aClearFirst: boolean = true);
    { Don't free this one! ...or death, murder & mayhem may occur!!! }
    function AsTStrings: TStrings;
    procedure BeginUpdate;
    procedure Clear;
    function CountItems: ptrint;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    function Equals(Obj: TObject): Boolean; override; overload;
    function Equals(TheStrings: TStrings): Boolean; overload;
    procedure Exchange(Index1, Index2: Integer);
    function  ExtractName(Const S:String):String;
    procedure Filter(aFilter: TStringsFilterMethod; aList: TStrings); overload;
    function Filter(aFilter: TStringsFilterMethod):  TStrings; overload;
    procedure Fill(const aValue : String; aStart,aEnd : Integer);
    procedure ForEach(aCallback: TStringsForeachMethod);
    procedure ForEach(aCallback: TStringsForeachMethodEx);
    procedure ForEach(aCallback: TStringsForeachMethodExObj);
    procedure ForEach(aCallback: TStringsForEachMethodExObjData;UserData: pointer); overload; ///bc 200824
    procedure ForEachReverse(aCallback: TStringsForEachMethodExObjData;UserData: pointer); ///bc 200824
    procedure FPOAttachObserver(AObserver: TObject);
    procedure FPODetachObserver(AObserver: TObject);
    procedure FPONotifyObservers(ASender: TObject; AOperation: TFPObservedOperation; Data: Pointer);
    function Get(Index: Integer): string;
    function GetCapacity: Integer;
    function GetDelimitedText: string;
    function GetEnumerator: IslStringsEnumerator;
    procedure GetNameValue(Index : Integer; Out AName,AValue : String);
    function GetObject(Index: Integer): TObject;
    function GetText: PChar;
    function GetTextStr: string;
    function GetValueFromIndex(Index: Integer): string;
    function GetWriteBOM: Boolean;
    function IndexOf(const S: string): Integer; overload;
    function IndexOf(const S: string; aStart : Integer): Integer; overload;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    function LastIndexOf(const S: string; aStart : Integer): Integer; overload;
    function LastIndexOf(const S: string): Integer; overload;
    procedure LoadFromFile(const FileName: string); overload;
    procedure LoadFromFile(const FileName: string; IgnoreEncoding : Boolean); overload;
    procedure LoadFromFile(const FileName: string; AEncoding: TEncoding); overload;
    procedure LoadFromStream(Stream: TStream); overload;
    procedure LoadFromStream(Stream: TStream; IgnoreEncoding : Boolean); overload;
    procedure LoadFromStream(Stream: TStream; AEncoding: TEncoding); overload;
    {$ifdef useistreams}
    procedure LoadFromStreamEx(aStream: IStreamFP); overload;
    procedure LoadFromStreamEx(aStream: IStreamFP; IgnoreEncoding : boolean); overload;
    procedure LoadFromStreamEx(aStream: IStreamFP; anEncoding: TEncoding); overload;
    {$endif}
    procedure Map(aMap: TStringsMapMethod; aList: TStrings);
    function Map(aMap: TStringsMapMethod): TStrings;
    procedure Move(CurIndex, NewIndex: Integer);
    function Pop: String; 
    procedure Put(Index: Integer; const S: string);
    procedure PutObject(Index: Integer; AObject: TObject);
    function Reduce(aReduceMethod: TStringsReduceMethod; const startingValue: string): string;
    function RefCount: longint;
    function Reverse: TStrings;
    procedure Reverse(aList: TStrings);
    function ReverseEx: IStrings; { returns an 'IStringlist', but it's not defined yet ;) } 
    procedure ReverseSelf;
    procedure SaveToFile(const FileName: string); overload;
    procedure SaveToFile(const FileName: string; IgnoreEncoding : Boolean); overload;
    procedure SaveToFile(const FileName: string; AEncoding: TEncoding); overload;
    procedure SaveToStream(Stream: TStream); overload;
    procedure SaveToStream(Stream: TStream; IgnoreEncoding : Boolean); overload;
    procedure SaveToStream(Stream: TStream; AEncoding: TEncoding); overload;
    {$ifdef useistreams}
    procedure SaveToStreamEx(aStream: IStreamFP); overload;
    procedure SaveToStreamEx(aStream: IStreamFP; IgnoreEncoding: boolean); overload;
    procedure SaveToStreamEx(aStream: IStreamFP; anEncoding: TEncoding); overload; 
    {$endif}
    procedure ScanFor(const aStr: string;aCallback: TStringsForEachMethodExObjData;UserData: pointer); ///bc 261124
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetDelimitedText(Const aValue: string);
    procedure SetStrings(TheStrings: TStrings); overload;
    procedure SetStrings(TheStrings: array of string); overload;
    procedure SetText(TheText: PChar);
    procedure SetTextStr(const Value: string);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    procedure SetWriteBOM(AValue: Boolean);
    function Shift: string;
    procedure Slice(fromIndex: integer; aList: TStrings);
    function Slice(fromIndex: integer): TStrings;
    function ToObjectArray(aStart,aEnd : Integer) : TObjectDynArray; overload;
    function ToObjectArray: TObjectDynArray; overload;
    {$if fpc_fullversion >= 30301}
    function ToString: RTLString;
    {$else}
    function ToString: ansistring;
    {$ifend}
    function ToStringArray(aStart,aEnd : Integer) : TStringDynArray; overload;
    function ToStringArray: TStringDynArray; overload;
    { properties galore }
    property AlwaysQuote: Boolean read get_AlwaysQuote write set_AlwaysQuote;
    { useful for working with sqldb-blob-fields - TBlobData = TBytes; }
    property AsBytes: TBytes read get_AsBytes write set_AsBytes;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read get_CommaText write set_CommaText;
    property Count: ptrint read CountItems;
    property DefaultEncoding: TEncoding read get_DefaultEncoding write set_DefaultEncoding;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Delimiter: Char read get_Delimiter write set_Delimiter;
    property Encoding: TEncoding read get_Encoding;
    property LineBreak: string Read get_LineBreak write set_LineBreak;
    property MissingNameValueSeparatorAction: TMissingNameValueSeparatorAction read get_MNVSAction write set_MNVSAction;
    property Names[Index: Integer]: string read get_Name;
    property NameValueSeparator: Char read get_NameValueSeparator write set_NameValueSeparator;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Options: TStringsOptions read get_Options write set_Options;
    property QuoteChar: Char read get_QuoteChar write set_QuoteChar;
    property SkipLastLineBreak: boolean read get_SkipLastLineBreak write set_SkipLastLineBreak;
    property StrictDelimiter: boolean read get_StrictDelimiter write set_StrictDelimiter;
    property Strings[Index: Integer]: string read Get write Put; default;
    property StringsAdapter: IStringsAdapter read get_Adapter write set_StringsAdapter;
    property Text: string read GetTextStr write SetTextStr;
    property TextLineBreakStyle: TTextLineBreakStyle read get_LBS write set_LBS;
    { Same as SkipLastLineBreak but for Delphi compatibility. Note it has opposite meaning! }
    property TrailingLineBreak : Boolean read get_TrailingLineBreak write set_TrailingLineBreak;
    property UseLocale: boolean read get_UseLocale write set_UseLocale;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property Values[const Name: string]: string read get_Value write set_Value;
  end; { IStrings }

  { IStringList is a com-interface for the standard TStringList in FPC's RTL,
    usage:
      sl:= CreateStrList;
      ... use sl here ...
      sl:= nil; // or just let fpc manage it }
  IStringList = interface(IStrings)[SGUIDIStringList]
    { added getters & setters not found in TStringList }
    function get_CaseSensitive: Boolean;
    function get_Duplicates: TDuplicates;  
    function get_OnChange: TNotifyEvent; 
    function get_OnChanging: TNotifyEvent; 
    function get_OwnsObjects: boolean;
    function get_SortStyle: TStringsSortStyle;  
    procedure set_CaseSensitive(aValue: Boolean);
    procedure set_Duplicates(aValue: TDuplicates);
    procedure set_OnChange(aValue: TNotifyEvent);  
    procedure set_OnChanging(aValue: TNotifyEvent); 
    procedure set_OwnsObjects(aValue: boolean);
    procedure set_SortStyle(aValue: TStringsSortStyle);
    { methods & properties surfaced from TStringList }
    function Find(const S: string; Out Index: Integer): Boolean;
    function GetSorted: Boolean;
    { BEWARE: don't even think about freeing this one!!! }
    function List: TStringList;
    procedure SetSorted(Value: Boolean);
    procedure Sort;
    procedure CustomSort(CompareFn: TStringListSortCompare); 
    {$if fpc_fullversion >= 30301}
    procedure Sort(SortingAlgorithm: PSortingAlgorithm);
    procedure CustomSort(CompareFn: TStringListSortCompare; SortingAlgorithm: PSortingAlgorithm);
    {$ifend}
    property CaseSensitive: Boolean read get_CaseSensitive write set_CaseSensitive;
    property Count: ptrint read CountItems;  
    property Duplicates: TDuplicates read get_Duplicates write set_Duplicates; 
    property OnChange: TNotifyEvent read get_OnChange write set_OnChange; 
    property OnChanging: TNotifyEvent read get_OnChanging write set_OnChanging; 
    property OwnsObjects: boolean read get_OwnsObjects write set_OwnsObjects;
    property Sorted: Boolean read GetSorted write SetSorted; 
    Property SortStyle : TStringsSortStyle Read get_SortStyle Write set_SortStyle;
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;
  end; { IStringList }

{$region 'TiStringListH'}
  type
    { TislStringsEnumerator for TiStringList }
    TislStringsEnumerator = class(TInterfacedObject,IslStringsEnumerator)
    private
      fStrings: IStrings;
      fPosition: ptrint;
    public
      constructor Create(aStrings: IStrings);
      function GetCurrent: string;
      function MoveNext: boolean;
      procedure Reset;
      property Current: string read GetCurrent;
    end;
    { TiStringList; NOTE: an object MUST list ALL the interfaces it implements,
      in order for it to be able to deliver said interfaces, by way of getinterface! }
    TiStringList = class(TStringList,IStrings,IStringList,IslStringsEnumerable)
    private
      fDebugOn: boolean;
      function get_Adapter: IStringsAdapter;
      function get_AlwaysQuote: boolean;
      function get_AsBytes: TBytes;
      function get_CaseSensitive: Boolean;
      function get_CommaText: string;
      function get_DefaultEncoding: TEncoding;
      function get_Delimiter: Char;
      function get_Duplicates: TDuplicates;
      function get_Encoding: TEncoding;
      function get_LBS: TTextLineBreakStyle;
      function get_LineBreak: string;
      function get_MNVSAction: TMissingNameValueSeparatorAction;
      function get_Name(Index: Integer): string;
      function get_NameValueSeparator: Char;
      function get_Options: TStringsOptions;
      function get_OnChange: TNotifyEvent;
      function get_OnChanging: TNotifyEvent;
      function get_OwnsObjects: boolean;
      function get_QuoteChar: Char;
      function get_SkipLastLineBreak: boolean;
      function get_SortStyle: TStringsSortStyle;
      function get_StrictDelimiter: boolean;
      function get_TrailingLineBreak: boolean;
      function get_UseLocale: boolean;
      function get_Value(const Name: string): string;
      function GetSorted: Boolean;
      function GetWriteBOM: boolean;
      procedure set_AlwaysQuote(aValue: boolean);
      procedure set_AsBytes(aValue: TBytes);
      procedure set_CaseSensitive(aValue: boolean);
      procedure set_CommaText(aValue: string);
      procedure set_Debug(aValue: boolean);
      procedure set_DefaultEncoding(aValue: TEncoding);
      procedure set_Delimiter(aValue: Char);
      procedure set_Duplicates(aValue: TDuplicates);
      procedure set_LBS(aValue: TTextLineBreakStyle);
      procedure set_LineBreak(aValue: string);
      procedure set_MNVSAction(aValue: TMissingNameValueSeparatorAction);
      procedure set_NameValueSeparator(aValue: Char);
      procedure set_Options(aValue: TStringsOptions);
      procedure set_OnChange(aValue: TNotifyEvent);
      procedure set_OnChanging(aValue: TNotifyEvent);
      procedure set_OwnsObjects(aValue: boolean);
      procedure set_QuoteChar(aValue: Char);
      procedure set_SkipLastLineBreak(aValue: boolean);
      procedure set_SortStyle(aValue: TStringsSortStyle);
      procedure set_StrictDelimiter(aValue: boolean);
      procedure set_StringsAdapter(aValue: IStringsAdapter);
      procedure set_TrailingLineBreak(aValue: boolean);
      procedure set_UseLocale(aValue: boolean);
      procedure set_Value(const Name: string; aValue: string);
      procedure SetSorted(Value: Boolean);
      procedure SetWriteBOM(aValue: boolean);
    protected
      fRefCount: longint;
      fDestroyCount: longint;
      function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid;out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      procedure AddToDest(const aValue: string; const {%H-}anIdx: ptrint; anObj: TObject; aData: pointer);
      procedure AddToSelf(const CurrentValue: string; const {%H-}index: integer; Obj : TObject); // foreach
      procedure AssignEx(aSrc: IStrings;aClearFirst: boolean = true);
      procedure AssignFrom(aSrc: TStrings;aClearFirst: boolean = false);
      procedure AssignToEx(aDest: TStrings;aClearFirst: boolean = true);
      function AsTStrings: TStrings;
      function CountItems: ptrint;
      function List: TStringList;
    public
      destructor Destroy; override;
      procedure AfterConstruction;override;
      procedure BeforeDestruction;override;
      function AddOrSet(const aValue: string; anObj: TObject = nil): ptrint; overload;
      function AddOrSet(const aName,aValue: string; anObj: TObject = nil): ptrint; overload;
      procedure ForEach(aCallback: TStringsForEachMethodExObjData;UserData: pointer); overload; ///bc 200824
      procedure ForEachReverse(aCallback: TStringsForEachMethodExObjData;UserData: pointer); ///bc 200824
      function GetEnumerator: IslStringsEnumerator; { IEnumerable }
      function IndexOf(const S: string): Integer; override; { TStrings + TStringList }
      function IndexOf(const S: string; aStart : Integer): Integer; override; { TStrings }
      {$ifdef useistreams}
      procedure LoadFromStreamEx(aStream: IStreamFP); overload;
      procedure LoadFromStreamEx(aStream: IStreamFP; IgnoreEncoding : boolean); overload;
      procedure LoadFromStreamEx(aStream: IStreamFP; anEncoding: TEncoding); overload;
      {$endif}
      class function NewInstance : TObject;override;
      function RefCount: longint;
      {$ifdef useistreams}
      procedure SaveToStreamEx(aStream: IStreamFP); overload;
      procedure SaveToStreamEx(aStream: IStreamFP; IgnoreEncoding: boolean); overload;
      procedure SaveToStreamEx(aStream: IStreamFP; anEncoding: TEncoding); overload;
      {$endif}
      function Reverse: TStrings; reintroduce;
      procedure Reverse(aList: TStrings); reintroduce;
      function ReverseEx: IStrings; 
      procedure ReverseSelf;
      procedure ScanFor(const aStr: string;aCallback: TStringsForEachMethodExObjData;UserData: pointer);
      {$if fpc_fullversion >= 30301}
      function ToString: RTLString; override;
      {$else}
      function ToString: ansistring; override;
      {$ifend}
      property AsBytes: TBytes read get_AsBytes write set_AsBytes;
    end; { TiStringList }
{$endregion 'TiStringListH'}

{ factory function for IString(s/list), but since they share implementor behind the scenes,
  this has just become a shorter alias :) }
function CreStrings: IStringList; public name 'BC_CRESTRINGS';
function CreateStrings: IStrings; deprecated 'Use CreStrings instead.';
{ factory function for IStringlist, no frills }
function CreateStrList: IStringList;
{ it is the user's responsibility, to ensure that the 'a(B)inary (l)arge (o)bject of (b)yte'
  constitutes a body of text, i.e.: strings. this factory performs NO checks!!! }
function CreStrListFromBytes(const aBlob: array of byte): IStringList; public name 'BC_CRESTRLISTFROMBYTES';
{ this checks, to ensure that the 'aTextfileName' exists as a file. user must make sure it
  constitutes a body of text, i.e.: strings. this factory performs NO text-checks!!! }
function CreStrListFromFile(const aTextfileName: string): IStringList; public name 'BC_CRESTRLISTFROMFILE';
{ if fed a valid TStream-object, this factory will return a stringlist loaded with said data }
function CreStrListFromStream(const aTextStream: TStream;aFromBeginning: boolean = true): IStringList; public name 'BC_CRESTRLISTFROMSTREAM';
{ if fed a valid TStrings-object, this factory will return a stringlist loaded with said data }
function CreStrListFromStrings(const aSrc: TStrings): IStringList; public name 'BC_CRESTRLISTFROMSTRINGS';
{ 2 functions for inclusion in plugin-framework }
{ GetISLSvcWeak is a "ServiceProvider" function for use with registering plugin services,
  you call it like this: pointer(fsl):= GetISLSvcWeak(); or  you can use the factoryGEN
  to assign: fStrlist:= PluginMgr.FactoryGen^.specialize GetSvc<IStringList>('StrList');
  COM-object, so do: fStrlist:= nil; when you're done }
function GetISLSvcWeak: pointer;
{ GetISLSvcWeakParams is a "ServiceProvider" function for use with registering plugin services.
  This one takes no parameters. you call it like this:
  pointer(fStrlist):= GetISLSvcWeakParams([]); i.e.: no param!
  fStrlist:= PluginMgr.FactoryGen^.specialize GetSvcParams<IStringList>([]));
  works! COM-object, so do fStrlist:= nil; when you're done :o) }
function GetISLSvcWeakParams(const Args: array of const): pointer;

implementation

{$region 'factory'}
function CreStrings: IStringList;
begin
  Result:= TiStringList.Create as IStringList;
end;

function CreateStrings: IStrings;
begin
  Result:= CreStrings as IStrings;
end;

function CreateStrList: IStringList;
begin
  Result:= TiStringList.Create as IStringList;
end;

function CreStrListFromBytes(const aBlob: array of byte): IStringList;
var lms: TMemoryStream;
begin
  Result:= TiStringList.Create as IStringList;
  lms:= TMemoryStream.Create; try
    if Length(aBlob) > 0 then begin
      lms.WriteBuffer(aBlob[0],Length(aBlob));
      lms.Position:= 0;
      Result.LoadFromStream(lms);
    end;
  finally lms.Free; end;
end;

function CreStrListFromFile(const aTextfileName: string): IStringList;
begin
  Result:= TiStringList.Create as IStringList;
  if ((aTextfileName <> '') and (FileExists(aTextfileName))) then
    Result.LoadFromFile(aTextfileName);
end;

function CreStrListFromStream(const aTextStream: TStream; aFromBeginning: boolean): IStringList;
begin { the result is always a valid stringlist, regardless of input }
  Result:= TiStringList.Create as IStringList;
  if Assigned(aTextStream) then begin
    if aFromBeginning then aTextStream.Position:= 0; { flexible :o) }
    Result.LoadFromStream(aTextStream); { no need to clear first }
    aTextStream.Position:= 0;
  end;
end;

function CreStrListFromStrings(const aSrc: TStrings): IStringList;
begin { the result is always a valid stringlist, regardless of input }
  Result:= TiStringList.Create as IStringList;
  if Assigned(aSrc) then Result.AssignFrom(aSrc); { no need to clear first }
end;

function GetISLSvcWeak: pointer;
begin { no params }
  TiStringList
    .Create
    .GetInterface(SGUIDIStringList,Result);
end;

function GetISLSvcWeakParams(const Args: array of const): pointer;
begin { no params }
  TiStringList
    .Create
    .GetInterface(SGUIDIStringList,Result);
end;
{$endregion 'factory'}
{$region 'TiStringList'}
{ TiStringList }
function TiStringList.RefCount: longint;
begin
  Result:= fRefCount;
end;

function TiStringList.Reverse: TStrings; { reintroduce }
begin
  Result:= TStringsClass(Self.ClassType).Create;
  try Reverse(Result);
  except FreeAndNil(Result); Raise; end;
end;

procedure TiStringList.Reverse(aList: TStrings); { reintroduce }
var li: ptrint;
begin { the existing one in classes wipes the objects -- they were gone afterwards!!! }
  if aList = Self then begin ReverseSelf; exit; end; { someone might do this }
  for li:= inherited Count-1 downto 0 do
    aList.AddObject(Strings[li],Objects[li]); { copy strings & objects in reverse order to aList }
end;

function TiStringList.ReverseEx: IStrings;
begin
  Result:= CreStrings; { we need a new result stringlist }
  Reverse(Result.AsTStrings); { now get the reversed result from reintroduced method }
end;

procedure TiStringList.ReverseSelf;
var lsl: IStringList; { we need an interim list }
begin
  lsl:= ReverseEx as IStringList; { fetch our own reverse }
  AssignEx(lsl); { now clear ourselves and assign the reverse list to us }
end;

{$ifdef useistreams}
procedure TiStringList.SaveToStreamEx(aStream: IStreamFP); //=^
begin
  SaveToStream(aStream.AsTStream);
end;

procedure TiStringList.SaveToStreamEx(aStream: IStreamFP; IgnoreEncoding: boolean); //=^
begin
  SaveToStream(aStream.AsTStream,IgnoreEncoding);
end;

procedure TiStringList.SaveToStreamEx(aStream: IStreamFP; anEncoding: TEncoding); //=^
begin
  SaveToStream(aStream.AsTStream,anEncoding);
end;
{$endif}

procedure TiStringList.ScanFor(const aStr: string; aCallback: TStringsForEachMethodExObjData; UserData: pointer);
var li: ptrint = 0; ustr: UnicodeString;
begin { I'll bet this will be slow with bigger data, but hey... }
  ustr:= UTF8Decode(aStr);
  for li:= 0 to Count-1 do begin
    if (pos(uStr,UTF8Decode(Strings[li])) > 0) then aCallback(Strings[li],li,Objects[li],UserData);
  end;
end;

function TiStringList.get_Adapter: IStringsAdapter;
begin
  Result:= inherited StringsAdapter;
end;

function TiStringList.get_AlwaysQuote: boolean;
begin
  Result:= inherited AlwaysQuote;
end;

function TiStringList.get_AsBytes: TBytes;
begin
  Result:= BytesOf(Self.Text);
end;

function TiStringList.get_CaseSensitive: Boolean;
begin
  Result:= inherited CaseSensitive;
end;

function TiStringList.get_CommaText: string;
begin
  Result:= inherited CommaText;
end;

function TiStringList.get_DefaultEncoding: TEncoding;
begin
  Result:= inherited DefaultEncoding;
end;

function TiStringList.get_Delimiter: Char;
begin
  Result:= inherited Delimiter;
end;

function TiStringList.get_Duplicates: TDuplicates;
begin
  Result:= inherited Duplicates;
end;

function TiStringList.get_Encoding: TEncoding;
begin
  Result:= inherited Encoding;
end;

function TiStringList.get_LBS: TTextLineBreakStyle;
begin
  Result:= inherited TextLineBreakStyle;
end;

function TiStringList.get_LineBreak: string;
begin
  Result:= inherited LineBreak;
end;

function TiStringList.get_MNVSAction: TMissingNameValueSeparatorAction;
begin
  Result:= inherited MissingNameValueSeparatorAction;
end;

function TiStringList.get_Name(Index: Integer): string;
begin
  Result:= inherited Names[Index];
end;

function TiStringList.get_NameValueSeparator: Char;
begin
  Result:= inherited NameValueSeparator;
end;

function TiStringList.get_Options: TStringsOptions;
begin
  Result:= inherited Options;
end;

function TiStringList.get_OnChange: TNotifyEvent;
begin
  Result:= inherited OnChange;
end;

function TiStringList.get_OnChanging: TNotifyEvent;
begin
  Result:= inherited OnChanging;
end;

function TiStringList.get_OwnsObjects: boolean;
begin
  Result:= inherited OwnsObjects;
end;

function TiStringList.get_QuoteChar: Char;
begin
  Result:= inherited QuoteChar;
end;

function TiStringList.get_SkipLastLineBreak: boolean;
begin
  Result:= inherited SkipLastLineBreak;
end;

function TiStringList.get_StrictDelimiter: boolean;
begin
  Result:= inherited StrictDelimiter;
end;

function TiStringList.get_TrailingLineBreak: boolean;
begin
  Result:= inherited TrailingLineBreak;
end;

function TiStringList.get_UseLocale: boolean;
begin
  Result:= inherited UseLocale;
end;

function TiStringList.get_Value(const Name: string): string;
begin
  Result:= inherited Values[Name];
end;

function TiStringList.GetSorted: Boolean;
begin
  Result:= inherited SortStyle in [sslUser,sslAuto];
end;

function TiStringList.GetWriteBOM: boolean;
begin
  Result:= inherited WriteBOM;
end;

procedure TiStringList.set_AlwaysQuote(aValue: boolean);
begin
  inherited AlwaysQuote:= aValue;
end;

function BytesToStr(const aBlob: TBytes): string;
var len: ptrint;
begin
  len:= Length(aBlob);
  SetLength(Result,len);
  if len > 0 then Move(aBlob[0],Result[1],len);
end;

procedure TiStringList.set_AsBytes(aValue: TBytes);
begin    { this doesn't discriminate, it will set the 'Text' property }
  Clear; { to exactly what's in the array of bytes!!! (even nothing) }
  Self.Text:= BytesToStr(aValue);
end;

function TiStringList.get_SortStyle: TStringsSortStyle;
begin
  Result:= inherited SortStyle;
end;

procedure TiStringList.set_CaseSensitive(aValue: boolean);
begin
  inherited CaseSensitive:= aValue;
end;

procedure TiStringList.set_CommaText(aValue: string);
begin
  inherited CommaText:= aValue;
end;

procedure TiStringList.set_Debug(aValue: boolean);
begin
  fDebugOn:= aValue;
end;

procedure TiStringList.set_DefaultEncoding(aValue: TEncoding);
begin
  inherited DefaultEncoding:= aValue;
end;

procedure TiStringList.set_Delimiter(aValue: Char);
begin
  inherited Delimiter:= aValue;
end;

procedure TiStringList.set_Duplicates(aValue: TDuplicates);
begin
  inherited Duplicates:= aValue;
end;

procedure TiStringList.set_LBS(aValue: TTextLineBreakStyle);
begin
  inherited TextLineBreakStyle:= aValue;
end;

procedure TiStringList.set_LineBreak(aValue: string);
begin
  inherited LineBreak:= aValue;
end;

procedure TiStringList.set_MNVSAction(aValue: TMissingNameValueSeparatorAction);
begin
  inherited MissingNameValueSeparatorAction:= aValue;
end;

procedure TiStringList.set_NameValueSeparator(aValue: Char);
begin
  inherited NameValueSeparator:= aValue;
end;

procedure TiStringList.set_Options(aValue: TStringsOptions);
begin
  inherited Options:= aValue;
end;

procedure TiStringList.set_OnChange(aValue: TNotifyEvent);
begin
  inherited OnChange:= aValue;
end;

procedure TiStringList.set_OnChanging(aValue: TNotifyEvent);
begin
  inherited OnChanging:= aValue;
end;

procedure TiStringList.set_OwnsObjects(aValue: boolean);
begin
  inherited OwnsObjects:= aValue;
end;

procedure TiStringList.set_QuoteChar(aValue: Char);
begin
  inherited QuoteChar:= aValue;
end;

procedure TiStringList.set_SkipLastLineBreak(aValue: boolean);
begin
  inherited SkipLastLineBreak:= aValue;
end;

procedure TiStringList.SetSorted(Value: Boolean);
begin
  if Value then inherited SortStyle:= sslAuto
  else inherited SortStyle:= sslNone;
end;

procedure TiStringList.SetWriteBOM(aValue: boolean);
begin
  inherited WriteBOM:= aValue;
end;

procedure TiStringList.set_SortStyle(aValue: TStringsSortStyle);
begin
  inherited SortStyle:= aValue;
end;

procedure TiStringList.set_StrictDelimiter(aValue: boolean);
begin
  inherited StrictDelimiter:= aValue;
end;

procedure TiStringList.set_StringsAdapter(aValue: IStringsAdapter);
begin
  inherited StringsAdapter:= aValue;
end;

procedure TiStringList.set_TrailingLineBreak(aValue: boolean);
begin
  inherited TrailingLineBreak:= aValue;
end;

procedure TiStringList.set_UseLocale(aValue: boolean);
begin
  inherited UseLocale:= aValue;
end;

procedure TiStringList.set_Value(const Name: string; aValue: string);
begin
  inherited Values[Name]:= aValue;
end;

function TiStringList.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid;out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid,obj) then Result:= S_OK
  else Result:= longint(E_NOINTERFACE);
end;

function TiStringList._AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:= InterlockedIncrement(fRefCount);
end;

function TiStringList._Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:= InterlockedDecrement(fRefCount);
  if Result = 0 then begin
    if InterlockedIncrement(fDestroyCount) = 1 then Self.Destroy;
  end;
end;

procedure TiStringList.AddToDest(const aValue: string; const anIdx: ptrint; anObj: TObject; aData: pointer);
begin
  TStrings(aData).AddObject(aValue,anObj);
end;

procedure TiStringList.AddToSelf(const CurrentValue: string; const index: integer; Obj: TObject);
begin
  AddObject(CurrentValue,Obj);
end;

procedure TiStringList.AssignEx(aSrc: IStrings; aClearFirst: boolean);
begin
  if aClearFirst then Clear;
  aSrc.ForEach(@AddToSelf);
end;

procedure TiStringList.AssignFrom(aSrc: TStrings; aClearFirst: boolean);
begin
  if aClearFirst then Clear; 
  aSrc.ForEach(@AddToSelf);
end;

procedure TiStringList.AssignToEx(aDest: TStrings; aClearFirst: boolean);
begin                     
  if aClearFirst then aDest.Clear;
  ForEach(@AddToDest,aDest);
end;

function TiStringList.AsTStrings: TStrings;
begin
  Result:= Self;
end;

function TiStringList.CountItems: ptrint;
begin
  Result:= Count;
end;

function TiStringList.List: TStringList;
begin
  Result:= Self;
end;

destructor TiStringList.Destroy;
begin
  Clear;
  fRefCount:= 0;
  fDestroyCount:= 0;          if fDebugOn then writeln('IStringList destroyed');
  inherited Destroy;
end;

procedure TiStringList.AfterConstruction;
begin
  inherited AfterConstruction;
  { we need to fix the refcount we forced in newinstance further, it must be }
  InterlockedDecrement(fRefCount); { done in a thread safe way }
  { InterlockedDecrement is a function but it also decrements the var param }
end;

procedure TiStringList.BeforeDestruction;
begin
  if fRefCount <> 0 then raise Exception.CreateHelp('Error 204: [TiStringList.BeforeDestruction] Invalid pointer operation, RefCount <> 0',204);
  inherited BeforeDestruction;
end;

function TiStringList.AddOrSet(const aValue: string; anObj: TObject): ptrint;
begin { code inspired and mostly written by Thaddy }
  Result:= IndexOf(aValue);
  if Result = -1 then Result:= AddObject(aValue,anObj)
  else begin
    Strings[Result]:= aValue;
    Objects[Result]:= anObj;
  end;
end;

function TiStringList.AddOrSet(const aName, aValue: string; anObj: TObject): ptrint;
begin { code inspired and mostly written by Thaddy }
  Result:= IndexOf(aName);
  if Result = -1 then Result:= AddObject(aName+NameValueSeparator+aValue,anObj)
  else begin Values[aName]:= aValue; Objects[Result]:= anObj; end;
end;

procedure TiStringList.ForEach(aCallback: TStringsForEachMethodExObjData; UserData: pointer);
var i: ptrint;
begin
  for i:= 0 to Count-1 do aCallback(Strings[i],i,Objects[i],UserData);
end;

procedure TiStringList.ForEachReverse(aCallback: TStringsForEachMethodExObjData; UserData: pointer);
var i: ptrint;
begin
  for i:= Count-1 downto 0 do aCallback(Strings[i],i,Objects[i],UserData);
end;

function TiStringList.GetEnumerator: IslStringsEnumerator;
begin
  Result:= TislStringsEnumerator.Create(Self);
end;

function TiStringList.IndexOf(const S: string): Integer;
begin
  Result:= inherited IndexOf(S);
end;

function TiStringList.IndexOf(const S: string; aStart: Integer): Integer;
begin { replica of TStrings' implementation }
  if aStart < 0 then begin
    aStart:= Count + aStart;
    if aStart < 0 then aStart:= 0;
  end;
  Result:= aStart;
  While (Result < Count) and (DoCompareText(Strings[Result],S) <> 0) do Result:= Result + 1;
  if Result = Count then Result:= -1;
end;

{$ifdef useistreams}
procedure TiStringList.LoadFromStreamEx(aStream: IStreamFP); //=^
begin
  LoadFromStream(aStream.AsTStream);
end;

procedure TiStringList.LoadFromStreamEx(aStream: IStreamFP; IgnoreEncoding: boolean); //=^
begin
  LoadFromStream(aStream.AsTStream,IgnoreEncoding);
end;

procedure TiStringList.LoadFromStreamEx(aStream: IStreamFP; anEncoding: TEncoding); //=^
begin
  LoadFromStream(aStream.AsTStream,anEncoding);
end;
{$endif}

class function TiStringList.NewInstance: TObject;
begin
  Result:= inherited NewInstance;
  if Result <> nil then TiStringList(Result).fRefCount:= 1;
end;

{$if fpc_fullversion >= 30301}
function TiStringList.ToString: RTLString;
{$else}
function TiStringList.ToString: ansistring;
{$ifend}
begin
  Result:= 'IStrings & IStringList COM implementation, ver.: '+islVersion+LineEnding+
           'Copyright (c)2024-2025 Benny Christensen a.k.a. cdbc, All rights reserved!';
end;
{$endregion 'TiStringList'}
{$region 'TislStringsEnumerator'}
{ TislStringsEnumerator }
constructor TislStringsEnumerator.Create(aStrings: IStrings);
begin
  inherited Create;
  fStrings:= aStrings;
  fPosition:= -1;
end;

function TislStringsEnumerator.GetCurrent: string;
begin
  Result:= fStrings[fPosition];
end;

function TislStringsEnumerator.MoveNext: boolean;
begin
  Inc(fPosition);
  Result:= fPosition < fStrings.CountItems;
end;

procedure TislStringsEnumerator.Reset;
begin
  fPosition:= -1;
end;
{$endregion 'TislStringsEnumerator'}

end.

