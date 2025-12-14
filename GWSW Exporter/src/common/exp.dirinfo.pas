unit exp.dirinfo;
{$mode ObjFPC}{$H+}   (* This <experimental> unit only works fully with unices & linuces, NO WINDERS! *)

interface
uses sysutils,istrlist;
const
  idiVersion = '2.21.04.2025'; /// experimental 'manual interface' :o)
type
  { IDirInfo is a service that can tell you information about a directory on unices, you
    create it via a call to "GetDirInfo" and DON'T worry about freeing, it's automagical }
  IDirInfo = interface['{F4E40DBC-4488-428C-B666-2B665F9FD4A7}']
    { returns the unix-timestamp for a directory's last modification time, while
      at the same time spitting out the timestamp in the form of a 'TDateTime' }
    function DirAge(const aDirName: rawbytestring; out asDaTi: TDateTime): ptrint;
    { returns the unix-timestamp for a file's last modification time, while
      at the same time spitting out the timestamp in the form of a 'TDateTime' }
    function FileAge(const aFileName: rawbytestring; out asDaTi: TDateTime): ptrint;   
    { returns a stringlist with the directory-names (only) found in directory; NO files & NO recursion }
    function GetDirs(aDirName: string;IncludeFullPath: boolean = false): IStringList;
    { returns a stringlist with the filenames (only) found in directory; NO dirs & NO recursion }
    function GetFiles(aDirName: string;IncludeFullPath: boolean = false): IStringList;
  end;
{ service factory for IDirInfo :o) }
function GetDirInfo: IDirInfo;

implementation
{$ifdef unix} uses baseunix; {$endif}

type
  TInterface = class
  public
    function QueryInterface(constref {%H-}IID: TGUID;{%H-} out {%H-}Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual; abstract;
    function _Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};  virtual; abstract;
  end;

  TRawInterface = class(TInterface)
  public
    function _AddRef: LongInt; override;
    function _Release: LongInt; override;
  end;
  { muy importante memory layout for the interface }
  PDirInfoVMT = {%H-}^TDirInfoVMT;
  TDirInfoVMT = packed record
    QueryInterface: CodePointer;
    _AddRef: CodePointer;
    _Release: CodePointer;
    DirAge: CodePointer;
    FileAge: CodePointer; 
    GetDirs: CodePointer;
    GetFiles: CodePointer;
  end;

  { TDirInfo is the actual implementor of our interface }
  TDirInfo = class
    { i figured it would work with a class-function }
    class function DirAge(const aDirName: rawbytestring; out asDaTi: TDateTime): ptrint;
    { but that it *also* works, without the class-part, that baffles me a bit %) }
    function FileAge(const aFileName: rawbytestring; out asDaTi: TDateTime): ptrint;
    { returns a stringlist with the directory-names (only) found in directory; NO files & NO recursion }
    function GetDirs(aDirName: string;IncludeFullPath: boolean = false): IStringList;
    { returns a stringlist with the filenames (only) found in directory; NO dirs & NO recursion }
    function GetFiles(aDirName: string;IncludeFullPath: boolean = false): IStringList;
  end;
//////////////// area 51 ////////////////
const
  { please note that while this uses instance methods of TRawInterface this does not necessarily
    mean that a TRawInterface instance is passed in; e.g. the code in Generics.Defaults uses
    a different type as Self that contains the reference count for those types where the
    reference count is necessary }
  DirInfo_VMT: TDirInfoVMT = (QueryInterface: @TRawInterface.QueryInterface;
                              _AddRef: @TRawInterface._AddRef;
                              _Release: @TRawInterface._Release;
                              DirAge: @TDirInfo.DirAge;
                              FileAge: @TDirInfo.FileAge;
                              GetDirs: @TDirInfo.GetDirs;
                              GetFiles: @TDirInfo.GetFiles);
  { ...and here we instantiate the interface as a const pointer }
  DirInfo_Instance: pointer = @DirInfo_VMT;
//////////////// area 51 ////////////////
{ TInterface }
function TInterface.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IfNDef WINDOWS}cdecl{$Else}stdcall{$EndIf};
begin
  Result:= E_NOINTERFACE;
end;

{ TRawInterface }
function TRawInterface._AddRef: LongInt; {$IfNDef WINDOWS}cdecl{$Else}stdcall{$EndIf};
begin
 Result:= -1;        /// writeln('_AddRef');
end;

function TRawInterface._Release: LongInt; {$IfNDef WINDOWS}cdecl{$Else}stdcall{$EndIf};
begin
 Result:= -1;        /// writeln('_Release');
end;

{ TDirInfo }
class function TDirInfo.DirAge(const aDirName: rawbytestring; out asDaTi: TDateTime): ptrint;
{$ifdef unix}
var Info: baseunix.stat; SystemDirName: rawbytestring;
begin
  SystemDirName:= ToSingleByteFileSystemEncodedFileName(aDirName); /// compiler magic
  if fpstat(pansichar(SystemDirName),Info{%H-}) < 0 then begin asDaTi:= MinDateTime; exit(-1); end
  else begin
    Result:= Info.st_mtime; /// we're mostly interested in 'modified'
    asDaTi:= FileDateToDateTime(Result); /// just convenience for the user
  end;
{$else} begin asDaTi:= 0; Result:= -1; {$endif}
end; { DirAge }

function TDirInfo.FileAge(const aFileName:rawbytestring;out asDaTi:TDateTime): ptrint;
{$ifdef unix}
var Info: baseunix.stat; SystemFileName: rawbytestring;
begin
  SystemFileName:= ToSingleByteFileSystemEncodedFileName(aFileName); /// compiler magic
  if fpstat(pansichar(SystemFileName),Info{%H-}) < 0 then begin asDaTi:= MinDateTime; exit(-1); end
  else begin
    Result:= Info.st_mtime; /// we're mostly interested in 'modified'
    asDaTi:= FileDateToDateTime(Result); /// just convenience for the user
  end;
{$else} begin asDaTi:= 0; Result:= -1; {$endif}
end; { FileAge }

function TDirInfo.GetDirs(aDirName: string; IncludeFullPath: boolean): IStringList;
var finished: longint; found: TSearchRec;
begin
  Result:= CreStrings;
  if aDirName = '' then exit(Result);
  aDirName:= IncludeTrailingPathDelimiter(aDirName); { checks for (and) include(d) pathdelim }
  finished:= FindFirst(aDirName + '*',faAnyFile,found); { * and  faAnyFile works on linux }
  while finished = 0 do begin { the findxxx functions return 0 on success }
    if (found.Name[1] <> '.') then begin { subdue the pestering . & .. intrinsics }
      if (found.Attr and faDirectory = faDirectory) then
        if IncludeFullPath then Result.Append(aDirName + found.Name)
        else Result.Append(found.Name);
    end;
    finished:= FindNext(found); { fetch next item in dir }
  end; { while finished }
  FindClose(found); { release resources again }
end;

function TDirInfo.GetFiles(aDirName: string;IncludeFullPath: boolean): IStringList;
var finished: longint; found: TSearchRec;
begin
  Result:= CreStrings;
  if aDirName = '' then exit(Result);
  aDirName:= IncludeTrailingPathDelimiter(aDirName); { checks for (and) include(d) pathdelim }
  finished:= FindFirst(aDirName + '*',faAnyFile,found); { * and  faAnyFile works on linux }
  while finished = 0 do begin { the findxxx functions return 0 on success }
    if (found.Name[1] <> '.') then begin { subdue the pestering . & .. intrinsics }
      if (found.Attr and faDirectory <> faDirectory) then
        if IncludeFullPath then Result.Append(aDirName + found.Name)
        else Result.Append(found.Name);
    end;
    finished:= FindNext(found); { fetch next item in dir }
  end; { while finished }
  FindClose(found); { release resources again }
end;

{ service factory for IDirInfo :o) }
function GetDirInfo: IDirInfo;
begin { here we just typecast our const instance pointer to the right interface }
  Result:= IDirInfo(@DirInfo_Instance);
end; { ...and Bob's your uncle :o) }

end.

