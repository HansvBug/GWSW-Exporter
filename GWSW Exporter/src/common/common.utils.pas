{ Copyright ©2025 Hans van Buggenum }
unit common.utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms {$IFDEF MSWINDOWS}, Windows {$ENDIF};

function CheckFormIsEntireVisible(Rect : TRect) : TRect;
function SetStatusbarPnlWidth(stbWidth, lpWidth, rpWidth: Integer): Integer;
function IsFileInUse(FileName: TFileName): Boolean;

implementation


function CheckFormIsEntireVisible(Rect : TRect) : TRect;
var
  aWidth: Integer;
  aHeight: Integer;
begin
  Result:= Rect;
  aWidth:= Rect.Right - Rect.Left;
  aHeight:= Rect.Bottom - Rect.Top;

  if Result.Left < (Screen.DesktopLeft) then begin
    Result.Left:= Screen.DesktopLeft;
    Result.Right:= Screen.DesktopLeft + aWidth;
  end;
  if Result.Right > (Screen.DesktopLeft + Screen.DesktopWidth) then begin
    Result.Left:= Screen.DesktopLeft + Screen.DesktopWidth - aWidth;
    Result.Right:= Screen.DesktopLeft + Screen.DesktopWidth;
  end;
  if Result.Top < Screen.DesktopTop then begin
    Result.Top:= Screen.DesktopTop;
    Result.Bottom:= Screen.DesktopTop + aHeight;
  end;
  if Result.Bottom > (Screen.DesktopTop + Screen.DesktopHeight) then begin
    Result.Top:= Screen.DesktopTop + Screen.DesktopHeight - aHeight;
    Result.Bottom:= Screen.DesktopTop + Screen.DesktopHeight;
  end;
end;

function SetStatusbarPnlWidth(stbWidth, lpWidth, rpWidth : Integer) : Integer;
begin
  Result:= stbWidth - lpWidth - rpWidth;
end;

function IsFileInUse(FileName : TFileName) : Boolean;
{$IFDEF MSWINDOWS}
var
  HFileRes: HFILE;
begin
  Result:= False;
  if not FileExists(FileName) then Exit;
  HFileRes := CreateFile(PChar(FileName),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         nil,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0);
  Result:= (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;
{$ENDIF}
{$IFDEF LINUX}
{ #note : Dit werkt ook voor Windows. }
var
  FS: TFileStream;
begin
  Result:= False;
  if not FileExists(FileName) then Exit;

  try
    FS:= TFileStream.Create(
      FileName,
      fmOpenReadWrite or fmShareExclusive
    );
    FS.Free;
  except
    Result := True;
  end;
end;
{$ENDIF}

end.

