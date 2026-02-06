{ Copyright ©2025-2026 Hans van Buggenum }
unit common.utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, ExtCtrls
  {$IFDEF WINDOWS}, Windows {$ENDIF};

function CheckFormIsEntireVisible(Rect : TRect) : TRect;
function SetStatusbarPnlWidth(stbWidth, lpWidth, rpWidth: Integer): Integer;
function IsFileInUse(FileName: TFileName): Boolean;
procedure DisableChildControls(Parent: TObject);
procedure EnableChildControls(Parent: TObject);

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
{$IFDEF WINDOWS}
var
  HFileRes: HFILE;
begin
  Result:= False;
  if not FileExists(FileName) then Exit;
  HFileRes:= CreateFile(PChar(FileName),
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
{$ELSE}
var
  FileHandle: THandle;
begin
  Result:= False;
  if not FileExists(FileName) then
    Exit;

  // Probeer het bestand exclusief te openen voor lezen/schrijven
  FileHandle:= FileOpen(FileName, fmOpenReadWrite or fmShareExclusive);

  if FileHandle = THandle(-1) then
  begin
    // Bestand is in gebruik of kan niet worden geopend
    Result:= True;
  end
  else
  begin
    // Bestand is niet in gebruik, sluit het handle
    FileClose(FileHandle);
    Result:= False;
  end;
end;
{$ENDIF}

procedure DisableChildControls(Parent : TObject);
var
  i: Integer;
  lParent: TWinControl;
begin
  if not (Parent is TWinControl) then
    Exit;

  lParent:= TWinControl(Parent);

  for i:= 0 to lParent.ControlCount - 1 do
  begin
    if lParent.Controls[i] is TButton then
      lParent.Controls[i].Enabled:= False;

    if lParent.Controls[i] is TSplitter then
      lParent.Controls[i].Enabled:= False;

    // If it's a container, continue recursively
    if lParent.Controls[i] is TWinControl then
      DisableChildControls(lParent.Controls[i]);
  end;
end;

procedure EnableChildControls(Parent : TObject);
var
  i: Integer;
  lParent: TWinControl;
begin
  if not (Parent is TWinControl) then
    Exit;

  lParent:= TWinControl(Parent);

  for i:= 0 to lParent.ControlCount - 1 do
  begin
    if lParent.Controls[i] is TButton then
      lParent.Controls[i].Enabled:= True;

    if lParent.Controls[i] is TSplitter then
      lParent.Controls[i].Enabled:= True;

    // // If it's a container, continue recursively
    if lParent.Controls[i] is TWinControl then
      EnableChildControls(lParent.Controls[i]);
  end;
end;

end.
