{ Copyright ©2025-2026 Hans van Buggenum }
{ AppLogging.pas - Advanced Logging Module
  This unit provides a thread-safe logging system with multiple log levels,
  file output, and configurable options.

  Key Features:
  - Multiple log levels (Info, Warning, Error, Debug)
  - Immediate or buffered writing
  - Automatic user-specific log files
  - Retry mechanism for file operations
  - Configurable log file appending

  Usage:
  1. Set LogFolder and FileName properties
  2. Configure AppendLogFile and ActivateLogging
  3. Call StartLogging to initialize
  4. Use WriteToLog* methods for logging
  5. Call StopLogging when finished

  Version 1.0.0.0 (24-05-2025, HvB).
}

unit AppLogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms;

type
{$Interfaces CORBA}

  { ILogging }
  ILogging = interface['{00DEC400-7DDF-4748-9AFB-5AE4D74A610B}']
    procedure StopLogging;
    procedure StartLogging;
    procedure WriteToLogInfo(const Comment: String);
    procedure WriteToLogWarning(const Comment: String);
    procedure WriteToLogError(const Comment: String);
    procedure WriteToLogDebug(const Comment: String);
  end;

{$Interfaces COM}

type
  { TLog_File
    Implements a file-based logging system with configurable options }
  TLog_File = class(TObject, ILogging)
  private
    FStringList: TStringList;        // Internal string list for log message buffering.
    FFileStream: TFileStream;        // File stream for log output.
    FLogFolder: String;              // Path to log file directory.
    FLogFileName: String;            // Base name of log file.
    FUserName: String;               // Current user name for log file naming.
    FAppendCurrentLogfile: Boolean;  // Flag to append to existing log file.
    FActivateLogging: Boolean;       // Flag to enable/disable logging.
    FCurrentTime: String;            // Buffer for current time string.
    FApplicationName: String;        // Name of the application for log headers.
    FAppVersion: String;             // Application version for log headers.
    FLock: TRTLCriticalSection;      // Critical section for thread safety.

    { Gets the append mode setting. }
    function GetAppendCurrentLogfile: Boolean;
    { Sets the append mode setting. }
    procedure SetAppendCurrentLogfile(AValue: Boolean);
    { Returns current date as string in DD-MM-YYYY format }
    function CurrentDate: String;
    { Writes buffered log entries to file. }
    procedure FlushLog;
    { Updates current time string. }
    procedure UpdateCurrentTime;
    { Basic log writing method. }
    procedure WriteToLog(const LogText: String);
    { Writes and immediately flushes to file. }
    procedure WriteToLogAndFlush(const LogText: String);
  public
    { Constructor - initializes logging system. }
    constructor Create;
    { Destructor - ensures proper cleanup. }
    destructor Destroy; override;

    { ILogging interface implementation }

    { Initializes logging and opens log file. }
    procedure StartLogging;
    { Stops logging and closes log file. }
    procedure StopLogging;
    { Writes an informational message to log buffer. }
    procedure WriteToLogInfo(const LogText: String);
    { Writes a warning message to log buffer. }
    procedure WriteToLogWarning(const LogText: String);
    { Writes an error message to log buffer. }
    procedure WriteToLogError(const LogText: String);
    { Writes a debug message to log buffer. }
    procedure WriteToLogDebug(const LogText: String);

    { Immediate write methods that flush to disk. }

    { Writes informational message and flushes to disk. }
    procedure WriteToLogAndFlushInfo(const LogText: String);
    { Writes warning message and flushes to disk. }
    procedure WriteToLogAndFlushWarning(const LogText: String);
    { Writes error message and flushes to disk. }
    procedure WriteToLogAndFlushError(const LogText: String);
    { Writes debug message and flushes to disk. }
    procedure WriteToLogAndFlushDebug(const LogText: String);

    { Properties. }
    { Name of the log file (without path). }
    property FileName: String read FLogFileName write FLogFileName;
    { Directory where log files are stored. }
    property LogFolder: String read FLogFolder write FLogFolder;
    { Determines whether to append to existing log file. }
    property AppendLogFile: Boolean read GetAppendCurrentLogfile write SetAppendCurrentLogfile;
    { Enables or disables logging globally. }
    property ActivateLogging: Boolean read FActivateLogging write FActivateLogging;
    { Name of the application for log headers. }
    property ApplicationName: String read FApplicationName write FApplicationName;
    { Version of the application for log headers. }
    property AppVersion: String read FAppVersion write FAppVersion;
  end;

var
  Logging: TLog_File; { Global logging instance. }

implementation

uses
  LazFileUtils;

{ TLog_File }
constructor TLog_File.Create;
begin
  InitCriticalSection(FLock);         // Initialize critical section for thread safety.
  FStringList := TStringList.Create;  // Create string list for log buffering.
  FUserName := StringReplace(GetEnvironmentVariable('USERNAME'), ' ', '_',
    [rfIgnoreCase, rfReplaceAll]);    // Get current username for log file naming.
end;

destructor TLog_File.Destroy;
begin
  EnterCriticalSection(FLock);  // Ensure thread-safe cleanup.
  try
    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);

    FreeAndNil(FStringList);
  finally
    LeaveCriticalSection(FLock);
    DoneCriticalSection(FLock);
  end;
  inherited Destroy;
end;

function TLog_File.GetAppendCurrentLogfile: Boolean;
begin
  Result := FAppendCurrentLogfile;
end;

procedure TLog_File.SetAppendCurrentLogfile(AValue: Boolean);
begin
  FAppendCurrentLogfile := AValue;
end;

procedure TLog_File.UpdateCurrentTime;
begin
  FCurrentTime := FormatDateTime('hh:mm:ss', Now) + ' --> | ';
end;

function TLog_File.CurrentDate: String;
var
  Present: TDateTime;
  Year, Month, Day: Word;
begin
  Present := Now;
  DecodeDate(Present, Year, Month, Day);
  Result := Format('%.2d-%.2d-%.4d', [Day, Month, Year]);
end;

procedure TLog_File.FlushLog;
const
  MAX_RETRIES = 10;
  RETRY_DELAY_MS = 50;
var
  RetryCount: Integer = 0;
  LogEntry: String;
  Success: Boolean = False;
begin
  if not FActivateLogging then Exit;

  EnterCriticalSection(FLock);
  try
    // Retry loop for file operations.
    while not Success and (RetryCount < MAX_RETRIES) do
    begin
      try
        // Write all buffered entries.
        for LogEntry in FStringList do
        begin
          FFileStream.Seek(0, soFromEnd);
          FFileStream.WriteBuffer(PChar(LogEntry + LineEnding)^,
            Length(LogEntry + LineEnding) * SizeOf(Char));
        end;
        FStringList.Clear;
        Success := True;
      except
        on E: EInOutError do
        begin
          Inc(RetryCount);
          Sleep(RETRY_DELAY_MS * RetryCount);

          if RetryCount >= MAX_RETRIES then
          begin
            WriteToLog('INFO | Failed to write to log file after ' +
              IntToStr(MAX_RETRIES) + ' attempts');
            Exit;
          end;
        end;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.StartLogging;
var
  FullPath: String;
begin
  if not FActivateLogging then Exit;

  EnterCriticalSection(FLock);
  try
    // Construct full log file path.
    FullPath := IncludeTrailingPathDelimiter(FLogFolder) +
      FUserName + '_' + FLogFileName;

    // Open or create log file based on append mode.
    if FAppendCurrentLogfile and FileExists(FullPath) then
      FFileStream := TFileStream.Create(FullPath, fmOpenReadWrite or fmShareDenyNone)
    else
      FFileStream := TFileStream.Create(FullPath, fmCreate or fmShareDenyNone);

    try
      // Write log header.
      FStringList.Add('#' + StringOfChar('#', 99));
      FStringList.Add(' Application: ' + FApplicationName);
      FStringList.Add(' Version    : ' + FAppVersion);
      FStringList.Add(' Date       : ' + CurrentDate);
      FStringList.Add('#' + StringOfChar('#', 99));
      FlushLog;
      UpdateCurrentTime;
    except
      on E: Exception do
        FStringList.Add('ERROR | Unexpected error during logging initialization: ' + E.Message);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.StopLogging;
begin
  if not Assigned(FStringList) then Exit;

  EnterCriticalSection(FLock);
  try
    // Write log footer.
    FStringList.Add('');
    FStringList.Add('#' + StringOfChar('#', 99));
    FStringList.Add(' Application ' + FApplicationName + ' terminated');
    FStringList.Add('#' + StringOfChar('#', 99));
    FStringList.Add('');
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ Common logging methods with thread protection. }
procedure TLog_File.WriteToLog(const LogText: String);
begin
  if not FActivateLogging then Exit;

  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' :              | ' + LogText);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogAndFlush(const LogText: String);
begin
  if not FActivateLogging then Exit;

  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' :              | ' + LogText);
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ Log level specific methods. }
procedure TLog_File.WriteToLogInfo(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : INFO         | ' + LogText);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogWarning(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : WARNING      | ' + LogText);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogError(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : ERROR        | ' + LogText);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogDebug(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : DEBUG        | ' + LogText);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ Immediate flush methods. }
procedure TLog_File.WriteToLogAndFlushInfo(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : INFO         | ' + LogText);
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogAndFlushWarning(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : WARNING      | ' + LogText);
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogAndFlushError(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : ERROR        | ' + LogText);
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLog_File.WriteToLogAndFlushDebug(const LogText: String);
begin
  EnterCriticalSection(FLock);
  try
    UpdateCurrentTime;
    FStringList.Add(FCurrentTime + ' : DEBUG        | ' + LogText);
    FlushLog;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ Unit finalization. }
procedure FinalizeLogging;
begin
  if Assigned(Logging) then
    FreeAndNil(Logging);
end;

initialization
  Logging := TLog_File.Create;  // Create global logging instance .

finalization
  FinalizeLogging;  // Ensure proper cleanup.

end.
