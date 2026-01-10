{ Copyright ©2025-2026 Hans van Buggenum }
unit exportprogressreporter.intf;

{$mode ObjFPC}{$H+}
{$interfaces corba}

interface

uses common.consts;

type
  IExportProgressReporter = interface
    ['{4E3FEE97-3E03-48C0-9C39-8965C19CB9C6}']
    procedure ReportProgressMsg(const Message: string);
    procedure ReportError(const ErrMsg: string; const ErrorType: Integer = eetInformation;const Guid: string = '');
    procedure ReportProgressCount(const Current, Total: Integer);
  end;

implementation

end.
