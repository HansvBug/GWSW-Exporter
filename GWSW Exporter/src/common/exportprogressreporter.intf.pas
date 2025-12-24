{ Copyright ©2025 Hans van Buggenum }
unit exportprogressreporter.intf;

{$mode ObjFPC}{$H+}
{$interfaces corba}

interface

type
  IExportProgressReporter = interface
    ['{4E3FEE97-3E03-48C0-9C39-8965C19CB9C6}']
    procedure ReportProgress(const Message: string);
    procedure ReportError(const ErrorMessage: string);
  end;

implementation

end.
