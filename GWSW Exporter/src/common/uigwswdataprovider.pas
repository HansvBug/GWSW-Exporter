unit uIGWSWDataProvider;

interface

uses
  SysUtils, Classes;

type
  IGWSWDataProvider = interface
    ['{2154A8B3-4D3E-4A47-B87A-7F8E1F3C6A9D}']

    // Algemene methoden
    procedure Open;
    procedure Close;
    function First: Boolean;
    function Next: Boolean;
    function EOF: Boolean;

    // Veld toegang methods
    function GetFieldValue(const FieldName: string): Variant;
    function FieldExists(const FieldName: string): Boolean;
    function GetObjectType: string;

    // Metadata
    function GetRecordCount: Integer;
    function GetProviderType: string;
  end;

implementation

end.
