{******************************************************************************
  MIT License

  Copyright (c) 2025 Hans van Buggenum

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
******************************************************************************}

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
