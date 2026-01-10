unit GWSWValidationConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGWSWConfig = record
    MinPutWidth,
    MaxPutWidth,
    MinPutLength,
    MaxPutLength,
    MinPutHeight,
    MaxPutHeight,
    MinPutDiameter,
    MaxPutDiameter: Integer;
    MinLeidingWidth,
    MaxLeidingWidth,
    MinLeidingLength,
    MaxLeidingLength,
    MinLeidingHeight,
    MaxLeidingHeight,
    MinLeidingDiameter,
    MaxLeidingDiameter: Integer;
    MinPersleidingWidth,
    MaxPersleidingWidth,
    MinPersleidingLength,
    MaxPersleidingLength,
    MinPersleidingHeight,
    MaxPersleidingHeight,
    MinPersleidingDiameter,
    MaxPersleidingDiameter: Integer;
    MinKolkWidth,
    MaxKolkWidth,
    MinKolkLength,
    MaxKolkLength,
    MinKolkHeight,
    MaxKolkHeight,
    MinKolkDiameter,
    MaxKolkDiameter,
    MinZCoord,
    MaxZCoord: Integer;
  end;

procedure LoadGWSWConfig(const Section, Version: string);
function GetGWSWConfig: TGWSWConfig;

implementation
uses common.consts, IniFiles;

var
  fGWSWConfig: TGWSWConfig;

procedure LoadGWSWConfig(const Section, Version : string);
var
  Ini: TIniFile;
  lVersion: String;
begin
  lVersion:= StringReplace(Version, '.', '', [rfReplaceAll]);
  Ini:= TIniFile.Create(GetEnvironmentVariable('appdata') + PathDelim + ApplicationName+PathDelim+ adSettings +PathDelim+ 'GWSW_' + lVersion + '.cfg');

  try
    // Reading with default
    // Put
    fGWSWConfig.MinPutWidth:= Ini.ReadInteger(Section, 'MinPutWidth', 300);
    fGWSWConfig.MaxPutWidth:= Ini.ReadInteger(Section, 'MaxPutWidth', 4000);
    fGWSWConfig.MinPutLength:= Ini.ReadInteger(Section, 'MinPutLength', 300);
    fGWSWConfig.MaxPutLength:= Ini.ReadInteger(Section, 'MaxPutLength', 4000);
    fGWSWConfig.MinPutHeight:= Ini.ReadInteger(Section, 'MinPutHeight', 500);
    fGWSWConfig.MaxPutHeight:= Ini.ReadInteger(Section, 'MaxPutHeight', 4000);
    fGWSWConfig.MinPutDiameter:= Ini.ReadInteger(Section, 'MinPutDiameter', 300);
    fGWSWConfig.MaxPutDiameter:= Ini.ReadInteger(Section, 'MaxPutDiameter', 4000);
    // Leiding (Streng)
    fGWSWConfig.MinLeidingWidth:= Ini.ReadInteger(Section, 'MinLeidingWidth', 63);                 // [mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxLeidingWidth:= Ini.ReadInteger(Section, 'MaxLeidingWidth', 4000);
    fGWSWConfig.MinLeidingLength:= Ini.ReadInteger(Section, 'MinLeidingLength', 1);                // [m] xsd:decimal: min=1 max=75 ??? validatie website geeft: Kenmerk LengteLeiding (transport) - waarde wijkt af (min=1,max=1000)
    fGWSWConfig.MaxLeidingLength:= Ini.ReadInteger(Section, 'MaxLeidingLength', 75);
    fGWSWConfig.MinLeidingHeight:= Ini.ReadInteger(Section, 'MinLeidingHeight', 63);               // [mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxLeidingHeight:= Ini.ReadInteger(Section, 'MaxLeidingHeight', 4000);
    fGWSWConfig.MinLeidingDiameter:= Ini.ReadInteger(Section, 'MinLeidingDiameter', 63);           // [mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxLeidingDiameter:= Ini.ReadInteger(Section, 'MaxLeidingDiameter', 4000);
    // Persleiding
    fGWSWConfig.MinPersleidingWidth:= Ini.ReadInteger(Section, 'MinPersleidingWidth', 63);         // [mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxPersleidingWidth:= Ini.ReadInteger(Section, 'MaxPersleidingWidth', 4000);
    fGWSWConfig.MinPersleidingLength:= Ini.ReadInteger(Section, 'MinPersleidingLength', 1);        // [m] xsd:decimal: min=1 max=75
    fGWSWConfig.MaxPersleidingLength:= Ini.ReadInteger(Section, 'MaxPersleidingLength', 1000);  { #note : Deze is vreemd. De validatie op de gwsw server geeft pas eenmelding als de streng > 1000 m }
    fGWSWConfig.MinPersleidingHeight:= Ini.ReadInteger(Section, 'MinPersleidingHeight', 63);       // 	[mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxPersleidingHeight:= Ini.ReadInteger(Section, 'MaxPersleidingHeight', 4000);
    fGWSWConfig.MinPersleidingDiameter:= Ini.ReadInteger(Section, 'MinPersleidingDiameter', 63);   // [mm] xsd:integer: min=63 max=4000
    fGWSWConfig.MaxPersleidingDiameter:= Ini.ReadInteger(Section, 'MaxPersleidingDiameter', 4000);
    //Kolk
    fGWSWConfig.MinKolkWidth:= Ini.ReadInteger(Section, 'MinKolkWidth', 300);         // [mm] xsd:integer: min=300 max=4000
    fGWSWConfig.MaxKolkWidth:= Ini.ReadInteger(Section, 'MaxKolkWidth', 4000);
    fGWSWConfig.MinKolkLength:= Ini.ReadInteger(Section, 'MinKolkLength', 300);       // [mm] xsd:integer: min=300 max=4000
    fGWSWConfig.MaxKolkLength:= Ini.ReadInteger(Section, 'MaxKolkLength', 4000);
    fGWSWConfig.MinKolkHeight:= Ini.ReadInteger(Section, 'MinKolkHeight', 500);       // [mm] xsd:integer: min=500 max=4000
    fGWSWConfig.MaxKolkHeight:= Ini.ReadInteger(Section, 'MaxKolkHeight', 4000);
    fGWSWConfig.MinKolkDiameter:= Ini.ReadInteger(Section, 'MinKolkDiameter', 300);   // [mm] xsd:integer: min=300 max=4000
    fGWSWConfig.MaxKolkDiameter:= Ini.ReadInteger(Section, 'MaxKolkDiameter', 4000);

    // Hoogtes
    fGWSWConfig.MinZCoord:= Ini.ReadInteger(Section, 'MinZCoord', -20);
    fGWSWConfig.MaxZCoord:= Ini.ReadInteger(Section, 'MaxZCoord', 300);


    // Write defaults back if file didn't exist yet
    //Put
    Ini.WriteInteger(Section, 'MinPutWidth', fGWSWConfig.MinPutWidth);
    Ini.WriteInteger(Section, 'MaxPutWidth', fGWSWConfig.MaxPutWidth);
    Ini.WriteInteger(Section, 'MinPutLength', fGWSWConfig.MinPutLength);
    Ini.WriteInteger(Section, 'MaxPutLength', fGWSWConfig.MaxPutLength);
    Ini.WriteInteger(Section, 'MinPutHeight', fGWSWConfig.MinPutHeight);
    Ini.WriteInteger(Section, 'MaxPutHeight', fGWSWConfig.MaxPutHeight);
    Ini.WriteInteger(Section, 'MinPutDiameter', fGWSWConfig.MinPutDiameter);
    Ini.WriteInteger(Section, 'MaxPutDiameter', fGWSWConfig.MaxPutDiameter);
    // Leiding
    Ini.WriteInteger(Section, 'MinLeidingWidth', fGWSWConfig.MinLeidingWidth);
    Ini.WriteInteger(Section, 'MaxLeidingWidth', fGWSWConfig.MaxLeidingWidth);
    Ini.WriteInteger(Section, 'MinLeidingLength', fGWSWConfig.MinLeidingLength);
    Ini.WriteInteger(Section, 'MaxLeidingLength', fGWSWConfig.MaxLeidingLength);
    Ini.WriteInteger(Section, 'MinLeidingHeight', fGWSWConfig.MinLeidingHeight);
    Ini.WriteInteger(Section, 'MaxLeidingHeight', fGWSWConfig.MaxLeidingHeight);
    Ini.WriteInteger(Section, 'MinLeidingDiameter', fGWSWConfig.MinLeidingDiameter);
    Ini.WriteInteger(Section, 'MaxLeidingDiameter', fGWSWConfig.MaxLeidingDiameter);

    Ini.WriteInteger(Section, 'MinPersleidingWidth', fGWSWConfig.MinPersleidingWidth);
    Ini.WriteInteger(Section, 'MaxPersleidingWidth', fGWSWConfig.MaxPersleidingWidth);
    Ini.WriteInteger(Section, 'MinPersleidingLength', fGWSWConfig.MinPersleidingLength);
    Ini.WriteInteger(Section, 'MaxPersleidingLength', fGWSWConfig.MaxPersleidingLength);
    Ini.WriteInteger(Section, 'MinPersleidingHeight', fGWSWConfig.MinPersleidingHeight);
    Ini.WriteInteger(Section, 'MaxPersleidingHeight', fGWSWConfig.MaxPersleidingHeight);
    Ini.WriteInteger(Section, 'MinPersleidingDiameter', fGWSWConfig.MinPersleidingDiameter);
    Ini.WriteInteger(Section, 'MaxPersleidingDiameter', fGWSWConfig.MaxPersleidingDiameter);

    // Kolk
    Ini.WriteInteger(Section, 'MinKolkWidth', fGWSWConfig.MinKolkWidth);
    Ini.WriteInteger(Section, 'MaxKolkWidth', fGWSWConfig.MaxKolkWidth);
    Ini.WriteInteger(Section, 'MinKolkLength', fGWSWConfig.MinKolkLength);
    Ini.WriteInteger(Section, 'MaxKolkLength', fGWSWConfig.MaxKolkLength);
    Ini.WriteInteger(Section, 'MinKolkHeight', fGWSWConfig.MinKolkHeight);
    Ini.WriteInteger(Section, 'MaxKolkHeight', fGWSWConfig.MaxKolkHeight);
    Ini.WriteInteger(Section, 'MinKolkDiameter', fGWSWConfig.MinKolkDiameter);
    Ini.WriteInteger(Section, 'MaxKolkDiameter', fGWSWConfig.MaxKolkDiameter);

    // Hoogtes
    Ini.WriteInteger(Section, 'MinZCoord', fGWSWConfig.MinZCoord);
    Ini.WriteInteger(Section, 'MazZCoord', fGWSWConfig.MaxZCoord);


  finally
    Ini.Free;
  end;
end;

function GetGWSWConfig : TGWSWConfig;
begin
  Result:= fGWSWConfig;
end;

end.

