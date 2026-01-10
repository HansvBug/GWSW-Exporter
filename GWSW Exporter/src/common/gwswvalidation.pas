{ Copyright ©2025-2026 Hans van Buggenum }
unit GWSWValidation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GWSWTypes, GWSWValidationConfig;

type
  TGWSWValidationResult = record
    IsValid: Boolean;
    ErrorMsg: string;
  end;

  procedure InitValidationConfig;
  function ValidatePutWidth(const Put: PGWSWPut): TGWSWValidationResult;
  function ValidatePutLength(const Put: PGWSWPut): TGWSWValidationResult;
  function ValidatePutHeight(const Put: PGWSWPut): TGWSWValidationResult;
  function ValidatePutDiameter(const Put: PGWSWPut): TGWSWValidationResult;

  function ValidateLeidingWidth(const Leiding: PGWSWLeiding): TGWSWValidationResult;
  function ValidateLeidingLength(const Leiding: PGWSWLeiding): TGWSWValidationResult;
  function ValidateLeidingHeight(const Leiding: PGWSWLeiding): TGWSWValidationResult;
  function ValidateLeidingDiameter(const Leiding: PGWSWLeiding): TGWSWValidationResult;

  function ValidatePersleidingWidth(const Persleiding: PGWSWPersleiding): TGWSWValidationResult;
  function ValidatePersleidingLength(const Persleiding: PGWSWPersleiding): TGWSWValidationResult;
  function ValidatePersleidingHeight(const Persleiding: PGWSWPersleiding): TGWSWValidationResult;
  function ValidatePersleidingDiameter(const Persleiding: PGWSWPersleiding): TGWSWValidationResult;

  function ValidateKolkWidth(const Kolk: PGWSWKolk): TGWSWValidationResult;
  function ValidateKolkLength(const Kolk: PGWSWKolk): TGWSWValidationResult;
  function ValidateKolkHeight(const Kolk: PGWSWKolk): TGWSWValidationResult;
  function ValidateKolkDiameter(const Kolk: PGWSWKolk): TGWSWValidationResult;

  function ValidatePutZCoord(const Put: PGWSWPut): TGWSWValidationResult;

implementation

var
  Cfg: TGWSWConfig;

procedure InitValidationConfig;
begin
  Cfg:= GetGWSWConfig;  // Retrieve the previously read validation values.
end;

{$Region Put}
function ValidatePutWidth(const Put: PGWSWPut): TGWSWValidationResult;
begin
  Result.IsValid:=
    (Put^.Breedte >= Cfg.MinPutWidth) and
    (Put^.Breedte <= Cfg.MaxPutWidth);

  if not Result.IsValid then
    Result.ErrorMsg:= 'ManholeWidth' + '|' + Cfg.MinPutWidth.ToString + '|' + 'And' + '|' + Cfg.MaxPutWidth.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Put^.Breedte.ToString+ '|' + '. (' + Put^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePutLength(const Put : PGWSWPut) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Put^.Lengte >= Cfg.MinPutLength) and
    (Put^.Lengte <= Cfg.MaxPutLength);

  if not Result.IsValid then
    Result.ErrorMsg:= 'ManholeLength' + '|' + Cfg.MinPutLength.ToString + '|' + 'And' + '|' + Cfg.MaxPutLength.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Put^.Lengte.ToString+ '|' + '. (' + Put^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePutHeight(const Put : PGWSWPut) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Put^.Hoogte >= Cfg.MinPutHeight) and
    (Put^.Hoogte <= Cfg.MaxPutHeight);

  if not Result.IsValid then
    Result.ErrorMsg:= 'ManholeHeight' + '|' + Cfg.MinPutHeight.ToString + '|' + 'And' + '|' + Cfg.MaxPutHeight.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Put^.Hoogte.ToString+ '|' + '. (' + Put^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePutDiameter(const Put : PGWSWPut) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Put^.Diameter >= Cfg.MinPutDiameter) and
    (Put^.Diameter <= Cfg.MaxPutDiameter);

  if not Result.IsValid then
    Result.ErrorMsg:= 'ManholeDiameter' + '|' + Cfg.MinPutDiameter.ToString + '|' + 'And' + '|' + Cfg.MaxPutDiameter.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Put^.Diameter.ToString+ '|' + '. (' + Put^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;
{$EndRegion Put}

{$Region Leiding}
function ValidateLeidingWidth(const Leiding : PGWSWLeiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Leiding^.Breedte >= Cfg.MinLeidingWidth) and
    (Leiding^.Breedte <= Cfg.MaxLeidingWidth);

  if not Result.IsValid then
    Result.ErrorMsg:= 'PipelineWidth' + '|' + Cfg.MinLeidingWidth.ToString + '|' + 'And' + '|' + Cfg.MaxLeidingWidth.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Leiding^.Breedte.ToString+ '|' + '. (' + Leiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidateLeidingLength(const Leiding : PGWSWLeiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Leiding^.Lengte >= Cfg.MinLeidingLength) and
    (Leiding^.Lengte <= Cfg.MaxLeidingLength);

  if not Result.IsValid then
    Result.ErrorMsg:= 'PipelineLength' + '|' + Cfg.MinLeidingLength.ToString + '|' + 'And' + '|' + Cfg.MaxLeidingLength.ToString + '|' + 'Meters' + '|' +
    'Found' + '|' + Leiding^.Lengte.ToString+ '|' + '. (' + Leiding^.GUID + ')'

  else
    Result.ErrorMsg:= '';
end;

function ValidateLeidingHeight(const Leiding : PGWSWLeiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Leiding^.Hoogte >= Cfg.MinLeidingHeight) and
    (Leiding^.Hoogte <= Cfg.MaxLeidingHeight);

  if not Result.IsValid then
    Result.ErrorMsg:= 'PipelineHeight' + '|' + Cfg.MinLeidingHeight.ToString + '|' + 'And' + '|' + Cfg.MaxLeidingHeight.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Leiding^.Hoogte.ToString+ '|' + '. (' + Leiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidateLeidingDiameter(const Leiding : PGWSWLeiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Leiding^.Diameter >= Cfg.MinLeidingDiameter) and
    (Leiding^.Diameter <= Cfg.MaxLeidingDiameter);

  if not Result.IsValid then
    Result.ErrorMsg:= 'PipelineDiameter' + '|' + Cfg.MinLeidingDiameter.ToString + '|' + 'And' + '|' + Cfg.MaxLeidingDiameter.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Leiding^.Diameter.ToString+ '|' + '. (' + Leiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;
{$EndRegion Leiding}

{$Region Perseiding}
function ValidatePersleidingWidth(const Persleiding : PGWSWPersleiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Persleiding^.Breedte >= Cfg.MinPersleidingWidth) and
    (Persleiding^.Breedte <= Cfg.MaxPersleidingWidth);

  if not Result.IsValid then
    Result.ErrorMsg:= 'MechanicPipelineWidth' + '|' + Cfg.MinPersleidingWidth.ToString + '|' + 'And' + '|' + Cfg.MaxPersleidingWidth.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Persleiding^.Breedte.ToString+ '|' + '. (' + Persleiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePersleidingLength(const Persleiding : PGWSWPersleiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Persleiding^.Lengte >= Cfg.MinPersleidingLength) and
    (Persleiding^.Lengte <= Cfg.MaxPersleidingLength);

  if not Result.IsValid then
    Result.ErrorMsg:= 'MechanicPipelineLength' + '|' + Cfg.MinPersleidingLength.ToString + '|' + 'And' + '|' + Cfg.MaxPersleidingLength.ToString + '|' + 'Meters' + '|' +
    'Found' + '|' + Persleiding^.Lengte.ToString+ '|' + '. (' + Persleiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePersleidingHeight(const Persleiding : PGWSWPersleiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Persleiding^.Hoogte >= Cfg.MinPersleidingHeight) and
    (Persleiding^.Hoogte <= Cfg.MaxPersleidingHeight);

  if not Result.IsValid then
    Result.ErrorMsg:= 'MechanicPipelineHeight' + '|' + Cfg.MinPersleidingHeight.ToString + '|' + 'And' + '|' + Cfg.MaxPersleidingHeight.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Persleiding^.Hoogte.ToString+ '|' + '. (' + Persleiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePersleidingDiameter(const Persleiding : PGWSWPersleiding) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Persleiding^.Diameter >= Cfg.MinPersleidingDiameter) and
    (Persleiding^.Diameter <= Cfg.MaxPersleidingDiameter);

  if not Result.IsValid then
    Result.ErrorMsg:= 'MechanicPipelineDiameter' + '|' + Cfg.MinPersleidingDiameter.ToString + '|' + 'And' + '|' + Cfg.MaxPersleidingDiameter.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Persleiding^.Diameter.ToString+ '|' + '. (' + Persleiding^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;
{$EndRegion Perseiding}

{$Region Kolk}
function ValidateKolkWidth(const Kolk : PGWSWKolk) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Kolk^.Breedte >= Cfg.MinKolkWidth) and
    (Kolk^.Breedte <= Cfg.MaxKolkWidth);

  if not Result.IsValid then
    Result.ErrorMsg:= 'GullyWidth' + '|' + Cfg.MinKolkWidth.ToString + '|' + 'And' + '|' + Cfg.MaxKolkWidth.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Kolk^.Breedte.ToString+ '|' + '. (' + Kolk^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidateKolkLength(const Kolk : PGWSWKolk) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Kolk^.Lengte >= Cfg.MinKolkLength) and
    (Kolk^.Lengte <= Cfg.MaxKolkLength);

  if not Result.IsValid then
    Result.ErrorMsg:= 'GullyLength' + '|' + Cfg.MinKolkLength.ToString + '|' + 'And' + '|' + Cfg.MaxKolkLength.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Kolk^.Lengte.ToString+ '|' + '. (' + Kolk^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidateKolkHeight(const Kolk : PGWSWKolk) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Kolk^.Hoogte >= Cfg.MinKolkHeight) and
    (Kolk^.Hoogte <= Cfg.MaxKolkHeight);

  if not Result.IsValid then
    Result.ErrorMsg:= 'GullyHeight' + '|' + Cfg.MinKolkHeight.ToString + '|' + 'And' + '|' + Cfg.MaxKolkHeight.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Kolk^.Hoogte.ToString+ '|' + '. (' + Kolk^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidateKolkDiameter(const Kolk : PGWSWKolk) : TGWSWValidationResult;
begin
  Result.IsValid:=
    (Kolk^.Diameter >= Cfg.MinPersleidingDiameter) and
    (Kolk^.Diameter <= Cfg.MaxPersleidingDiameter);

  if not Result.IsValid then
    Result.ErrorMsg:= 'GullyDiameter' + '|' + Cfg.MinKolkDiameter.ToString + '|' + 'And' + '|' + Cfg.MaxKolkDiameter.ToString + '|' + 'Millimeters' + '|' +
    'Found' + '|' + Kolk^.Diameter.ToString+ '|' + '. (' + Kolk^.GUID + ')'
  else
    Result.ErrorMsg:= '';
end;

function ValidatePutZCoord(const Put: PGWSWPut): TGWSWValidationResult;
begin
  Result.IsValid:=
    (Put^.Maaiveldhoogte >= Cfg.MinZCoord) and
    (Put^.Maaiveldhoogte <= Cfg.MaxZCoord);

 if not Result.IsValid then
   Result.ErrorMsg:= 'Put: Maaiveldhoogte moet tussen ' + cfg.MinZCoord.ToString + ' en ' + cfg.MaxZCoord.ToString + ' liggen. gevonden: ' + put^.Maaiveldhoogte.ToString + ' m.';
end;

{$EndRegion Kolk}


end.

