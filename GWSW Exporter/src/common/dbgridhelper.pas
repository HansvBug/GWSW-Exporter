{ Copyright ©2025-2026 Hans van Buggenum }
unit DbGridHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, SQLDB, Contnrs, BufDataset;

type
  TIndexInfo = class
    IndexName: string;
    LastUsed: TDateTime;
    FieldName: string;
    IsAscending: Boolean;
  end;

  { TDbGridHelper }
  TDbGridHelper = class(TObject)
  private
    fLastColumn: TColumn;
    fIndexList: TObjectList; // Bevat TIndexInfo objecten
    procedure UpdateIndexes(Query: TSQLQuery);
    function IndexExists(Query: TSQLQuery; const IndexName: string): Boolean;
    function GetIndexInfo(const IndexName: string): TIndexInfo;
    procedure UpdateIndexUsage(const IndexName: string; FieldName: string; IsAscending: Boolean);
    function FindOldestIndex: TIndexInfo;
    procedure RemoveOldestIndex(Query: TSQLQuery);

    procedure RemoveOldestBufIndex(BufDS: TBufDataset);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortOracleDbGrid(DataProvider, Column: TObject);

    procedure SortCSVDataSet(ADataset: TObject; AColumn: TObject);
  end;

implementation

{ TDbGridHelper }

procedure TDbGridHelper.UpdateIndexes(Query: TSQLQuery);
begin
  if not Assigned(Query) then Exit;
  Query.IndexDefs.Updated:= false;
  Query.IndexDefs.Update;
end;

function TDbGridHelper.IndexExists(Query: TSQLQuery; const IndexName: string): Boolean;
begin
  if not Assigned(Query) then Exit(False);
  UpdateIndexes(Query);
  Result:= Query.IndexDefs.IndexOf(IndexName) <> -1;
end;

function TDbGridHelper.GetIndexInfo(const IndexName: string): TIndexInfo;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to fIndexList.Count - 1 do begin
    if TIndexInfo(fIndexList[i]).IndexName = IndexName then begin
      Result:= TIndexInfo(fIndexList[i]);
      Exit;
    end;
  end;
end;

procedure TDbGridHelper.UpdateIndexUsage(const IndexName: string; FieldName: string; IsAscending: Boolean);
var
  Info: TIndexInfo;
begin
  Info:= GetIndexInfo(IndexName);
  if not Assigned(Info) then begin
    Info:= TIndexInfo.Create;
    Info.IndexName:= IndexName;
    Info.FieldName:= FieldName;
    Info.IsAscending:= IsAscending;
    fIndexList.Add(Info);
  end;
  Info.LastUsed:= Now;
end;

function TDbGridHelper.FindOldestIndex: TIndexInfo;
var
  i: Integer;
  Oldest: TIndexInfo;
begin
  Result:= nil;
  if fIndexList.Count = 0 then Exit;

  Oldest:= TIndexInfo(fIndexList[0]);
  for i:= 1 to fIndexList.Count - 1 do begin
    if TIndexInfo(fIndexList[i]).LastUsed < Oldest.LastUsed then
      Oldest:= TIndexInfo(fIndexList[i]);
  end;
  Result:= Oldest;
end;

procedure TDbGridHelper.RemoveOldestIndex(Query: TSQLQuery);
var
  Oldest: TIndexInfo;
begin
  if fIndexList.Count < 98 then Exit; // Laat 2 plaatsen vrij

  Oldest:= FindOldestIndex;
  if not Assigned(Oldest) then Exit;

  try
    // Voorkom dat er teveel indexen worden aangemaakt.
    fIndexList.Remove(Oldest);
  except
    // Negeer fouten ...
  end;
end;


procedure TDbGridHelper.RemoveOldestBufIndex(BufDS: TBufDataset);
var
  Oldest: TIndexInfo;
  Idx: Integer;
begin
  if fIndexList.Count = 0 then Exit;

  Oldest := FindOldestIndex;
  if not Assigned(Oldest) then Exit;

  Idx := BufDS.IndexDefs.IndexOf(Oldest.IndexName);
  if Idx <> -1 then
  begin
    // Laat actieve index eerst los
    if BufDS.IndexName = Oldest.IndexName then
      BufDS.IndexName := '';

    BufDS.IndexDefs.Delete(Idx);
  end;

  fIndexList.Remove(Oldest);
end;


constructor TDbGridHelper.Create;
begin
  inherited Create;
  fLastColumn:= nil;
  fIndexList:= TObjectList.Create(True); // True = eigenaar van objecten
end;

destructor TDbGridHelper.Destroy;
begin
  if Assigned(fIndexList) then
    FreeAndNil(fIndexList);
  inherited Destroy;
end;

procedure TDbGridHelper.SortOracleDbGrid(DataProvider, Column: TObject);
const
  ImageArrowUp = 0;
  ImageArrowDown = 1;
  MAX_INDEXES = 100;
var
  lColumn: TColumn;
  lQuery: TSQLQuery;
  ASC_IndexName, DESC_IndexName: string;
  CurrentSortAscending: Boolean;
begin
  if not Assigned(DataProvider) or not Assigned(Column) then
    Exit;

  lColumn:= TColumn(Column);
  lQuery:= TSQLQuery(DataProvider);

  if not Assigned(lQuery) or not Assigned(lColumn.Field) then
    Exit;

  if (lColumn.Field.DataType in [ftBLOB, ftMemo, ftWideMemo]) then
    Exit;

  ASC_IndexName:= 'ASC_' + lColumn.FieldName;
  DESC_IndexName:= 'DESC_' + lColumn.FieldName;

  CurrentSortAscending:= not Boolean(lColumn.tag);

  try
    // Controleer of we de index limiet naderen
    if fIndexList.Count >= MAX_INDEXES - 2 then begin
      RemoveOldestIndex(lQuery);
    end;

    // Controleer en maak ASC index aan indien nodig
    if not IndexExists(lQuery, ASC_IndexName) then begin
      // Nog meer controle: kijk of IndexDefs niet al vol is
      UpdateIndexes(lQuery);
      if lQuery.IndexDefs.Count >= MAX_INDEXES - 2 then begin
        // Te veel indexen - kan er geen meer aanmaken
        Exit;
      end;

      lQuery.AddIndex(ASC_IndexName, lColumn.FieldName, []);
      UpdateIndexes(lQuery);
      UpdateIndexUsage(ASC_IndexName, lColumn.FieldName, True);
    end
    else begin
      UpdateIndexUsage(ASC_IndexName, lColumn.FieldName, True);
    end;

    // Controleer en maak DESC index aan indien nodig
    if not IndexExists(lQuery, DESC_IndexName) then begin
      UpdateIndexes(lQuery);
      if lQuery.IndexDefs.Count >= MAX_INDEXES - 2 then begin
        Exit;
      end;

      lQuery.AddIndex(DESC_IndexName, lColumn.FieldName, [ixDescending]);
      UpdateIndexes(lQuery);
      UpdateIndexUsage(DESC_IndexName, lColumn.FieldName, False);
    end
    else begin
      UpdateIndexUsage(DESC_IndexName, lColumn.FieldName, False);
    end;

  except
    on E: Exception do begin
      // Fout bij indexbeheer
      Exit;
    end;
  end;

  lColumn.tag:= not lColumn.tag;

  if CurrentSortAscending then begin
    lColumn.Title.ImageIndex:= ImageArrowUp;
    lQuery.IndexName:= ASC_IndexName;
  end
  else begin
    lColumn.Title.ImageIndex:= ImageArrowDown;
    lQuery.IndexName:= DESC_IndexName;
  end;

  if (fLastColumn <> nil) and (fLastColumn <> Column) then
    fLastColumn.Title.ImageIndex:= -1;

  fLastColumn:= lColumn;
end;

procedure TDbGridHelper.SortCSVDataSet(ADataset: TObject; AColumn: TObject);
const
  ImageArrowUp   = 0;
  ImageArrowDown = 1;
  MAX_INDEXES    = 100;
var
  BufDS: TBufDataset;
  Col: TColumn;
  Ascending: Boolean;
  AscIdx, DescIdx: string;
  // Hoeveel nieuwe indexen moeten er aangemaakt worden voor deze kolom?
  function SlotsNeeded: Integer;
  begin
    Result := 0;
    if BufDS.IndexDefs.IndexOf('ASC_' + Col.FieldName) = -1 then Inc(Result);
    if BufDS.IndexDefs.IndexOf('DESC_' + Col.FieldName) = -1 then Inc(Result);
  end;

begin
  if not Assigned(ADataset) or not Assigned(AColumn) then Exit;
  if not (ADataset is TBufDataset) then Exit;
  if not (AColumn is TColumn) then Exit;

  BufDS := TBufDataset(ADataset);
  Col   := TColumn(AColumn);

  if not BufDS.Active then Exit;
  if not Assigned(Col.Field) then Exit;

  AscIdx  := 'ASC_'  + Col.FieldName;
  DescIdx := 'DESC_' + Col.FieldName;

  // Maak ruimte vrij als het totaal aantal indexen te hoog wordt
  while (BufDS.IndexDefs.Count + SlotsNeeded > MAX_INDEXES - 2) do
    RemoveOldestBufIndex(BufDS);

  // ASC index aanmaken indien nodig
  if BufDS.IndexDefs.IndexOf(AscIdx) = -1 then
  begin
    BufDS.AddIndex(AscIdx, Col.FieldName, []);
    UpdateIndexUsage(AscIdx, Col.FieldName, True);
  end
  else
    UpdateIndexUsage(AscIdx, Col.FieldName, True);

  // DESC index aanmaken indien nodig
  if BufDS.IndexDefs.IndexOf(DescIdx) = -1 then
  begin
    BufDS.AddIndex(DescIdx, Col.FieldName, [ixDescending]);
    UpdateIndexUsage(DescIdx, Col.FieldName, False);
  end
  else
    UpdateIndexUsage(DescIdx, Col.FieldName, False);

  // Toggle sortering
  Ascending := not Boolean(Col.Tag);
  Col.Tag := Ord(Ascending);

  if Ascending then
  begin
    BufDS.IndexName := AscIdx;
    Col.Title.ImageIndex := ImageArrowUp;
  end
  else
  begin
    BufDS.IndexName := DescIdx;
    Col.Title.ImageIndex := ImageArrowDown;
  end;

  // Vorige kolom resetten
  if Assigned(fLastColumn) and (fLastColumn <> Col) then
    fLastColumn.Title.ImageIndex := -1;

  fLastColumn := Col;
end;






end.
