{ Copyright ©2025-2026 Hans van Buggenum }
unit DbGridHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, SQLDB, Contnrs, BufDataset;

type
  { TIndexInfo
    Represents metadata for a database index used for sorting.
    Tracks index name, last usage timestamp, field name, and sort direction. }
  TIndexInfo = class
    IndexName: string;      // Name of the index (e.g., 'ASC_FieldName')
    LastUsed: TDateTime;    // Timestamp of last usage for LRU management (Last Recently Used Management)
    FieldName: string;      // Database field name this index operates on
    IsAscending: Boolean;   // Sort direction: True = Ascending, False = Descending
  end;

  { TDbGridHelper
    Helper class for managing dynamic index creation and sorting in TDBGrid components.
    Provides efficient client-side sorting for TSQLQuery (Oracle) and TBufDataset (CSV)
    with automatic index lifecycle management. }
  TDbGridHelper = class(TObject)
  private
    fLastColumn: TColumn;       // Reference to the last sorted column for UI cleanup
    fIndexList: TObjectList;    // List of TIndexInfo objects for LRU index management

    { Updates the index definitions cache for a TSQLQuery }
    procedure UpdateIndexes(Query: TSQLQuery);

    { Checks if an index exists in the Query's IndexDefs }
    function IndexExists(Query: TSQLQuery; const IndexName: string): Boolean;

    { Retrieves TIndexInfo by index name from the internal cache }
    function GetIndexInfo(const IndexName: string): TIndexInfo;

    { Updates usage timestamp and metadata for an index, creates entry if missing }
    procedure UpdateIndexUsage(const IndexName: string; FieldName: string; IsAscending: Boolean);

    { Finds the least recently used (LRU) index from the internal cache }
    function FindOldestIndex: TIndexInfo;

    { Removes the LRU index from TSQLQuery and internal cache when limit approached }
    procedure RemoveOldestIndex(Query: TSQLQuery);

    { Removes the LRU index from TBufDataset and internal cache }
    procedure RemoveOldestBufIndex(BufDS: TBufDataset);

  public
    { Initializes the helper with empty index cache }
    constructor Create;

    { Cleans up internal objects }
    destructor Destroy; override;

    { Sorts TSQLQuery-based DBGrid by dynamically creating/using ascending/descending indexes
      Parameters:
        DataProvider: TSQLQuery instance attached to the DBGrid
        Column: TColumn being sorted }
    procedure SortOracleDbGrid(DataProvider, Column: TObject);

    { Sorts TBufDataset-based DBGrid (typically for CSV data) with index management
      Parameters:
        ADataset: TBufDataset instance
        AColumn: TColumn being sorted }
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
  Idx: Integer;
begin
  // Exit early if we haven't reached the cleanup threshold (98 out of 100 indexes)
  if fIndexList.Count < 98 then Exit;

  // Find the least recently used (LRU) index from the internal cache
  Oldest := FindOldestIndex;
  if not Assigned(Oldest) then Exit;

  try
    // Remove the index from the actual TSQLQuery (not just from cache)
    if Assigned(Query) then
    begin
      // Ensure IndexDefs are up to date
      UpdateIndexes(Query);

      // Find the index in the query's IndexDefs
      Idx := Query.IndexDefs.IndexOf(Oldest.IndexName);
      if Idx <> -1 then
      begin
        // If this index is currently active, deactivate it first
        if Query.IndexName = Oldest.IndexName then
          Query.IndexName := '';

        // Actually delete the index definition from the query
        Query.IndexDefs.Delete(Idx);
      end;
    end;

    // Remove the index metadata from the internal cache (LRU list)
    fIndexList.Remove(Oldest);
  except
    // Ignore any errors during index removal to prevent application crashes
    // This is a defensive measure - index management errors shouldn't break the UI
  end;
end;

procedure TDbGridHelper.RemoveOldestBufIndex(BufDS: TBufDataset);
var
  Oldest: TIndexInfo;
  Idx: Integer;
begin
  if fIndexList.Count = 0 then Exit;

  Oldest:= FindOldestIndex;
  if not Assigned(Oldest) then Exit;

  Idx:= BufDS.IndexDefs.IndexOf(Oldest.IndexName);
  if Idx <> -1 then
  begin
    // Release active index first if it's the one being removed
    if BufDS.IndexName = Oldest.IndexName then
      BufDS.IndexName:= '';

    BufDS.IndexDefs.Delete(Idx);
  end;

  fIndexList.Remove(Oldest);
end;

constructor TDbGridHelper.Create;
begin
  inherited Create;
  fLastColumn:= nil;
  fIndexList:= TObjectList.Create(True); // True = Owns contained objects
end;

destructor TDbGridHelper.Destroy;
begin
  if Assigned(fIndexList) then
    FreeAndNil(fIndexList);
  inherited Destroy;
end;

procedure TDbGridHelper.SortOracleDbGrid(DataProvider, Column: TObject);
const
  ImageArrowUp = 0;     // ImageList index for ascending sort indicator
  ImageArrowDown = 1;   // ImageList index for descending sort indicator
  MAX_INDEXES = 100;    // Maximum allowed indexes before cleanup
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

  // Skip sorting for BLOB and memo fields
  if (lColumn.Field.DataType in [ftBLOB, ftMemo, ftWideMemo]) then
    Exit;

  ASC_IndexName:= 'ASC_' + lColumn.FieldName;
  DESC_IndexName:= 'DESC_' + lColumn.FieldName;

  // Determine current sort direction from column tag
  CurrentSortAscending:= not Boolean(lColumn.tag);

  try
    // Proactive cleanup when approaching index limit
    if fIndexList.Count >= MAX_INDEXES - 2 then begin
      RemoveOldestIndex(lQuery);
    end;

    // Create or update ascending index
    if not IndexExists(lQuery, ASC_IndexName) then begin
      UpdateIndexes(lQuery);
      // Prevent index overflow
      if lQuery.IndexDefs.Count >= MAX_INDEXES - 2 then begin
        Exit;
      end;

      lQuery.AddIndex(ASC_IndexName, lColumn.FieldName, []);
      UpdateIndexes(lQuery);
      UpdateIndexUsage(ASC_IndexName, lColumn.FieldName, True);
    end
    else begin
      UpdateIndexUsage(ASC_IndexName, lColumn.FieldName, True);
    end;

    // Create or update descending index
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
      // Gracefully handle index management errors
      Exit;
    end;
  end;

  // Toggle sort direction for next click
  lColumn.tag:= not lColumn.tag;

  // Apply sorting and update UI indicators
  if CurrentSortAscending then begin
    lColumn.Title.ImageIndex:= ImageArrowUp;
    lQuery.IndexName:= ASC_IndexName;
  end
  else begin
    lColumn.Title.ImageIndex:= ImageArrowDown;
    lQuery.IndexName:= DESC_IndexName;
  end;

  // Clear previous column's sort indicator
  if (fLastColumn <> nil) and (fLastColumn <> Column) then
    fLastColumn.Title.ImageIndex:= -1;

  fLastColumn:= lColumn;
end;

procedure TDbGridHelper.SortCSVDataSet(ADataset: TObject; AColumn: TObject);
const
  ImageArrowUp   = 0;   // ImageList index for ascending sort indicator
  ImageArrowDown = 1;   // ImageList index for descending sort indicator
  MAX_INDEXES    = 100; // Maximum allowed indexes before cleanup
var
  BufDS: TBufDataset;
  Col: TColumn;
  Ascending: Boolean;
  AscIdx, DescIdx: string;

  { Calculates how many new indexes need creation for current column }
  function SlotsNeeded: Integer;
  begin
    Result:= 0;
    if BufDS.IndexDefs.IndexOf('ASC_' + Col.FieldName) = -1 then Inc(Result);
    if BufDS.IndexDefs.IndexOf('DESC_' + Col.FieldName) = -1 then Inc(Result);
  end;

begin
  if not Assigned(ADataset) or not Assigned(AColumn) then Exit;
  if not (ADataset is TBufDataset) then Exit;
  if not (AColumn is TColumn) then Exit;

  BufDS:= TBufDataset(ADataset);
  Col  := TColumn(AColumn);

  if not BufDS.Active then Exit;
  if not Assigned(Col.Field) then Exit;

  AscIdx := 'ASC_'  + Col.FieldName;
  DescIdx:= 'DESC_' + Col.FieldName;

  // Free space dynamically if approaching index limit
  while (BufDS.IndexDefs.Count + SlotsNeeded > MAX_INDEXES - 2) do
    RemoveOldestBufIndex(BufDS);

  // Create ascending index if missing
  if BufDS.IndexDefs.IndexOf(AscIdx) = -1 then
  begin
    BufDS.AddIndex(AscIdx, Col.FieldName, []);
    UpdateIndexUsage(AscIdx, Col.FieldName, True);
  end
  else
    UpdateIndexUsage(AscIdx, Col.FieldName, True);

  // Create descending index if missing
  if BufDS.IndexDefs.IndexOf(DescIdx) = -1 then
  begin
    BufDS.AddIndex(DescIdx, Col.FieldName, [ixDescending]);
    UpdateIndexUsage(DescIdx, Col.FieldName, False);
  end
  else
    UpdateIndexUsage(DescIdx, Col.FieldName, False);

  // Toggle sort direction
  Ascending:= not Boolean(Col.Tag);
  Col.Tag:= Ord(Ascending);

  // Apply sorting and update UI
  if Ascending then
  begin
    BufDS.IndexName:= AscIdx;
    Col.Title.ImageIndex:= ImageArrowUp;
  end
  else
  begin
    BufDS.IndexName:= DescIdx;
    Col.Title.ImageIndex:= ImageArrowDown;
  end;

  // Clear previous column's sort indicator
  if Assigned(fLastColumn) and (fLastColumn <> Col) then
    fLastColumn.Title.ImageIndex:= -1;

  fLastColumn:= Col;
end;

end.
