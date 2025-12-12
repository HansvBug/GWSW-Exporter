unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  DBGrids, DBCtrls, SynHighlighterSQL, SynEdit, Windows, ShellAPI, uIExportView,
  fpspreadsheetctrls, ZConnection, ZDataset, ZDbcIntfs, DB, fpcsvexport;

type

  { TMainForm }

  TMainForm = class(TForm, IExportView)
    btnConnect : TButton;
    btnExecuteSQLPutenStreng : TButton;
    btnExportFromCSV : TButton;
    btnExportFromQuery : TButton;
    Button1 : TButton;
    btnExportToCsv : TButton;
    CSVExporter1 : TCSVExporter;
    DataSource1 : TDataSource;
    DBGrid1 : TDBGrid;
    DBNavigator1 : TDBNavigator;
    edtDatabaseName : TEdit;
    edtOrganisatieNaam : TEdit;
    edtPassword : TEdit;
    edtUserName : TEdit;
    gbConnect : TGroupBox;
    gbGetData : TGroupBox;
    gbExport : TGroupBox;
    lblConnSuccess : TLabel;
    lblDatabaseName : TLabel;
    lblOrganisatieNaam : TLabel;
    lblPassword : TLabel;
    lblUserName : TLabel;
    Memo1 : TMemo;
    MemoputEnStreng : TMemo;
    OpenDialog1 : TOpenDialog;
    PageControl1 : TPageControl;
    ProgressBar1 : TProgressBar;
    SaveDialog1 : TSaveDialog;
    StatusBar1 : TStatusBar;
    TabSheet1 : TTabSheet;
    TabSheet2 : TTabSheet;
    ZConnection1 : TZConnection;
    ZQuery : TZQuery;
    ZTransaction1 : TZTransaction;
    procedure btnConnectClick(Sender : TObject);
    procedure btnExecuteSQLPutenStrengClick(Sender : TObject);
    procedure btnExportFromQueryClick(Sender : TObject);
    procedure btnExportToCsvClick(Sender : TObject);
    procedure Button1Click(Sender : TObject);
    procedure edtDatabaseNameChange(Sender : TObject);
    procedure edtPasswordChange(Sender : TObject);
    procedure edtUserNameChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
  private
    procedure ShowProgress(Current, Total: Integer);
    procedure ShowMessage(const Msg: string);
    procedure ExportComplete(const FileName: string);
    procedure ExportError(const ErrorMsg: string);
    procedure CheckEntryData;
    procedure OptimizeOracleSession(Connection: TZConnection);
  public

  end;

var
  MainForm : TMainForm;

implementation
uses OroxExportPresenter;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnExportFromQueryClick(Sender : TObject);
var
  Presenter: TOroxExportPresenter;
begin
  if not ZQuery.Active then
  begin
    ShowMessage('Query is niet actief. Activeer eerst de query.');
    Exit;
  end;

  if edtOrganisatieNaam.Text = '' then begin
    ShowMessage('Gemeente naam ontbreekt.');
    Exit;
  end;

  SaveDialog1.Filter := 'GWSW-OroX Bestanden (*.orox.ttl)|*.orox.ttl|Turtle Bestanden (*.ttl)|*.ttl|Alle bestanden (*.*)|*.*';
  SaveDialog1.DefaultExt := 'orox.ttl';  // wordt op deze manier voorgescheven.
  SaveDialog1.FileName := edtOrganisatieNaam.Text +  '_export.orox.ttl';

  if SaveDialog1.Execute then
  begin
    Presenter := TOroxExportPresenter.Create(Self);
    try
      Screen.Cursor := crHourGlass;
      Presenter.SetOrganisatieNaam(edtOrganisatieNaam.Text);
      Presenter.ExportToOroxFromQuery(ZQuery, SaveDialog1.FileName);
    finally
      Screen.Cursor := crDefault;
      Presenter.Free;
    end;
  end;
end;

procedure TMainForm.btnExportToCsvClick(Sender : TObject);
begin
  CSVExporter1.Dataset:= DBGrid1.DataSource.DataSet;


  SaveDialog1.Filter := 'CSV Bestanden (*.csv)|Alle bestanden (*.*)|*.*';
  SaveDialog1.DefaultExt := 'csv';  // wordt op deze manier voorgescheven.

  if SaveDialog1.Execute then
  begin
    Screen.Cursor:= crHourGlass;
    CSVExporter1.FileName:= SaveDialog1.FileName;
    CSVExporter1.Execute;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TMainForm.Button1Click(Sender : TObject);
begin
  Close;
end;

procedure TMainForm.edtDatabaseNameChange(Sender : TObject);
begin
  CheckEntryData;
end;

procedure TMainForm.edtPasswordChange(Sender : TObject);
begin
  CheckEntryData;
end;

procedure TMainForm.edtUserNameChange(Sender : TObject);
begin
  CheckEntryData
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  Self.Color:= clWindow;
  Self.Caption:= 'GWSW-export    -->    Proof of concept';
end;

procedure TMainForm.btnConnectClick(Sender : TObject);
begin
  Screen.Cursor := crHourGlass;

  with ZConnection1 do begin
    Database:= edtDatabaseName.Text;
    User:= edtUserName.Text;
    Password:= edtPassword.Text;

    Properties.Clear;
    // Oracle-specifieke optimalisaties
    Properties.Add('arraysize=100'); // Aantal rijen per fetch
    Properties.Add('prefetch_rows=1000'); // Prefetch count
    Properties.Add('statement_cache_size=20');

    // Character set instellingen
    Properties.Add('codepage=UTF8');
    Properties.Add('controls_cp=CP_UTF8');

    // Specifieke Oracle driver optimalisaties
    Properties.Add('BindDoubleAsString=0');
    Properties.Add('BindDecimalAsString=0');
    Properties.Add('rsfetch=100'); // Fetch size voor resultsets

    // algemene instellingen
    AutoCommit := True; // Voor read-only queries
    Catalog := ''; // Oracle gebruikt geen catalog

    Properties.Add('compression=on');
  end;

  if not ZConnection1.Connected then begin
    try
      ZConnection1.Connect;
      ZConnection1.TransactIsolationLevel:= tiReadCommitted; // Bij alleen lezen query's os dit de beste optie.

      lblConnSuccess.Caption:= 'Connection Success';
      btnExecuteSQLPutenStreng.Enabled:= True;

      // Sessie optimaliseren  -- > test of dat werkt...
      OptimizeOracleSession(ZConnection1);

      Screen.Cursor := crDefault;
    Except
      lblConnSuccess.Caption:= 'Connection failed';
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.btnExecuteSQLPutenStrengClick(Sender : TObject);
begin
  screen.Cursor:= crHourGlass;
  try
    with ZQuery do begin
      DisableControls;
      Close;
      FetchRow:= 500;
      SQL.Clear;
      SQL.Text:= MemoputEnStreng.Text;
      Prepared:= True;
      Open;
      if not IsEmpty then FetchAll;
      EnableControls;
    end;

    btnExportFromQuery.Enabled:= True;
    screen.Cursor:= crDefault;
  except
    // failed
    ZQuery.EnableControls;
    screen.Cursor:= crDefault;
  end;
end;

procedure TMainForm.ShowProgress(Current, Total : Integer);
begin
  ProgressBar1.Max := Total;
  ProgressBar1.Position := Current;
  ProgressBar1.Visible := (Total > 0);
  Application.ProcessMessages;
end;

procedure TMainForm.ShowMessage(const Msg : string);
begin
  StatusBar1.SimpleText := Msg;
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  Application.ProcessMessages;
end;

procedure TMainForm.ExportComplete(const FileName : string);
begin
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := False;
  ShowMessage('Export voltooid: ' + ExtractFileName(FileName));

  if MessageDlg('Export voltooid. Bestand openen?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ShellExecute(Handle, 'open', PChar(FileName), nil, nil, SW_SHOWNORMAL);   // uses: ShellAPI
  end;
end;

procedure TMainForm.ExportError(const ErrorMsg : string);
begin
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := False;
  ShowMessage('Export fout: ' + ErrorMsg);
  MessageDlg('Export fout:' + sLineBreak + ErrorMsg, mtError, [mbOK], 0);
end;

procedure TMainForm.CheckEntryData;
begin
  if (edtDatabaseName.Text <> '') and (edtUserName.Text <> '') and (edtPassword.Text <> '') then
    btnConnect.Enabled:= True
  else
    btnConnect.Enabled:= False;
end;

procedure TMainForm.OptimizeOracleSession(Connection : TZConnection);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    // Stel sessie parameters in voor betere performance
    Query.SQL.Text :=
      'ALTER SESSION SET ' +
      'NLS_DATE_FORMAT = ''YYYY-MM-DD HH24:MI:SS'' ' +
      'NLS_NUMERIC_CHARACTERS = ''.,'' ' +
      'OPTIMIZER_MODE = ALL_ROWS ' +  // Voor batch queries
      'CURSOR_SHARING = EXACT ' +     // Meestal beter voor performance
      'QUERY_REWRITE_ENABLED = TRUE';
    Query.ExecSQL;

    // Voor grote datasets:
    Query.SQL.Text := 'ALTER SESSION SET SORT_AREA_SIZE = 1048576';
    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

end.

