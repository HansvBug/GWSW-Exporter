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

unit OroxExportPresenter;

interface

uses
  SysUtils, Classes, ZDataset,
  uIExportView, OroxExportModel;

type

  { TOroxExportPresenter }

  TOroxExportPresenter = class
  private
    FView: IExportView;
    FModel: TOroxExportModel;
    procedure UpdateProgress(Current, Total: Integer);
  public
    constructor Create(AView : IExportView);
    destructor Destroy; override;

    procedure ExportToOroxFromQuery(Query: TZQuery; const FileName: string);
    procedure ExportToOroxFromCSV(const CSVFilename, OutputFilename: string);
    procedure SetOrganisatieNaam(const Value: string);
  end;

implementation

constructor TOroxExportPresenter.Create(AView: IExportView);
begin
  FView := AView;
  FModel := TOroxExportModel.Create;
  FModel.OnProgress := @UpdateProgress;
end;

destructor TOroxExportPresenter.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TOroxExportPresenter.UpdateProgress(Current, Total: Integer);
begin
  if Assigned(FView) then
    FView.ShowProgress(Current, Total);
end;

procedure TOroxExportPresenter.ExportToOroxFromQuery(Query : TZQuery;
  const FileName : string);
begin
  try
    FView.ShowMessage('Bezig met exporteren vanuit database...');
    FModel.ExportToOroxFromQuery(Query, FileName);
    FView.ExportComplete(FileName);
  except
    on E: Exception do
      FView.ExportError(E.Message);
  end;
end;

procedure TOroxExportPresenter.ExportToOroxFromCSV(const CSVFilename, OutputFilename: string);
begin
  { #todo : ttl kunnen opbouwen vanuit een csv. (en mischien ook nog shape) }
  try
    FView.ShowMessage('Bezig met exporteren vanuit CSV...');
    FModel.ExportToOroxFromCSV(CSVFilename, OutputFilename);
    FView.ExportComplete(OutputFilename);
  except
    on E: Exception do
      FView.ExportError(E.Message);
  end;
end;

procedure TOroxExportPresenter.SetOrganisatieNaam(const Value : string);
begin
  FModel.OrganisatieNaam := Value;
end;

end.
