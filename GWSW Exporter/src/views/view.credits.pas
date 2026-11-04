unit view.credits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  model.intf;

type

  { TFrmCredits }

  TFrmCredits = class(TForm)
    btnClose: TButton;
    memCredits: TMemo;
    Panel1: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fPresenter: IPresenterMain;
    procedure SetmemoText(const AVersion: String);
  public
    property Presenter: IPresenterMain read fPresenter write fPresenter;
  end;

var
  FrmCredits: TFrmCredits;

procedure ShowCreditsForm(aMainPresenter: IPresenterMain; const AVersion: String);

implementation

procedure ShowCreditsForm(aMainPresenter: IPresenterMain; const AVersion: String);
begin
  with TFrmCredits.Create(nil) do
    try
      Presenter:= aMainPresenter;
      SetmemoText(AVersion);
      btnClose.caption:= fPresenter.GetstaticText('view.main', 'btnClose');
      ShowModal;
    finally
      Free;
    end;
end;

{$R *.lfm}

{ TFrmCredits }

procedure TFrmCredits.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCredits.FormCreate(Sender: TObject);
begin

end;

procedure TFrmCredits.SetmemoText(const AVersion: String);
begin
  memCredits.Clear;

  memCredits.Lines.Add(fPresenter.GetstaticText('view.main', 'frmMain'));
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'lblVersion') + AVersion);
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'Description'));
  memCredits.Lines.Add('');
  memCredits.Lines.Add('------------------------------------------------------------------------------');
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'DevelopedBy') + 'Hans van Buggenum');
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'SourceCode'));
  memCredits.Lines.Add(#9 + 'https://codeberg.org/HansvBug/GWSW-Exporter');
  memCredits.Lines.Add(#9 + 'https://github.com/HansvBug/GWSW-Exporter');
  memCredits.Lines.Add('');
  memCredits.Lines.Add('------------------------------------------------------------------------------');
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'BuildWith'));
  memCredits.Lines.Add(fPresenter.GetstaticText('view.about', 'SpecialThanks'));




end;

end.

