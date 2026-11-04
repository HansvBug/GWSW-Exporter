

unit view.about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  model.intf;

type
  { TFrmAbout }
  TFrmAbout = class(TForm)
    btnClose: TButton;
    Image1: TImage;
    lblCredits: TLabel;
    lblApplicationName: TLabel;
    lblBUG: TLabel;
    lblBUGFull: TLabel;
    lblVersion: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblCreditsClick(Sender: TObject);
    procedure lblCreditsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    fPresenter: IPresenterMain;
    fVersion: String;
    procedure InitTexts(const AVersion: String);
    function BuildClipboardText: String;
  public
    property Presenter: IPresenterMain read fPresenter write fPresenter;
  end;

var
  FrmAbout: TFrmAbout;

procedure ShowAboutForm(aMainPresenter: IPresenterMain; const AVersion: String);

implementation
uses view.credits;

procedure ShowAboutForm(aMainPresenter: IPresenterMain; const AVersion: String);
begin
  with TFrmAbout.Create(nil) do
    try
      Presenter:= aMainPresenter;
      fVersion:= AVersion;
      InitTexts(AVersion);
      ShowModal;
    finally
      Free;
    end;
end;

{$R *.lfm}

{ TFrmAbout }
procedure TFrmAbout.FormCreate(Sender: TObject);
begin

end;

procedure TFrmAbout.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
{  if Key = VK_ESCAPE then
  begin
    Key:= 0;
    Close;
  end;}
end;

procedure TFrmAbout.lblCreditsClick(Sender: TObject);
begin
  ShowCreditsForm(Presenter, fVersion);
end;

procedure TFrmAbout.lblCreditsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TFrmAbout.InitTexts(const AVersion: String);
begin
  if fPresenter = nil then Exit;

  Caption:= fPresenter.GetstaticText('view.about', 'Caption');
  lblVersion.Caption:= fPresenter.GetstaticText('view.about', 'lblVersion') + AVersion;
  lblCredits.Caption:= fPresenter.GetstaticText('view.about', 'lblCredits');
end;

function TFrmAbout.BuildClipboardText: String;
begin

end;

procedure TFrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

