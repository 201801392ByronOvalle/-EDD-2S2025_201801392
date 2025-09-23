unit uPrincipalRoot;  // Cambiado de frmPrincipalRoot a uPrincipalRoot

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmPrincipalRoot }

  TfrmPrincipalRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnComunidades: TButton;
    btnReportes: TButton;
    btnVerMensajesComunidad: TButton;
    lblTitulo: TLabel;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnComunidadesClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnVerMensajesComunidadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmPrincipalRoot: TfrmPrincipalRoot;

implementation

{$R *.lfm}

{ TfrmPrincipalRoot }

procedure TfrmPrincipalRoot.FormCreate(Sender: TObject);
begin
  // Configuración inicial del formulario
  Caption := 'EDDMail - Panel de Control Root';
  Width := 600;
  Height := 400;
  Position := poScreenCenter;
end;

procedure TfrmPrincipalRoot.btnCargaMasivaClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Carga Masiva pendiente de implementar');
end;

procedure TfrmPrincipalRoot.btnComunidadesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Comunidades pendiente de implementar');
end;

procedure TfrmPrincipalRoot.btnReportesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Reportes pendiente de implementar');
end;

procedure TfrmPrincipalRoot.btnVerMensajesComunidadClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Ver Mensajes de Comunidad pendiente de implementar');
end;

end.
