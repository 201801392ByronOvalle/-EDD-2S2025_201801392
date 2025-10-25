unit UFormDetalleCorreo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmDetalleCorreo }

  TfrmDetalleCorreo = class(TForm)
    btnEliminar: TButton;
    btnFavorito: TButton;
    btnVolver: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblRemitente: TLabel;
    lblAsunto: TLabel;
    lblFecha: TLabel;
    memMensaje: TMemo;
    procedure btnEliminarClick(Sender: TObject);
    procedure btnFavoritoClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRemitente: string;
    FAsunto: string;
    FFecha: string;
    FMensaje: string;
  public
    property Remitente: string read FRemitente write FRemitente;
    property Asunto: string read FAsunto write FAsunto;
    property Fecha: string read FFecha write FFecha;
    property Mensaje: string read FMensaje write FMensaje;
  end;

var
  frmDetalleCorreo: TfrmDetalleCorreo;

implementation

{$R *.lfm}

{ TfrmDetalleCorreo }

procedure TfrmDetalleCorreo.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Detalle del Correo';
  Label1.Caption := 'Detalle del Correo';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  Label2.Caption := 'Remitente:';
  Label3.Caption := 'Asunto:';
  Label4.Caption := 'Fecha:';

  lblRemitente.Caption := FRemitente;
  lblAsunto.Caption := FAsunto;
  lblFecha.Caption := FFecha;
  memMensaje.Lines.Text := FMensaje;

  btnEliminar.Caption := 'Eliminar';
  btnFavorito.Caption := 'Favorito';
  btnVolver.Caption := 'Volver';
end;

procedure TfrmDetalleCorreo.btnEliminarClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de eliminar correo en desarrollo...');
  // Aquí se implementará la lógica para mover a papelera
end;

procedure TfrmDetalleCorreo.btnFavoritoClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de marcar como favorito en desarrollo...');
  // Aquí se implementará la lógica para agregar a favoritos (Árbol B)
end;

procedure TfrmDetalleCorreo.btnVolverClick(Sender: TObject);
begin
  Close;
end;

end.
