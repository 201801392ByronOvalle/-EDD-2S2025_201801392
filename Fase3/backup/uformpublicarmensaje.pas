unit UFormPublicarMensaje;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UArbolComunidades;

type

  { TfrmPublicarMensaje }

  TfrmPublicarMensaje = class(TForm)
    btnPublicar: TButton;
    btnVolver: TButton;
    cbComunidades: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memMensaje: TMemo;
    procedure btnPublicarClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUsuarioActual: string;
    procedure ActualizarComboBoxComunidades;
  public
    property UsuarioActual: string read FUsuarioActual write FUsuarioActual;
  end;

var
  frmPublicarMensaje: TfrmPublicarMensaje;

implementation

{$R *.lfm}

{ TfrmPublicarMensaje }

procedure TfrmPublicarMensaje.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Publicar Mensaje';
  Label1.Caption := 'Publicar Mensaje en Comunidad';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  Label2.Caption := 'Selecciona comunidad:';
  Label3.Caption := 'Mensaje:';
  btnPublicar.Caption := 'Publicar';
  btnVolver.Caption := 'Volver';

  memMensaje.Clear;
  ActualizarComboBoxComunidades;
end;

procedure TfrmPublicarMensaje.btnPublicarClick(Sender: TObject);
var
  NombreComunidad, Mensaje: string;
begin
  if cbComunidades.ItemIndex = -1 then
  begin
    ShowMessage('Por favor, seleccione una comunidad.');
    Exit;
  end;

  NombreComunidad := cbComunidades.Items[cbComunidades.ItemIndex];
  Mensaje := Trim(memMensaje.Text);

  if Mensaje = '' then
  begin
    ShowMessage('Por favor, escriba un mensaje.');
    Exit;
  end;

  // Agregar mensaje a la comunidad
  ArbolComunidadesGlobal.AgregarMensajeAComunidad(NombreComunidad, FUsuarioActual, Mensaje);

  ShowMessage('Mensaje publicado exitosamente en la comunidad "' + NombreComunidad + '".');

  memMensaje.Clear;
end;

procedure TfrmPublicarMensaje.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPublicarMensaje.ActualizarComboBoxComunidades;
var
  Comunidades: TStringList;
begin
  cbComunidades.Clear;

  Comunidades := ArbolComunidadesGlobal.ObtenerComunidades;
  try
    cbComunidades.Items.Assign(Comunidades);
    if cbComunidades.Items.Count > 0 then
      cbComunidades.ItemIndex := 0;
  finally
    Comunidades.Free;
  end;
end;

end.
