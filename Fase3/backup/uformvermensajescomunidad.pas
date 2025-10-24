unit UFormVerMensajesComunidad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UArbolComunidades;

type

  { TfrmVerMensajesComunidad }

  TfrmVerMensajesComunidad = class(TForm)
    btnVerMensajes: TButton;
    btnVolver: TButton;
    cbComunidades: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    memMensajes: TMemo;
    procedure btnVerMensajesClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ActualizarComboBoxComunidades;
  public
  end;

var
  frmVerMensajesComunidad: TfrmVerMensajesComunidad;

implementation

{$R *.lfm}

{ TfrmVerMensajesComunidad }

procedure TfrmVerMensajesComunidad.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Ver Mensajes de Comunidad';
  Label1.Caption := 'Ver Mensajes de Comunidad';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  Label2.Caption := 'Selecciona comunidad:';
  btnVerMensajes.Caption := 'Ver Mensajes';
  btnVolver.Caption := 'Volver';

  memMensajes.Clear;
  ActualizarComboBoxComunidades;
end;

procedure TfrmVerMensajesComunidad.btnVerMensajesClick(Sender: TObject);
var
  NombreComunidad: string;
  Mensajes: TStringList;
begin
  if cbComunidades.ItemIndex = -1 then
  begin
    ShowMessage('Por favor, seleccione una comunidad.');
    Exit;
  end;

  NombreComunidad := cbComunidades.Items[cbComunidades.ItemIndex];

  // Obtener mensajes de la comunidad
  Mensajes := ArbolComunidadesGlobal.ObtenerMensajesComunidad(NombreComunidad);
  try
    memMensajes.Lines.Clear;
    memMensajes.Lines.Add('=== MENSAJES DE LA COMUNIDAD: ' + NombreComunidad + ' ===');
    memMensajes.Lines.Add('');
    memMensajes.Lines.Assign(Mensajes);
  finally
    Mensajes.Free;
  end;
end;

procedure TfrmVerMensajesComunidad.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmVerMensajesComunidad.ActualizarComboBoxComunidades;
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
