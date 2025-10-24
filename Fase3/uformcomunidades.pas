unit UFormComunidades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UArbolComunidades, UListaSimple;

type

  { TfrmComunidades }

  TfrmComunidades = class(TForm)
    btnCrearComunidad: TButton;
    btnAgregarUsuario: TButton;
    btnVolver: TButton;
    cbComunidades: TComboBox;
    edtNombreComunidad: TEdit;
    edtCorreoUsuario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnAgregarUsuarioClick(Sender: TObject);
    procedure btnCrearComunidadClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ActualizarComboBoxComunidades;
  public
  end;

var
  frmComunidades: TfrmComunidades;

implementation

{$R *.lfm}

{ TfrmComunidades }

procedure TfrmComunidades.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Gestión de Comunidades';
  Label1.Caption := 'Gestión de Comunidades';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  Label2.Caption := 'Crear Comunidad';
  Label3.Caption := 'Nombre de la comunidad:';
  btnCrearComunidad.Caption := 'Crear Comunidad';

  Label4.Caption := 'Agregar Usuario a Comunidad';
  Label5.Caption := 'Selecciona comunidad:';
  Label6.Caption := 'Correo del usuario:';
  btnAgregarUsuario.Caption := 'Agregar Usuario';

  btnVolver.Caption := 'Volver';

  ActualizarComboBoxComunidades;
end;

procedure TfrmComunidades.btnCrearComunidadClick(Sender: TObject);
var
  NombreComunidad: string;
begin
  NombreComunidad := Trim(edtNombreComunidad.Text);

  if NombreComunidad = '' then
  begin
    ShowMessage('Por favor, ingrese un nombre para la comunidad.');
    Exit;
  end;

  if ArbolComunidadesGlobal.ExisteComunidad(NombreComunidad) then
  begin
    ShowMessage('La comunidad "' + NombreComunidad + '" ya existe.');
    Exit;
  end;

  // Crear la comunidad
  ArbolComunidadesGlobal.InsertarComunidad(NombreComunidad);
  ShowMessage('Comunidad "' + NombreComunidad + '" creada exitosamente.');

  edtNombreComunidad.Text := '';
  ActualizarComboBoxComunidades;
end;

procedure TfrmComunidades.btnAgregarUsuarioClick(Sender: TObject);
var
  NombreComunidad, CorreoUsuario: string;
begin
  if cbComunidades.ItemIndex = -1 then
  begin
    ShowMessage('Por favor, seleccione una comunidad.');
    Exit;
  end;

  NombreComunidad := cbComunidades.Items[cbComunidades.ItemIndex];
  // Extraer solo el nombre de la comunidad (sin la fecha)
  if Pos(' (', NombreComunidad) > 0 then
    NombreComunidad := Copy(NombreComunidad, 1, Pos(' (', NombreComunidad) - 1);

  CorreoUsuario := Trim(edtCorreoUsuario.Text);

  if CorreoUsuario = '' then
  begin
    ShowMessage('Por favor, ingrese el correo del usuario.');
    Exit;
  end;

  if not ArbolComunidadesGlobal.ExisteComunidad(NombreComunidad) then
  begin
    ShowMessage('La comunidad "' + NombreComunidad + '" no existe.');
    Exit;
  end;

  // Verificar si el usuario existe en el sistema
  if not ListaUsuariosGlobal.ExisteUsuario(CorreoUsuario) then
  begin
    ShowMessage('El usuario "' + CorreoUsuario + '" no existe en el sistema.');
    Exit;
  end;

  // Agregar usuario a la comunidad
  ArbolComunidadesGlobal.AgregarUsuarioAComunidad(NombreComunidad, CorreoUsuario);
  ShowMessage('Usuario "' + CorreoUsuario + '" agregado a la comunidad "' + NombreComunidad + '".');

  edtCorreoUsuario.Text := '';
  ActualizarComboBoxComunidades;
end;

procedure TfrmComunidades.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmComunidades.ActualizarComboBoxComunidades;
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
