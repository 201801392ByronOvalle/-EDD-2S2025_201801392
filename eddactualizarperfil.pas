unit EDDActualizarPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EDDEstructuras, EDDGlobal;

type

  { TfrmActualizarPerfil }

  TfrmActualizarPerfil = class(TForm)
    btnGuardar: TButton;
    btnCancelar: TButton;
    edtUsuario: TEdit;
    edtTelefono: TEdit;
    lblTitulo: TLabel;
    lblUsuario: TLabel;
    lblTelefono: TLabel;
    lblNombre: TLabel;
    lblEmail: TLabel;
    lblDatosNombre: TLabel;
    lblDatosEmail: TLabel;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    UsuarioActual: PUsuario;
  public
    procedure SetUsuarioActual(usuario: PUsuario);
  end;

var
  frmActualizarPerfil: TfrmActualizarPerfil;

implementation

{$R *.lfm}

{ TfrmActualizarPerfil }


procedure TfrmActualizarPerfil.SetUsuarioActual(usuario: PUsuario);
begin
  UsuarioActual := usuario;
end;

procedure TfrmActualizarPerfil.FormShow(Sender: TObject);
begin
  if UsuarioActual <> nil then
  begin
    // Mostrar datos actuales (solo lectura para nombre y email)
    lblDatosNombre.Caption := UsuarioActual^.nombre;
    lblDatosEmail.Caption := UsuarioActual^.email;
    edtUsuario.Text := UsuarioActual^.usuario;
    edtTelefono.Text := UsuarioActual^.telefono;
  end;
end;

procedure TfrmActualizarPerfil.btnGuardarClick(Sender: TObject);
var
  nuevoUsuario, nuevoTelefono: string;
begin
  if UsuarioActual = nil then Exit;

  nuevoUsuario := Trim(edtUsuario.Text);
  nuevoTelefono := Trim(edtTelefono.Text);

  // Validaciones
  if nuevoUsuario = '' then
  begin
    ShowMessage('El nombre de usuario no puede estar vacío');
    edtUsuario.SetFocus;
    Exit;
  end;

  if nuevoTelefono = '' then
  begin
    ShowMessage('El teléfono no puede estar vacío');
    edtTelefono.SetFocus;
    Exit;
  end;

  // Verificar si el nombre de usuario ya existe (excepto para el usuario actual)
  if (nuevoUsuario <> UsuarioActual^.usuario) and
     (BuscarUsuarioPorUsuario(ListaUsuariosGlobal, nuevoUsuario) <> nil) then
  begin
    ShowMessage('El nombre de usuario ya está en uso');
    edtUsuario.SetFocus;
    Exit;
  end;

  // Actualizar datos
  UsuarioActual^.usuario := nuevoUsuario;
  UsuarioActual^.telefono := nuevoTelefono;

  ShowMessage('Perfil actualizado exitosamente');
  ModalResult := mrOk;
end;

procedure TfrmActualizarPerfil.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

