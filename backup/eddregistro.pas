unit EDDRegistro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EDDEstructuras, EDDGlobal;

type

  { TfrmRegistro }

  TfrmRegistro = class(TForm)
    btnRegistrar: TButton;
    btnCancelar: TButton;
    edtNombre: TEdit;
    edtUsuario: TEdit;
    edtEmail: TEdit;
    edtTelefono: TEdit;
    edtContrasenia: TEdit;
    edtConfirmarContrasenia: TEdit;
    lblConfirmarContrasenia: TLabel;
    lblNombre: TLabel;
    lblUsuario: TLabel;
    lblEmail: TLabel;
    lblTelefono: TLabel;
    lblContrasenia: TLabel;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnRegistrarClick(Sender: TObject);
  private
    function GenerarNuevoID: Integer;
  public

  end;

var
  frmRegistro: TfrmRegistro;

implementation

{$R *.lfm}

{ TfrmRegistro }

function TfrmRegistro.GenerarNuevoID: Integer;
var
  actual: PNodoUsuario;
  maxID: Integer;
begin
  maxID := 0;
  actual := ListaUsuariosGlobal.cabeza;

  // Si la lista esta vacia retornar 1
  if actual = nil then
  begin
    Result := 1;
    Exit;
  end;

  //Recorrer la lista para encontrar el ID maximo
  while actual <> nil do
  begin
    if actual^.dato^.id > maxID then
      maxID := actual^.dato^.id;
    actual := actual^.siguiente;
  end;

  Result := maxID + 1;
end;

procedure TfrmRegistro.btnRegistrarClick(Sender: TObject);
var
  nuevoUsuario: PUsuario;
  nombre, usuario, email, telefono, contrasenia, confirmacion: string;
begin
  // Obtener datos
  nombre := Trim(edtNombre.Text);
  usuario := Trim(edtUsuario.Text);
  email := Trim(edtEmail.Text);
  telefono := Trim(edtTelefono.Text);
  contrasenia := Trim(edtContrasenia.Text);
  confirmacion := Trim(edtConfirmarContrasenia.Text);

  // Validaciones
  if (nombre = '') or (usuario = '') or (email = '') or (telefono = '')
    or (contrasenia = '') then
  begin
    ShowMessage('Todos los campos son obligatorios');
    Exit;
  end;

  if contrasenia <> confirmacion then
  begin
    ShowMessage('Las contraseñas no coinciden');
    edtContrasenia.Text := '';
    edtConfirmarContrasenia.Text := '';
    edtContrasenia.SetFocus;
    Exit;
  end;

  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, email) <> nil then
  begin
    ShowMessage('El email ya está registrado');
    edtEmail.SetFocus;
    Exit;
  end;

  // Crear y insertar usuario
  nuevoUsuario := CrearUsuario(GenerarNuevoID, nombre, usuario, email,
                                               telefono, contrasenia);
  InsertarUsuario(ListaUsuariosGlobal, nuevoUsuario);

  ShowMessage('Usuario registrado exitosamente');
  ModalResult := mrOk;
end;

procedure TfrmRegistro.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

