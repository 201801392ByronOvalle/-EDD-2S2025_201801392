unit ProyectoF2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UMenuRoot,
  UListaSimple, UMenuUsuario, UControlLog;

type

  { TFormLogin }

  TFormLogin = class(TForm)
    btnIngresar: TButton;
    btnRegistrar: TButton;
    edtUsuario: TEdit;
    edtContrasenia: TEdit;
    lblContrasenia: TLabel;
    lblTitulo: TLabel;
    lblUsuario: TLabel;
    procedure btnIngresarClick(Sender: TObject);
    procedure btnRegistrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    function ValidarCredenciales(usuario, contrasenia: string): Boolean;
  public
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.lfm}

{ TFormLogin }

procedure TFormLogin.FormCreate(Sender: TObject);
begin
  // Configuración inicial del formulario
  Caption := 'EDDMail - Inicio de Sesión';
  lblTitulo.Caption := 'EDDMail - Sistema de Correo';
  lblTitulo.Font.Size := 14;
  lblTitulo.Font.Style := [fsBold];

  lblUsuario.Caption := 'Email:';
  lblContrasenia.Caption := 'Contraseña:';

  btnIngresar.Caption := 'Ingresar';
  btnRegistrar.Caption := 'Registrar Nuevo Usuario';

  // Placeholders para facilitar las pruebas
  edtUsuario.Text := 'root@edd.com';
  edtContrasenia.Text := 'root123';
end;

procedure TFormLogin.btnIngresarClick(Sender: TObject);
var
  usuario, contrasenia: string;
begin
  usuario := Trim(edtUsuario.Text);
  contrasenia := Trim(edtContrasenia.Text);

  // Validar campos vacíos
  if (usuario = '') or (contrasenia = '') then
  begin
    ShowMessage('Por favor, complete todos los campos.');
    Exit;
  end;

  // Validar credenciales
  if ValidarCredenciales(usuario, contrasenia) then
  begin
    // Registrar entrada en el log
    if Assigned(ControlLogGlobal) then
      ControlLogGlobal.RegistrarEntrada(usuario);

    if usuario = 'root@edd.com' then
    begin
      // Ocultar el formulario de login
      Hide;

      // Crear y mostrar el menú root
      FormMenuRoot := TFormMenuRoot.Create(Application);
      FormMenuRoot.Show;
    end
    else
    begin
      // Ocultar el formulario de login
      Hide;

      // Crear y mostrar el menú de usuario común
      FormMenuUsuario := TFormMenuUsuario.Create(Application);
      FormMenuUsuario.EmailUsuario := usuario; // Pasar el email del usuario
      FormMenuUsuario.Show;

      ShowMessage('Bienvenido usuario estándar: ' + usuario);
    end;

    // Limpiar campos después del login exitoso
    edtUsuario.Text := '';
    edtContrasenia.Text := '';
  end
  else
  begin
    ShowMessage('Credenciales incorrectas. Por favor, verifique su email y contraseña.');
    edtContrasenia.Text := '';
    edtContrasenia.SetFocus;
  end;
end;

procedure TFormLogin.btnRegistrarClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de registro en desarrollo...');
end;

procedure TFormLogin.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // No necesitamos liberar nada aquí, se maneja en las units
end;

function TFormLogin.ValidarCredenciales(usuario, contrasenia: string): Boolean;
var
  UsuarioEncontrado: TDatoUsuario;
begin
  // Primero verificar si es el usuario root
  if (usuario = 'root@edd.com') and (contrasenia = 'root123') then
  begin
    Result := True;
    Exit;
  end;

  // Buscar en la lista global de usuarios cargados
  if Assigned(ListaUsuariosGlobal) then
  begin
    UsuarioEncontrado := ListaUsuariosGlobal.ObtenerUsuarioPorEmail(usuario);
    if (UsuarioEncontrado.Id <> -1) and (UsuarioEncontrado.Password = contrasenia) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Si no se encuentra en ninguna lista
  Result := False;
end;

end.
