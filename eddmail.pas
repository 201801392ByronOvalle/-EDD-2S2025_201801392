unit EDDMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EDDGlobal, EDDEstructuras, EDDRegistro, EDDMenuRoot, EDDMenuUsuario;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnCrearCuenta: TButton;
    btnLogin: TButton;
    edtUsuario: TEdit;
    edtContrasenia: TEdit;
    lblTitulo: TLabel;
    lblUsuario: TLabel;
    lblContrasenia: TLabel;
    procedure btnCrearCuentaClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CargarUsuariosIniciales;
    procedure AbrirMenuRoot;
    procedure AbrirMenuUsuario(usuario: PUsuario);
  public

  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  CargarUsuariosIniciales;
end;

procedure TfrmLogin.CargarUsuariosIniciales;
begin
  // carga de prueba quemada ya no se necesita
end;

// Menus para root y usuario comun
procedure TfrmLogin.AbrirMenuRoot;
var
  menuRoot: TfrmMenuRoot;
begin
  Self.Hide;
  menuRoot := TfrmMenuRoot.Create(nil);
  try
    menuRoot.ShowModal;
  finally
    menuRoot.Free;
    Self.Show;
    edtUsuario.Text := '';
    edtContrasenia.Text := '';
    edtUsuario.SetFocus;
  end;
end;

procedure TfrmLogin.AbrirMenuUsuario(usuario: PUsuario);
var
  menuUsuario: TfrmMenuUsuario;
begin
  Self.Hide;
  menuUsuario := TfrmMenuUsuario.Create(nil);
  try
    menuUsuario.SetDatosUsuario(usuario);
    menuUsuario.ShowModal;
  finally
    menuUsuario.Free;
    Self.Show;
    edtUsuario.Text := '';
    edtContrasenia.Text := '';
    edtUsuario.SetFocus;
  end;
end;

procedure TfrmLogin.btnLoginClick(Sender: TObject);
var
  email, contrasenia: string;
  usuario: PUsuario;
begin
  email := Trim(edtUsuario.Text);
  contrasenia := Trim(edtContrasenia.Text);

  // 1ro: Validar root quemado
  if (email = 'root@edd.com') and (contrasenia = 'root123') then
  begin
    AbrirMenuRoot;
  end
  // 2do: Validar usuarios normales
  else if ValidarLogin(ListaUsuariosGlobal, email, contrasenia) then
  begin
    usuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, email);
    if usuario <> nil then
    begin
      AbrirMenuUsuario(usuario);
    end;
  end
  else
  begin
    ShowMessage('Usuario o Contraseña incorrecto');
    edtContrasenia.Text := '';
    edtContrasenia.SetFocus;
  end;
end;

procedure TfrmLogin.btnCrearCuentaClick(Sender: TObject);
var formRegistro: TfrmRegistro;
begin
  formRegistro := TfrmRegistro.Create(nil);
  try
    if formRegistro.ShowModal = mrOk then
    begin
      ShowMessage('Registro exitoso. Inicia sesión para continuar');
      edtUsuario.Text := '';
      edtContrasenia.Text := '';
    end;
  finally
    formRegistro.Free;
  end;
end;

end.
