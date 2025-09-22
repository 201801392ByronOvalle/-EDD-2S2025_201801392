unit edd_fase2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnIngresar: TButton;
    btnRegistrar: TButton;
    edtEmail: TEdit;
    edtPassword: TEdit;
    lblEmail: TLabel;
    lblContrasena: TLabel;
    lblTitulo: TLabel;
    procedure btnIngresarClick(Sender: TObject);
    procedure btnRegistrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.btnIngresarClick(Sender: TObject);
begin
  // Validar credenciales del usuario root
  if (edtEmail.Text = 'root@edd.com') and (edtPassword.Text = 'root123') then
  begin
    ShowMessage('Bienvenido usuario root');
    // Aquí abrirías el formulario principal para el usuario root
  end
  else
  begin
    // Buscar en la lista de usuarios estándar (implementar luego)
    // Por ahora mostramos un mensaje de error
    ShowMessage('Credenciales incorrectas o usuario no existe');
  end;
end;

procedure TfrmLogin.btnRegistrarClick(Sender: TObject);
begin
  // Aquí abrirías el formulario de registro de nuevos usuarios
  ShowMessage('Funcionalidad de registro pendiente de implementar');
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  // Configuración inicial del formulario
  Caption := 'EDDMail - Inicio de Sesión';
end;

end.
