unit edd_fase2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uPrincipalRoot;  // Cambiado aquí

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
    // Abrir formulario principal del root
    frmPrincipalRoot := TfrmPrincipalRoot.Create(Application);
    try
      frmPrincipalRoot.ShowModal;
    finally
      frmPrincipalRoot.Free;
    end;
  end
  else
  begin
    ShowMessage('Credenciales incorrectas o usuario no existe');
  end;
end;

procedure TfrmLogin.btnRegistrarClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de registro pendiente de implementar');
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Inicio de Sesión';
end;

end.
