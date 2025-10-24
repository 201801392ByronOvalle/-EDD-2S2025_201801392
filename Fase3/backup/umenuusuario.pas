unit UMenuUsuario;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UBandejaEntrada,
  UControlLog, UFormAgregarContacto, UFormContactos, UGrafoContactos, UListaSimple,
  UFormPublicarMensaje;

type

  { TFormMenuUsuario }

  TFormMenuUsuario = class(TForm)
    btnBandejaEntrada: TButton;
    btnEnviarCorreo: TButton;
    btnBorradores: TButton;
    btnFavoritos: TButton;
    btnContactos: TButton;
    btnPublicarComunidad: TButton;
    btnReportes: TButton;
    btnSalir: TButton;
    btnAgregarContacto: TButton;
    Label1: TLabel;
    lblUsuarioActual: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnBorradoresClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnEnviarCorreoClick(Sender: TObject);
    procedure btnFavoritosClick(Sender: TObject);
    procedure btnPublicarComunidadClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FEmailUsuario: string;
  public
    property EmailUsuario: string read FEmailUsuario write FEmailUsuario;
  end;

var
  FormMenuUsuario: TFormMenuUsuario;

implementation

uses
  ProyectoF2;

{$R *.lfm}

{ TFormMenuUsuario }

procedure TFormMenuUsuario.FormShow(Sender: TObject);
begin
  // Configurar el formulario cuando se muestra
  Caption := 'EDDMail - Menú Usuario';
  Label1.Caption := 'Menú Principal - Usuario Estándar';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  // Mostrar el email del usuario actual
  lblUsuarioActual.Caption := 'Usuario: ' + FEmailUsuario;
end;

procedure TFormMenuUsuario.btnBandejaEntradaClick(Sender: TObject);
begin
  FormBandejaEntrada := TFormBandejaEntrada.Create(Application);
  FormBandejaEntrada.EmailUsuario := FEmailUsuario;
  FormBandejaEntrada.ShowModal;
  FormBandejaEntrada.Free;
end;

procedure TFormMenuUsuario.btnAgregarContactoClick(Sender: TObject);
begin
  // Mostrar formulario para agregar contacto
  frmAgregarContacto := TfrmAgregarContacto.Create(Application);
  frmAgregarContacto.UsuarioActual := FEmailUsuario;
  frmAgregarContacto.ShowModal;
  frmAgregarContacto.Free;
end;

procedure TFormMenuUsuario.btnEnviarCorreoClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Enviar Correo en desarrollo...');
  // Aquí se implementará el envío de correos a contactos
end;

procedure TFormMenuUsuario.btnBorradoresClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Borradores en desarrollo...');
  // Aquí se mostrarán los correos guardados como borradores (Árbol AVL)
end;

procedure TFormMenuUsuario.btnFavoritosClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Favoritos en desarrollo...');
  // Aquí se mostrarán los correos marcados como favoritos (Árbol B)
end;

procedure TFormMenuUsuario.btnContactosClick(Sender: TObject);
begin
  // Mostrar formulario de contactos
  frmContactos := TfrmContactos.Create(Application);
  frmContactos.UsuarioActual := FEmailUsuario;
  frmContactos.ShowModal;
  frmContactos.Free;
end;

procedure TFormMenuUsuario.btnPublicarComunidadClick(Sender: TObject);
begin
  // Mostrar formulario para publicar mensaje en comunidad
  frmPublicarMensaje := TfrmPublicarMensaje.Create(Application);
  frmPublicarMensaje.UsuarioActual := FEmailUsuario;
  frmPublicarMensaje.ShowModal;
  frmPublicarMensaje.Free;
end;

procedure TFormMenuUsuario.btnReportesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Reportes en desarrollo...');
  // Aquí se generarán los reportes del usuario
end;

procedure TFormMenuUsuario.btnSalirClick(Sender: TObject);
begin
  // Registrar salida del usuario
  if Assigned(ControlLogGlobal) then
    ControlLogGlobal.RegistrarSalida(FEmailUsuario);
  Close;
end;

procedure TFormMenuUsuario.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(FormLogin) then
    FormLogin.Show;
end;

end.
