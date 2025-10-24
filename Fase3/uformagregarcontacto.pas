unit UFormAgregarContacto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGrafoContactos, UListaSimple;

type

  { TfrmAgregarContacto }

  TfrmAgregarContacto = class(TForm)
    btnAgregar: TButton;
    btnCancelar: TButton;
    edtCorreoContacto: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUsuarioActual: string;
  public
    property UsuarioActual: string read FUsuarioActual write FUsuarioActual;
  end;

var
  frmAgregarContacto: TfrmAgregarContacto;

implementation

{$R *.lfm}

{ TfrmAgregarContacto }

procedure TfrmAgregarContacto.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Agregar Contacto';
  Label1.Caption := 'Agregar Nuevo Contacto';
  Label1.Font.Size := 14;
  Label1.Font.Style := [fsBold];
  Label2.Caption := 'Correo del contacto:';
  btnAgregar.Caption := 'Agregar';
  btnCancelar.Caption := 'Cancelar';
end;

procedure TfrmAgregarContacto.btnAgregarClick(Sender: TObject);
var
  CorreoContacto: string;
  UsuarioEncontrado: TDatoUsuario;
begin
  CorreoContacto := Trim(edtCorreoContacto.Text);

  if CorreoContacto = '' then
  begin
    ShowMessage('Por favor, ingrese el correo del contacto.');
    Exit;
  end;

  if CorreoContacto = FUsuarioActual then
  begin
    ShowMessage('No puede agregarse a sí mismo como contacto.');
    Exit;
  end;

  // Verificar si el contacto existe en la lista de usuarios
  if not ListaUsuariosGlobal.ExisteUsuario(CorreoContacto) then
  begin
    ShowMessage('El usuario "' + CorreoContacto + '" no existe en el sistema.');
    Exit;
  end;

  // Agregar el contacto al grafo
  if Assigned(GrafoContactosGlobal) then
  begin
    // Obtener datos del usuario actual
    UsuarioEncontrado := ListaUsuariosGlobal.ObtenerUsuarioPorEmail(FUsuarioActual);

    // Primero asegurarnos de que ambos usuarios existan en el grafo
    if GrafoContactosGlobal.ObtenerNodoPorUsuario(FUsuarioActual) = nil then
    begin
      // Agregar usuario actual al grafo con sus datos reales
      GrafoContactosGlobal.AgregarUsuario(
        UsuarioEncontrado.Id,
        UsuarioEncontrado.Nombre,
        FUsuarioActual
      );
    end;

    // Obtener datos del contacto
    UsuarioEncontrado := ListaUsuariosGlobal.ObtenerUsuarioPorEmail(CorreoContacto);
    if GrafoContactosGlobal.ObtenerNodoPorUsuario(CorreoContacto) = nil then
    begin
      // Agregar contacto al grafo con sus datos reales
      GrafoContactosGlobal.AgregarUsuario(
        UsuarioEncontrado.Id,
        UsuarioEncontrado.Nombre,
        CorreoContacto
      );
    end;

    // Agregar la relación de contacto
    GrafoContactosGlobal.AgregarContacto(FUsuarioActual, CorreoContacto);

    ShowMessage('Contacto "' + CorreoContacto + '" agregado exitosamente.');
    Close;
  end
  else
  begin
    ShowMessage('Error: Sistema de contactos no disponible.');
  end;
end;

procedure TfrmAgregarContacto.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

end.
