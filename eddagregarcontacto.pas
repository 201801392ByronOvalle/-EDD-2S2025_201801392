unit EDDAgregarContacto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EDDEstructuras, EDDGlobal, EDDContactos;

type

  { TfrmAgregarContacto }

  TfrmAgregarContacto = class(TForm)
    btnAgregar: TButton;
    btnCancelar: TButton;
    edtEmail: TEdit;
    lblTitulo: TLabel;
    lblEmail: TLabel;
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private

  public

  end;

var
  frmAgregarContacto: TfrmAgregarContacto;

implementation

{$R *.lfm}

{ TfrmAgregarContacto }

procedure TfrmAgregarContacto.btnAgregarClick(Sender: TObject);
var
  email: string;
  usuario: PUsuario;
begin
  email := Trim(edtEmail.Text);

  if email = '' then
  begin
    ShowMessage('Ingrese un email válido');
    Exit;
  end;

  // Buscar usuario en la lista global
  usuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, email);

  if usuario = nil then
  begin
    ShowMessage('Usuario no encontrado en el sistema');
    Exit;
  end;

  // Agregar a lista de contactos
  InsertarContacto(ListaContactosGlobal, usuario);
  ShowMessage('Contacto agregado exitosamente: ' + usuario^.nombre);
  ModalResult := mrOk;
end;

procedure TfrmAgregarContacto.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

end.

