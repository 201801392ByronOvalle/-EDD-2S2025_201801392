unit EDDVerContactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EDDEstructuras, EDDGlobal, EDDContactos;

type

  { TfrmVerContactos }

  TfrmVerContactos = class(TForm)
    btnAnterior: TButton;
    btnSiguiente: TButton;
    btnCerrar: TButton;
    lblTitulo: TLabel;
    lblUsuario: TLabel;
    lblEmail: TLabel;
    lblTelefono: TLabel;
    lblNombre: TLabel;
    procedure btnAnteriorClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnSiguienteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ContactoActual: PNodoContacto;
    procedure MostrarContactoActual;
  public

  end;

var
  frmVerContactos: TfrmVerContactos;

implementation

{$R *.lfm}

{ TfrmVerContactos }

procedure TfrmVerContactos.FormShow(Sender: TObject);
begin
  if (ListaContactosGlobal.cabeza = nil) or (ListaContactosGlobal.tamanio = 0) then
  begin
    ShowMessage('No hay contactos para mostrar');
    Close;
    Exit;
  end;

  ContactoActual := ListaContactosGlobal.cabeza;
  MostrarContactoActual;

  // Deshabilitar flechas si solo hay un contacto
  btnAnterior.Enabled := (ListaContactosGlobal.tamanio > 1);
  btnSiguiente.Enabled := (ListaContactosGlobal.tamanio > 1);
end;

procedure TfrmVerContactos.MostrarContactoActual;
begin
  if ContactoActual = nil then Exit;

  lblNombre.Caption := 'Nombre: ' + ContactoActual^.usuario^.nombre;
  lblUsuario.Caption := 'Usuario: ' + ContactoActual^.usuario^.usuario;
  lblEmail.Caption := 'Correo: ' + ContactoActual^.usuario^.email;
  lblTelefono.Caption := 'Teléfono: ' + ContactoActual^.usuario^.telefono;
end;

procedure TfrmVerContactos.btnSiguienteClick(Sender: TObject);
begin
  if (ContactoActual = nil) or (ContactoActual^.siguiente = nil) then Exit;

  ContactoActual := ContactoActual^.siguiente;
  MostrarContactoActual;
end;

procedure TfrmVerContactos.btnAnteriorClick(Sender: TObject);
var
  temp: PNodoContacto;
begin
  if (ContactoActual = nil) or (ListaContactosGlobal.cabeza = nil) then Exit;

  // Encontrar el nodo anterior en lista circular
  temp := ListaContactosGlobal.cabeza;
  while (temp <> nil) and (temp^.siguiente <> ContactoActual) do
  begin
    if temp^.siguiente = ListaContactosGlobal.cabeza then break; // Evitar loop infinito
    temp := temp^.siguiente;
  end;

  if temp <> nil then
  begin
    ContactoActual := temp;
    MostrarContactoActual;
  end;
end;

procedure TfrmVerContactos.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

