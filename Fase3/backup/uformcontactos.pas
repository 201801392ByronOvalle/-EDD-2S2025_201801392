unit UFormContactos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGrafoContactos;

type

  { TfrmContactos }

  TfrmContactos = class(TForm)
    btnAnterior: TButton;
    btnSiguiente: TButton;
    btnVolver: TButton;
    Label1: TLabel;
    lblCorreoContacto: TLabel;
    lblContador: TLabel;
    procedure btnAnteriorClick(Sender: TObject);
    procedure btnSiguienteClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUsuarioActual: string;
    FContactos: array of string;
    FContactoActual: Integer;
    procedure CargarContactosUsuario;
    procedure MostrarContactoActual;
  public
    property UsuarioActual: string read FUsuarioActual write FUsuarioActual;
  end;

var
  frmContactos: TfrmContactos;

implementation

{$R *.lfm}

{ TfrmContactos }

procedure TfrmContactos.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Mis Contactos';
  Label1.Caption := 'Mis Contactos';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  btnAnterior.Caption := '<';
  btnSiguiente.Caption := '>';
  btnVolver.Caption := 'Volver';

  lblCorreoContacto.Caption := '';
  lblContador.Caption := '';

  // Cargar contactos del usuario
  CargarContactosUsuario;
  MostrarContactoActual;
end;

procedure TfrmContactos.btnAnteriorClick(Sender: TObject);
begin
  if Length(FContactos) = 0 then Exit;

  Dec(FContactoActual);
  if FContactoActual < 0 then
    FContactoActual := High(FContactos);

  MostrarContactoActual;
end;

procedure TfrmContactos.btnSiguienteClick(Sender: TObject);
begin
  if Length(FContactos) = 0 then Exit;

  Inc(FContactoActual);
  if FContactoActual > High(FContactos) then
    FContactoActual := 0;

  MostrarContactoActual;
end;

procedure TfrmContactos.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmContactos.CargarContactosUsuario;
var
  NodoUsuario: PNodoGrafo;
  I: Integer;
begin
  SetLength(FContactos, 0);
  FContactoActual := 0;

  if not Assigned(GrafoContactosGlobal) then
  begin
    ShowMessage('No hay contactos cargados en el sistema.');
    Exit;
  end;

  // Obtener el nodo del usuario actual
  NodoUsuario := GrafoContactosGlobal.ObtenerNodoPorUsuario(FUsuarioActual);

  if (NodoUsuario = nil) or (Length(NodoUsuario^.Adyacentes) = 0) then
  begin
    ShowMessage('No tiene contactos agregados.');
    Exit;
  end;

  // Cargar los contactos en el array
  SetLength(FContactos, Length(NodoUsuario^.Adyacentes));
  for I := 0 to High(NodoUsuario^.Adyacentes) do
  begin
    FContactos[I] := NodoUsuario^.Adyacentes[I]^.Usuario;
  end;
end;

procedure TfrmContactos.MostrarContactoActual;
begin
  if Length(FContactos) = 0 then
  begin
    lblCorreoContacto.Caption := 'No tiene contactos';
    lblContador.Caption := '0 / 0';
    btnAnterior.Enabled := False;
    btnSiguiente.Enabled := False;
  end
  else
  begin
    lblCorreoContacto.Caption := FContactos[FContactoActual];
    lblContador.Caption := IntToStr(FContactoActual + 1) + ' / ' + IntToStr(Length(FContactos));
    btnAnterior.Enabled := True;
    btnSiguiente.Enabled := True;
  end;
end;

end.
