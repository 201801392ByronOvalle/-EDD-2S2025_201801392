unit UListaSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Definición del registro de usuario
  TDatoUsuario = record
    Id: Integer;
    Nombre: string;
    Usuario: string;
    Password: string;
    Email: string;
    Telefono: string;
  end;

  // Definición del nodo
  PNodoLista = ^TNodoLista;
  TNodoLista = record
    Dato: TDatoUsuario;
    Siguiente: PNodoLista;
  end;

  // Clase de la lista simple
  TListaSimple = class
  private
    FCabeza: PNodoLista;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AgregarUsuario(Id: Integer; Nombre, Usuario, Password, Email, Telefono: string);
    function ObtenerUsuarioPorEmail(Email: string): TDatoUsuario;
    function ExisteUsuario(Email: string): Boolean;
    procedure LimpiarLista;
    function ObtenerPrimero: PNodoLista;
    property Count: Integer read FCount;
  end;

implementation

constructor TListaSimple.Create;
begin
  FCabeza := nil;
  FCount := 0;
end;

destructor TListaSimple.Destroy;
begin
  LimpiarLista;
  inherited Destroy;
end;

procedure TListaSimple.AgregarUsuario(Id: Integer; Nombre, Usuario, Password, Email, Telefono: string);
var
  NuevoNodo, Actual: PNodoLista;
begin
  New(NuevoNodo);
  NuevoNodo^.Dato.Id := Id;
  NuevoNodo^.Dato.Nombre := Nombre;
  NuevoNodo^.Dato.Usuario := Usuario;
  NuevoNodo^.Dato.Password := Password;
  NuevoNodo^.Dato.Email := Email;
  NuevoNodo^.Dato.Telefono := Telefono;
  NuevoNodo^.Siguiente := nil;

  if FCabeza = nil then
    FCabeza := NuevoNodo
  else
  begin
    Actual := FCabeza;
    while Actual^.Siguiente <> nil do
      Actual := Actual^.Siguiente;
    Actual^.Siguiente := NuevoNodo;
  end;
  Inc(FCount);
end;

function TListaSimple.ObtenerUsuarioPorEmail(Email: string): TDatoUsuario;
var
  Actual: PNodoLista;
begin
  Actual := FCabeza;
  while Actual <> nil do
  begin
    if Actual^.Dato.Email = Email then
    begin
      Result := Actual^.Dato;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
  // Si no encuentra, retornar un usuario vacío
  Result.Id := -1;
end;

function TListaSimple.ExisteUsuario(Email: string): Boolean;
var
  Actual: PNodoLista;
begin
  Actual := FCabeza;
  while Actual <> nil do
  begin
    if Actual^.Dato.Email = Email then
    begin
      Result := True;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
  Result := False;
end;

procedure TListaSimple.LimpiarLista;
var
  Actual, Temp: PNodoLista;
begin
  Actual := FCabeza;
  while Actual <> nil do
  begin
    Temp := Actual;
    Actual := Actual^.Siguiente;
    Dispose(Temp);
  end;
  FCabeza := nil;
  FCount := 0;
end;

function TListaSimple.ObtenerPrimero: PNodoLista;
begin
  Result := FCabeza;
end;

end.
