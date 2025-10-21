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
    function EstaInicializada: Boolean;
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

// Declarar la variable global aquí
var
  ListaUsuariosGlobal: TListaSimple;

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

function TListaSimple.EstaInicializada: Boolean;
begin
  Result := True; // Siempre está inicializada después del Create
end;

procedure TListaSimple.AgregarUsuario(Id: Integer; Nombre, Usuario, Password, Email, Telefono: string);
var
  NuevoNodo, Actual: PNodoLista;
begin
  if not EstaInicializada then Exit;

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
  // Inicializar resultado por si no encuentra
  Result.Id := -1;

  if not EstaInicializada then Exit;

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
end;

function TListaSimple.ExisteUsuario(Email: string): Boolean;
var
  Actual: PNodoLista;
begin
  Result := False;
  if not EstaInicializada then Exit;

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
end;

procedure TListaSimple.LimpiarLista;
var
  Actual, Temp: PNodoLista;
begin
  if not EstaInicializada then Exit;

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
  if EstaInicializada then
    Result := FCabeza
  else
    Result := nil;
end;

// Inicializar la variable global
initialization
  ListaUsuariosGlobal := TListaSimple.Create;

finalization
  if Assigned(ListaUsuariosGlobal) then
    ListaUsuariosGlobal.Free;

end.
