unit UArbolComunidades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaSimple;

type
  // Nodo del árbol BST para comunidades
  PNodoComunidad = ^TNodoComunidad;
  TNodoComunidad = record
    Nombre: string;
    FechaCreacion: TDateTime;
    MensajesPublicados: Integer;
    Usuarios: array of string; // Lista de usuarios en la comunidad
    Izquierdo, Derecho: PNodoComunidad;
  end;

  // Clase del árbol BST de comunidades
  TArbolComunidades = class
  private
    FRaiz: PNodoComunidad;
    function CrearNodo(Nombre: string): PNodoComunidad;
    function InsertarRecursivo(Nodo: PNodoComunidad; Nombre: string): PNodoComunidad;
    function BuscarRecursivo(Nodo: PNodoComunidad; Nombre: string): PNodoComunidad;
    procedure InOrdenRecursivo(Nodo: PNodoComunidad; var Resultado: TStringList);
    procedure LimpiarRecursivo(Nodo: PNodoComunidad);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertarComunidad(Nombre: string);
    function BuscarComunidad(Nombre: string): PNodoComunidad;
    function ExisteComunidad(Nombre: string): Boolean;
    procedure AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: string);
    function ObtenerComunidades: TStringList;
    procedure LimpiarArbol;
  end;

var
  ArbolComunidadesGlobal: TArbolComunidades;

implementation

constructor TArbolComunidades.Create;
begin
  inherited Create;
  FRaiz := nil;
end;

destructor TArbolComunidades.Destroy;
begin
  LimpiarArbol;
  inherited Destroy;
end;

function TArbolComunidades.CrearNodo(Nombre: string): PNodoComunidad;
begin
  New(Result);
  Result^.Nombre := Nombre;
  Result^.FechaCreacion := Now;
  Result^.MensajesPublicados := 0;
  SetLength(Result^.Usuarios, 0);
  Result^.Izquierdo := nil;
  Result^.Derecho := nil;
end;

function TArbolComunidades.InsertarRecursivo(Nodo: PNodoComunidad; Nombre: string): PNodoComunidad;
begin
  if Nodo = nil then
  begin
    Result := CrearNodo(Nombre);
    Exit;
  end;

  if CompareText(Nombre, Nodo^.Nombre) < 0 then
    Nodo^.Izquierdo := InsertarRecursivo(Nodo^.Izquierdo, Nombre)
  else if CompareText(Nombre, Nodo^.Nombre) > 0 then
    Nodo^.Derecho := InsertarRecursivo(Nodo^.Derecho, Nombre);

  Result := Nodo;
end;

procedure TArbolComunidades.InsertarComunidad(Nombre: string);
begin
  FRaiz := InsertarRecursivo(FRaiz, Nombre);
end;

function TArbolComunidades.BuscarRecursivo(Nodo: PNodoComunidad; Nombre: string): PNodoComunidad;
begin
  if (Nodo = nil) or (CompareText(Nodo^.Nombre, Nombre) = 0) then
    Result := Nodo
  else if CompareText(Nombre, Nodo^.Nombre) < 0 then
    Result := BuscarRecursivo(Nodo^.Izquierdo, Nombre)
  else
    Result := BuscarRecursivo(Nodo^.Derecho, Nombre);
end;

function TArbolComunidades.BuscarComunidad(Nombre: string): PNodoComunidad;
begin
  Result := BuscarRecursivo(FRaiz, Nombre);
end;

function TArbolComunidades.ExisteComunidad(Nombre: string): Boolean;
begin
  Result := BuscarComunidad(Nombre) <> nil;
end;

procedure TArbolComunidades.AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: string);
var
  Comunidad: PNodoComunidad;
  I: Integer;
begin
  Comunidad := BuscarComunidad(NombreComunidad);
  if Comunidad = nil then Exit;

  // Verificar si el usuario ya está en la comunidad
  for I := 0 to High(Comunidad^.Usuarios) do
  begin
    if Comunidad^.Usuarios[I] = EmailUsuario then
      Exit; // Usuario ya está en la comunidad
  end;

  // Agregar usuario a la comunidad
  SetLength(Comunidad^.Usuarios, Length(Comunidad^.Usuarios) + 1);
  Comunidad^.Usuarios[High(Comunidad^.Usuarios)] := EmailUsuario;
end;

procedure TArbolComunidades.InOrdenRecursivo(Nodo: PNodoComunidad; var Resultado: TStringList);
begin
  if Nodo = nil then Exit;

  InOrdenRecursivo(Nodo^.Izquierdo, Resultado);
  Resultado.Add(Nodo^.Nombre);
  InOrdenRecursivo(Nodo^.Derecho, Resultado);
end;

function TArbolComunidades.ObtenerComunidades: TStringList;
begin
  Result := TStringList.Create;
  InOrdenRecursivo(FRaiz, Result);
end;

procedure TArbolComunidades.LimpiarRecursivo(Nodo: PNodoComunidad);
begin
  if Nodo = nil then Exit;

  LimpiarRecursivo(Nodo^.Izquierdo);
  LimpiarRecursivo(Nodo^.Derecho);
  SetLength(Nodo^.Usuarios, 0);
  Dispose(Nodo);
end;

procedure TArbolComunidades.LimpiarArbol;
begin
  LimpiarRecursivo(FRaiz);
  FRaiz := nil;
end;

initialization
  ArbolComunidadesGlobal := TArbolComunidades.Create;

finalization
  if Assigned(ArbolComunidadesGlobal) then
    ArbolComunidadesGlobal.Free;

end.
