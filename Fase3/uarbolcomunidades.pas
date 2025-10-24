unit UArbolComunidades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaSimple;

type
  // Registro para almacenar cada mensaje
  TMensajeComunidad = record
    Autor: string;
    Fecha: TDateTime;
    Mensaje: string;
  end;

  // Nodo del árbol BST para comunidades
  PNodoComunidad = ^TNodoComunidad;
  TNodoComunidad = record
    Nombre: string;
    FechaCreacion: TDateTime;
    Mensajes: array of TMensajeComunidad; // Array de mensajes
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
    procedure AgregarMensajeAComunidad(NombreComunidad, Autor, Mensaje: string);
    function ObtenerComunidades: TStringList;
    function ObtenerMensajesComunidad(NombreComunidad: string): TStringList;
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
  SetLength(Result^.Mensajes, 0);
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

procedure TArbolComunidades.AgregarMensajeAComunidad(NombreComunidad, Autor, Mensaje: string);
var
  Comunidad: PNodoComunidad;
  NuevoMensaje: TMensajeComunidad;
begin
  Comunidad := BuscarComunidad(NombreComunidad);
  if Comunidad = nil then Exit;

  // Crear nuevo mensaje
  NuevoMensaje.Autor := Autor;
  NuevoMensaje.Fecha := Now;
  NuevoMensaje.Mensaje := Mensaje;

  // Agregar mensaje a la comunidad
  SetLength(Comunidad^.Mensajes, Length(Comunidad^.Mensajes) + 1);
  Comunidad^.Mensajes[High(Comunidad^.Mensajes)] := NuevoMensaje;
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

function TArbolComunidades.ObtenerMensajesComunidad(NombreComunidad: string): TStringList;
var
  Comunidad: PNodoComunidad;
  I: Integer;
begin
  Result := TStringList.Create;
  Comunidad := BuscarComunidad(NombreComunidad);

  if Comunidad = nil then
  begin
    Result.Add('La comunidad no existe.');
    Exit;
  end;

  if Length(Comunidad^.Mensajes) = 0 then
  begin
    Result.Add('No hay mensajes en esta comunidad.');
    Exit;
  end;

  for I := 0 to High(Comunidad^.Mensajes) do
  begin
    Result.Add('[' + FormatDateTime('dd/mm/yyyy hh:nn', Comunidad^.Mensajes[I].Fecha) + '] ' +
               Comunidad^.Mensajes[I].Autor + ':' + sLineBreak +
               Comunidad^.Mensajes[I].Mensaje + sLineBreak +
               '────────────────────');
  end;
end;

procedure TArbolComunidades.LimpiarRecursivo(Nodo: PNodoComunidad);
begin
  if Nodo = nil then Exit;

  LimpiarRecursivo(Nodo^.Izquierdo);
  LimpiarRecursivo(Nodo^.Derecho);
  SetLength(Nodo^.Mensajes, 0);
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
