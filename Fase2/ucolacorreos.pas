unit UColaCorreos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Definición del registro de correo
  TDatoCorreo = record
    Id: Integer;
    Remitente: string;
    Destinatario: string;
    Estado: string; // 'NL' para No Leído, 'L' para Leído
    Asunto: string;
    Mensaje: string;
  end;

  // Definición del nodo de la cola
  PNodoCola = ^TNodoCola;
  TNodoCola = record
    Dato: TDatoCorreo;
    Siguiente: PNodoCola;
  end;

  // Clase de la cola de correos
  TColaCorreos = class
  private
    FFrente: PNodoCola;
    FFinal: PNodoCola;
    FCount: Integer;
    procedure Inicializar;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Encolar(Id: Integer; Remitente, Destinatario, Estado, Asunto, Mensaje: string);
    function Desencolar: TDatoCorreo;
    function Frente: TDatoCorreo;
    function EstaVacia: Boolean;
    procedure LimpiarCola;
    function ToString: string; // Para debugging
    property Count: Integer read FCount;
  end;

// Declarar la variable global para la cola de correos
var
  ColaCorreosGlobal: TColaCorreos;

implementation

constructor TColaCorreos.Create;
begin
  inherited Create;
  Inicializar;
end;

destructor TColaCorreos.Destroy;
begin
  LimpiarCola;
  inherited Destroy;
end;

procedure TColaCorreos.Inicializar;
begin
  FFrente := nil;
  FFinal := nil;
  FCount := 0;
end;

procedure TColaCorreos.Encolar(Id: Integer; Remitente, Destinatario, Estado, Asunto, Mensaje: string);
var
  NuevoNodo: PNodoCola;
begin
  // Crear nuevo nodo
  New(NuevoNodo);

  // Asignar datos al nuevo nodo
  NuevoNodo^.Dato.Id := Id;
  NuevoNodo^.Dato.Remitente := Remitente;
  NuevoNodo^.Dato.Destinatario := Destinatario;
  NuevoNodo^.Dato.Estado := Estado;
  NuevoNodo^.Dato.Asunto := Asunto;
  NuevoNodo^.Dato.Mensaje := Mensaje;
  NuevoNodo^.Siguiente := nil;

  // Si la cola está vacía
  if EstaVacia then
  begin
    FFrente := NuevoNodo;
    FFinal := NuevoNodo;
  end
  else
  begin
    // Agregar al final de la cola
    FFinal^.Siguiente := NuevoNodo;
    FFinal := NuevoNodo;
  end;

  Inc(FCount);
end;

function TColaCorreos.Desencolar: TDatoCorreo;
var
  Temp: PNodoCola;
begin
  // Inicializar resultado por si la cola está vacía
  Result.Id := -1;
  Result.Remitente := '';
  Result.Destinatario := '';
  Result.Estado := '';
  Result.Asunto := '';
  Result.Mensaje := '';

  if EstaVacia then Exit;

  // Obtener el dato del frente
  Result := FFrente^.Dato;

  // Eliminar el nodo del frente
  Temp := FFrente;
  FFrente := FFrente^.Siguiente;

  // Si la cola queda vacía
  if FFrente = nil then
    FFinal := nil;

  Dispose(Temp);
  Dec(FCount);
end;

function TColaCorreos.Frente: TDatoCorreo;
begin
  // Inicializar resultado por si la cola está vacía
  Result.Id := -1;
  Result.Remitente := '';
  Result.Destinatario := '';
  Result.Estado := '';
  Result.Asunto := '';
  Result.Mensaje := '';

  if not EstaVacia then
    Result := FFrente^.Dato;
end;

function TColaCorreos.EstaVacia: Boolean;
begin
  Result := (FFrente = nil);
end;

procedure TColaCorreos.LimpiarCola;
begin
  while not EstaVacia do
    Desencolar;
end;

function TColaCorreos.ToString: string;
var
  Actual: PNodoCola;
  i: Integer;
begin
  Result := 'Cola de Correos (' + IntToStr(FCount) + ' elementos):' + sLineBreak;

  Actual := FFrente;
  i := 1;
  while Actual <> nil do
  begin
    Result := Result + Format('%d. [ID:%d] %s -> %s: %s' + sLineBreak,
      [i, Actual^.Dato.Id, Actual^.Dato.Remitente, Actual^.Dato.Destinatario, Actual^.Dato.Asunto]);
    Actual := Actual^.Siguiente;
    Inc(i);
  end;

  if EstaVacia then
    Result := Result + 'COLA VACÍA';
end;

// Inicializar la variable global
initialization
  ColaCorreosGlobal := TColaCorreos.Create;

finalization
  if Assigned(ColaCorreosGlobal) then
    ColaCorreosGlobal.Free;

end.
