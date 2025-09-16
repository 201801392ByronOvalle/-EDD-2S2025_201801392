unit EDDCorreos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Dialogs;

type
  TCorreo = record
    id: Integer;
    remitente: string;
    destinatario: string;
    asunto: string;
    mensaje: string;
    fechaHoraProgramada: TDateTime;
    estado: string;
  end;
// Estructura de la cola
PCorreo = ^TCorreo;

// Nodo para cola de correos programados
PNodoCorreo = ^TNodoCorreo;
TNodoCorreo = record
  dato: PCorreo;
  siguiente: PNodoCorreo;
end;

// Cola para correos programados (FIFO) primero en entrar primero en salir
TColaCorreos = record
  frente, final: PNodoCorreo;
  tamanio: Integer;
end;

  // Operaciones de cola de correos
procedure InicializarColaCorreos(var cola: TColaCorreos);
function CrearCorreo(id: Integer; remitente, destinatario, asunto, mensaje: string; fechaHora: TDateTime): PCorreo;
procedure EncolarCorreo(var cola: TColaCorreos; correo: PCorreo);
function DesencolarCorreo(var cola: TColaCorreos): PCorreo;
function ColaVacia(cola: TColaCorreos): Boolean;
procedure MostrarColaCorreos(cola: TColaCorreos);
function GenerarReporteCorreosProgramadosGraphviz(cola: TColaCorreos; const filename: string): Boolean;

implementation

procedure InicializarColaCorreos(var cola: TColaCorreos);
begin
  cola.frente := nil;
  cola.final := nil;
  cola.tamanio := 0;
end;

function CrearCorreo(id: Integer; remitente, destinatario, asunto, mensaje: string; fechaHora: TDateTime): PCorreo;
var
  nuevoCorreo: PCorreo;
begin
  New(nuevoCorreo);
  nuevoCorreo^.id := id;
  nuevoCorreo^.remitente := remitente;
  nuevoCorreo^.destinatario := destinatario;
  nuevoCorreo^.asunto := asunto;
  nuevoCorreo^.mensaje := mensaje;
  nuevoCorreo^.fechaHoraProgramada := fechaHora;
  nuevoCorreo^.estado := 'P'; // Programado
  Result := nuevoCorreo;
end;

procedure EncolarCorreo(var cola: TColaCorreos; correo: PCorreo);
var
  nuevoNodo: PNodoCorreo;
begin
  New(nuevoNodo);
  nuevoNodo^.dato := correo;
  nuevoNodo^.siguiente := nil;

  if ColaVacia(cola) then
    cola.frente := nuevoNodo
  else
    cola.final^.siguiente := nuevoNodo;

  cola.final := nuevoNodo;
  Inc(cola.tamanio);
end;

function DesencolarCorreo(var cola: TColaCorreos): PCorreo;
var
  nodoEliminar: PNodoCorreo;
begin
  if ColaVacia(cola) then
    Exit(nil);

  Result := cola.frente^.dato;
  nodoEliminar := cola.frente;
  cola.frente := cola.frente^.siguiente;

  if cola.frente = nil then
    cola.final := nil;

  Dispose(nodoEliminar);
  Dec(cola.tamanio);
end;

function ColaVacia(cola: TColaCorreos): Boolean;
begin
  Result := (cola.frente = nil);
end;

procedure MostrarColaCorreos(cola: TColaCorreos);
var
  actual: PNodoCorreo;
begin
  actual := cola.frente;
  WriteLn('=== COLA DE CORREOS PROGRAMADOS ===');
  WriteLn('Tamaño: ', cola.tamanio);

  while actual <> nil do
  begin
    WriteLn('ID: ', actual^.dato^.id);
    WriteLn('Asunto: ', actual^.dato^.asunto);
    WriteLn('Destinatario: ', actual^.dato^.destinatario);
    WriteLn('Programado para: ', DateTimeToStr(actual^.dato^.fechaHoraProgramada));
    WriteLn('---');
    actual := actual^.siguiente;
  end;
end;

function GenerarReporteCorreosProgramadosGraphviz(cola: TColaCorreos; const filename: string): Boolean;
var
  archivo: TextFile;
  actual: PNodoCorreo;
  i: Integer;
  estadoTexto: string;
begin
  Result := False;
  if ColaVacia(cola) then Exit;

  try
    AssignFile(archivo, filename);
    Rewrite(archivo);

    WriteLn(archivo, 'digraph ColaCorreosProgramados {');
    WriteLn(archivo, '  rankdir=TB;'); // Top to Bottom (cola en vertical)
    WriteLn(archivo, '  node [shape=box, style=filled, fillcolor=lightyellow, fontname=Arial, fontsize=10, width=3];');
    WriteLn(archivo, '  edge [arrowhead=vee, color=orange];');
    WriteLn(archivo, '  graph [bgcolor=transparent, label="Cola de Correos Programados (FIFO)", labelloc=top];');
    WriteLn(archivo, '');

    // Generar nodos
    actual := cola.frente;
    i := 1;
    while actual <> nil do
    begin
      // Convertir estado a texto legible
      if actual^.dato^.estado = 'P' then
        estadoTexto := 'Sí'
      else
        estadoTexto := 'No';

      WriteLn(archivo, '  node', i, ' [label="',
        'ID: ', actual^.dato^.id, '\n',
        'Destinatario: ', actual^.dato^.destinatario, '\n',
        'Asunto: ', actual^.dato^.asunto, '\n',
        'Fecha: ', FormatDateTime('yyyy-mm-dd hh:nn', actual^.dato^.fechaHoraProgramada), '\n',
        'Programado: ', estadoTexto, '\n',
        'Mensaje: ', Copy(actual^.dato^.mensaje, 1, 30), '...',
        '"];');

      actual := actual^.siguiente;
      Inc(i);
    end;

    WriteLn(archivo, '');

    // Generar flechas hacia abajo
    for i := 1 to cola.tamanio - 1 do
    begin
      WriteLn(archivo, '  node', i, ' -> node', i + 1, ';');
    end;

    WriteLn(archivo, '}');
    CloseFile(archivo);

    Result := True;
  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

end.
