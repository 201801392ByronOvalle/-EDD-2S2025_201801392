unit EDDCorreos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

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

end.

