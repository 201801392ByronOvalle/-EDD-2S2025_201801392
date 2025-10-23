unit UGrafoContactos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// estructuras y tipos para el grafo

type
  // Puntero al nodo del grafo (usuario)
  PNodoGrafo = ^TNodoGrafo;

  // Registro de nodo de usuario
  TNodoGrafo = record
    ID: Integer;
    Nombre: string;
    Usuario: string;
    Adyacentes: array of PNodoGrafo;
  end;

  // Clase principal del grafo no dirigido
  TGrafoContactos = class
  private
    FNodos: array of PNodoGrafo;  // Conjunto de nodos
    FCount: Integer;              // Número total de usuarios

    function BuscarNodoPorUsuario(Usuario: string): PNodoGrafo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AgregarUsuario(ID: Integer; Nombre, Usuario: string);
    procedure AgregarContacto(Usuario1, Usuario2: string);
    function ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;
    procedure LimpiarGrafo;
    function ToString: string;

    property Count: Integer read FCount;
  end;

// Instancia global del grafo
var
  GrafoContactosGlobal: TGrafoContactos;

implementation

// Constructor
constructor TGrafoContactos.Create;
begin
  inherited Create;
  SetLength(FNodos, 0);
  FCount := 0;
end;

destructor TGrafoContactos.Destroy;
begin
  LimpiarGrafo;
  inherited Destroy;
end;

// AgregarUsuario: añade un nuevo nodo al grafo
procedure TGrafoContactos.AgregarUsuario(ID: Integer; Nombre, Usuario: string);
var
  NuevoNodo: PNodoGrafo;
begin
  if BuscarNodoPorUsuario(Usuario) <> nil then Exit;

  New(NuevoNodo);
  NuevoNodo^.ID := ID;
  NuevoNodo^.Nombre := Nombre;
  NuevoNodo^.Usuario := Usuario;
  SetLength(NuevoNodo^.Adyacentes, 0);

  SetLength(FNodos, Length(FNodos) + 1);
  FNodos[High(FNodos)] := NuevoNodo;
  Inc(FCount);
end;

// AgregarContacto: crea una relación bidireccional
procedure TGrafoContactos.AgregarContacto(Usuario1, Usuario2: string);
var
  Nodo1, Nodo2: PNodoGrafo;
begin
  Nodo1 := BuscarNodoPorUsuario(Usuario1);
  Nodo2 := BuscarNodoPorUsuario(Usuario2);

  if (Nodo1 = nil) or (Nodo2 = nil) then Exit;

  SetLength(Nodo1^.Adyacentes, Length(Nodo1^.Adyacentes) + 1);
  Nodo1^.Adyacentes[High(Nodo1^.Adyacentes)] := Nodo2;

  SetLength(Nodo2^.Adyacentes, Length(Nodo2^.Adyacentes) + 1);
  Nodo2^.Adyacentes[High(Nodo2^.Adyacentes)] := Nodo1;
end;

// BuscarNodoPorUsuario: devuelve el puntero del nodo
function TGrafoContactos.BuscarNodoPorUsuario(Usuario: string): PNodoGrafo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FNodos) do
    if FNodos[I]^.Usuario = Usuario then
      Exit(FNodos[I]);
end;

// ObtenerNodoPorUsuario: wrapper del buscador interno
function TGrafoContactos.ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;
begin
  Result := BuscarNodoPorUsuario(Usuario);
end;

// LimpiarGrafo: libera toda la memoria del grafo
procedure TGrafoContactos.LimpiarGrafo;
var
  I: Integer;
begin
  for I := 0 to High(FNodos) do
  begin
    SetLength(FNodos[I]^.Adyacentes, 0);
    Dispose(FNodos[I]);
  end;
  SetLength(FNodos, 0);
  FCount := 0;
end;

// ToString: devuelve una representación legible del grafo
function TGrafoContactos.ToString: string;
var
  I, J: Integer;
begin
  Result := 'Grafo de Contactos (' + IntToStr(FCount) + ' usuarios):' + sLineBreak;

  for I := 0 to High(FNodos) do
  begin
    Result := Result + FNodos[I]^.Usuario + ' -> ';
    if Length(FNodos[I]^.Adyacentes) > 0 then
    begin
      for J := 0 to High(FNodos[I]^.Adyacentes) do
      begin
        Result := Result + FNodos[I]^.Adyacentes[J]^.Usuario;
        if J < High(FNodos[I]^.Adyacentes) then
          Result := Result + ', ';
      end;
    end
    else
      Result := Result + 'Sin contactos';
    Result := Result + sLineBreak;
  end;
end;

initialization
  GrafoContactosGlobal := TGrafoContactos.Create;

finalization
  if Assigned(GrafoContactosGlobal) then
    GrafoContactosGlobal.Free;

end.

