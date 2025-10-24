unit UGrafoContactos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Nodo del grafo para usuarios
  PNodoGrafo = ^TNodoGrafo;
  TNodoGrafo = record
    ID: Integer;
    Nombre: string;
    Usuario: string;
    Adyacentes: array of PNodoGrafo; // Lista de contactos
  end;

  // Clase del grafo no dirigido
  TGrafoContactos = class
  private
    FNodos: array of PNodoGrafo;
    FCount: Integer;
    function BuscarNodoPorUsuario(Usuario: string): PNodoGrafo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AgregarUsuario(ID: Integer; Nombre, Usuario: string);
    procedure AgregarContacto(Usuario1, Usuario2: string);
    function ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;
    procedure LimpiarGrafo;
    function ToStringGrafo: string; // Para debugging
    function GenerarReporteGraphviz: string;
    property Count: Integer read FCount;
  end;

var
  GrafoContactosGlobal: TGrafoContactos;

implementation

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

procedure TGrafoContactos.AgregarUsuario(ID: Integer; Nombre, Usuario: string);
var
  NuevoNodo: PNodoGrafo;
begin
  // Verificar si el usuario ya existe
  if BuscarNodoPorUsuario(Usuario) <> nil then Exit;

  // Crear nuevo nodo
  New(NuevoNodo);
  NuevoNodo^.ID := ID;
  NuevoNodo^.Nombre := Nombre;
  NuevoNodo^.Usuario := Usuario;
  SetLength(NuevoNodo^.Adyacentes, 0);

  // Agregar al grafo
  SetLength(FNodos, Length(FNodos) + 1);
  FNodos[High(FNodos)] := NuevoNodo;
  Inc(FCount);
end;

procedure TGrafoContactos.AgregarContacto(Usuario1, Usuario2: string);
var
  Nodo1, Nodo2: PNodoGrafo;
  I: Integer;
begin
  Nodo1 := BuscarNodoPorUsuario(Usuario1);
  Nodo2 := BuscarNodoPorUsuario(Usuario2);

  if (Nodo1 = nil) or (Nodo2 = nil) then Exit;

  // Verificar si ya existe la conexión
  for I := 0 to High(Nodo1^.Adyacentes) do
    if Nodo1^.Adyacentes[I] = Nodo2 then Exit;

  // Agregar Usuario2 a la lista de contactos de Usuario1
  SetLength(Nodo1^.Adyacentes, Length(Nodo1^.Adyacentes) + 1);
  Nodo1^.Adyacentes[High(Nodo1^.Adyacentes)] := Nodo2;

  // Agregar Usuario1 a la lista de contactos de Usuario2 (grafo no dirigido)
  SetLength(Nodo2^.Adyacentes, Length(Nodo2^.Adyacentes) + 1);
  Nodo2^.Adyacentes[High(Nodo2^.Adyacentes)] := Nodo1;
end;

function TGrafoContactos.BuscarNodoPorUsuario(Usuario: string): PNodoGrafo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FNodos) do
  begin
    if FNodos[I]^.Usuario = Usuario then
    begin
      Result := FNodos[I];
      Exit;
    end;
  end;
end;

function TGrafoContactos.ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;
begin
  Result := BuscarNodoPorUsuario(Usuario);
end;

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

function TGrafoContactos.ToStringGrafo: string;
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
    begin
      Result := Result + 'Sin contactos';
    end;
    Result := Result + sLineBreak;
  end;
end;

function TGrafoContactos.GenerarReporteGraphviz: string;
var
  I, J: Integer;
  Conexiones: TStringList;
begin
  Result := 'graph G {' + sLineBreak;
  Result := Result + '  rankdir=LR;' + sLineBreak;
  Result := Result + '  node [shape=ellipse, style=filled, color=lightblue];' + sLineBreak;
  Result := Result + '  edge [color=gray];' + sLineBreak + sLineBreak;

  // Agregar nodos con ID y Nombre
  for I := 0 to High(FNodos) do
  begin
    Result := Result + '  "' + FNodos[I]^.Usuario + '" [label="ID: ' + IntToStr(FNodos[I]^.ID) +
               '\n' + FNodos[I]^.Nombre + '\n' + FNodos[I]^.Usuario + '"];' + sLineBreak;
  end;

  Result := Result + sLineBreak;

  // Agregar conexiones (evitando duplicados)
  Conexiones := TStringList.Create;
  try
    for I := 0 to High(FNodos) do
    begin
      for J := 0 to High(FNodos[I]^.Adyacentes) do
      begin
        // Evitar conexiones duplicadas en grafos no dirigidos
        if (FNodos[I]^.Usuario < FNodos[I]^.Adyacentes[J]^.Usuario) then
        begin
          Conexiones.Add('  "' + FNodos[I]^.Usuario + '" -- "' +
                         FNodos[I]^.Adyacentes[J]^.Usuario + '";');
        end;
      end;
    end;

    for I := 0 to Conexiones.Count - 1 do
    begin
      Result := Result + Conexiones[I] + sLineBreak;
    end;
  finally
    Conexiones.Free;
  end;

  Result := Result + '}' + sLineBreak;
end;

initialization
  GrafoContactosGlobal := TGrafoContactos.Create;

finalization
  if Assigned(GrafoContactosGlobal) then
    GrafoContactosGlobal.Free;

end.
