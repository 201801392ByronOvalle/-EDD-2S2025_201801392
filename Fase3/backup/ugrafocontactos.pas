unit UGrafoContactos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// estructuras y tipos para el grafo

type
  // Puntero al nodo del grafo (usuario)
  PNodoGrafo = ^TNodoGrafo;

  // Registro que representa cada usuario
  TNodoGrafo = record
    ID: Integer;
    Nombre: string;
    Usuario: string;
    Adyacentes: array of PNodoGrafo; // Lista de contactos (nodos relacionados)
  end;

  // Clase principal grafo no dirigido
  TGrafoContactos = class
  private
    FNodos: array of PNodoGrafo;
    FCount: Integer;

    // Busca un nodo dentro del grafo usando el nombre de usuario
    function BuscarNodoPorUsuario(Usuario: string): PNodoGrafo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AgregarUsuario(ID: Integer; Nombre, Usuario: string);

    procedure AgregarContacto(Usuario1, Usuario2: string);

    function ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;

    // pemdientes:
    procedure LimpiarGrafo;
    function ToString: string;

    property Count: Integer read FCount;
  end;

var
  GrafoContactosGlobal: TGrafoContactos;

implementation

// Constructor: inicializa el grafo
constructor TGrafoContactos.Create;
begin
  inherited Create;
  SetLength(FNodos, 0);
  FCount := 0;
end;

// Destructor: limpia el grafo al destruir el objeto
destructor TGrafoContactos.Destroy;
begin
  LimpiarGrafo;
  inherited Destroy;
end;

// AgregarUsuario: Crea un nuevo nodo (usuario)
procedure TGrafoContactos.AgregarUsuario(ID: Integer; Nombre, Usuario: string);
var
  NuevoNodo: PNodoGrafo;
begin
  // Evitar duplicados
  if BuscarNodoPorUsuario(Usuario) <> nil then Exit;

  // Crear nodo dinámicamente
  New(NuevoNodo);
  NuevoNodo^.ID := ID;
  NuevoNodo^.Nombre := Nombre;
  NuevoNodo^.Usuario := Usuario;
  SetLength(NuevoNodo^.Adyacentes, 0);

  // Agregar a la lista general
  SetLength(FNodos, Length(FNodos) + 1);
  FNodos[High(FNodos)] := NuevoNodo;
  Inc(FCount);
end;

// AgregarContacto: conecta dos usuarios
procedure TGrafoContactos.AgregarContacto(Usuario1, Usuario2: string);
var
  Nodo1, Nodo2: PNodoGrafo;
begin
  Nodo1 := BuscarNodoPorUsuario(Usuario1);
  Nodo2 := BuscarNodoPorUsuario(Usuario2);

  if (Nodo1 = nil) or (Nodo2 = nil) then Exit;

  // Agregar la relación de ambos lados (no dirigido)
  SetLength(Nodo1^.Adyacentes, Length(Nodo1^.Adyacentes) + 1);
  Nodo1^.Adyacentes[High(Nodo1^.Adyacentes)] := Nodo2;

  SetLength(Nodo2^.Adyacentes, Length(Nodo2^.Adyacentes) + 1);
  Nodo2^.Adyacentes[High(Nodo2^.Adyacentes)] := Nodo1;
end;

// BuscarNodoPorUsuario: busca en el arreglo de nodos
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

// ObtenerNodoPorUsuario: simplemente llama al buscador
function TGrafoContactos.ObtenerNodoPorUsuario(Usuario: string): PNodoGrafo;
begin
  Result := BuscarNodoPorUsuario(Usuario);
end;


// Falta por implementar: LimpiarGrafo y ToString
initialization
  GrafoContactosGlobal := TGrafoContactos.Create;

finalization
  if Assigned(GrafoContactosGlobal) then
    GrafoContactosGlobal.Free;

end.
