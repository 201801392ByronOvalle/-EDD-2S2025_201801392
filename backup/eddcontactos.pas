unit EDDContactos;

{$mode ObjFPC}{$H+}

interface

uses                          // Importar la lista simple(los usuarios)
  Classes, SysUtils, Dialogs, EDDEstructuras;

type
  // NOdo para la lista circular de contactos
  PNodoContacto = ^TNodoContacto;
  TNodoContacto = record
    usuario: PUsuario; // Puntero al usuario de la lista simple
    siguiente: PNodoContacto
  end;

  // Lista circular de contactos
  TListaContactos = record
    cabeza: PNodoContacto;
    tamanio: Integer;
  end;

// OPeraciones de la lista circular
procedure InicializarListaContactos(var lista: TListaContactos);
procedure InsertarContacto(var lista: TListaContactos; usuario: PUsuario);
function BuscarContactoPorEmail(lista: TListaContactos;
  email: string): PNodoContacto;
procedure MostrarContactos(lista: TListaContactos);
function GenerarReporteContactosGraphviz(lista: TListaContactos;
  const filename: string): Boolean;

implementation

procedure InicializarListaContactos(var lista: TListaContactos);
begin
  lista.cabeza := nil;
  lista.tamanio := 0;
end;

procedure InsertarContacto(var lista: TListaContactos; usuario: PUsuario);
var
  nuevoNodo, actual: PNodoContacto;
begin
  // Verificar si el contacto ya existe
  if BuscarContactoPorEmail(lista, usuario^.email) <> nil then
    Exit;

  New(nuevoNodo);
  nuevoNodo^.usuario := usuario;

  if lista.cabeza = nil then
  begin
    // Primer nodo - se apunta a sí mismo
    nuevoNodo^.siguiente := nuevoNodo;
    lista.cabeza := nuevoNodo;
  end
  else
  begin
    // Insertar al final de la lista circular
    actual := lista.cabeza;
    while actual^.siguiente <> lista.cabeza do
      actual := actual^.siguiente;

    actual^.siguiente := nuevoNodo;
    nuevoNodo^.siguiente := lista.cabeza;
  end;
  Inc(lista.tamanio);
end;

function BuscarContactoPorEmail(lista: TListaContactos; email: string): PNodoContacto;
var
  actual: PNodoContacto;
begin
  if lista.cabeza = nil then
    Exit(nil);

  actual := lista.cabeza;
  repeat
    if actual^.usuario^.email = email then
      Exit(actual);
    actual := actual^.siguiente;
  until actual = lista.cabeza;

  Result := nil;
end;

procedure MostrarContactos(lista: TListaContactos);
var
  actual: PNodoContacto;
  i: Integer;
begin
  if lista.cabeza = nil then
  begin
    WriteLn('Lista de contactos vacía');
    Exit;
  end;

  actual := lista.cabeza;
  i := 1;
  repeat
    WriteLn('Contacto ', i, ':');
    WriteLn('  Nombre: ', actual^.usuario^.nombre);
    WriteLn('  Nombre: ', actual^.usuario^.usuario);
    WriteLn('  Email: ', actual^.usuario^.email);
    WriteLn('  Teléfono: ', actual^.usuario^.telefono);
    WriteLn('  ---');
    actual := actual^.siguiente;
    Inc(i);
  until actual = lista.cabeza;
end;

function GenerarReporteContactosGraphviz(lista: TListaContactos; const filename: string): Boolean;
var
  archivo: TextFile;
  actual: PNodoContacto;
  i: Integer;
  primero: PNodoContacto;
begin
  Result := False;
  if lista.cabeza = nil then Exit;

  try
    AssignFile(archivo, filename);
    Rewrite(archivo);

    WriteLn(archivo, 'digraph ListaContactos {');
    WriteLn(archivo, '  rankdir=LR;');
    WriteLn(archivo, '  node [shape=record, style=filled, fillcolor=lightgreen, fontname=Arial, fontsize=10];');
    WriteLn(archivo, '  edge [arrowhead=vee, color=darkgreen];');
    WriteLn(archivo, '  graph [bgcolor=transparent];');
    WriteLn(archivo, '');

    // Generar nodos
    actual := lista.cabeza;
    primero := lista.cabeza;
    i := 1;
    repeat
      WriteLn(archivo, '  node', i, ' [label="{');
      WriteLn(archivo, '    Nombre: ', actual^.usuario^.nombre, ' |');
      WriteLn(archivo, '    Usuario: ', actual^.usuario^.usuario, ' |');
      WriteLn(archivo, '    Email: ', actual^.usuario^.email, ' |');
      WriteLn(archivo, '    Teléfono: ', actual^.usuario^.telefono);
      WriteLn(archivo, '  }"];');

      actual := actual^.siguiente;
      Inc(i);
    until actual = primero;

    WriteLn(archivo, '');

    // Generar conexiones circulares (ida y vuelta)
    actual := lista.cabeza;
    i := 1;
    repeat
      // flecha hacia adelante
      WriteLn(archivo, '  node', i, ' -> node', (i mod lista.tamanio) + 1, ';');
      // flecha hacia atrás
      WriteLn(archivo, '  node', (i mod lista.tamanio) + 1, ' -> node', i, ';');

      actual := actual^.siguiente;
      Inc(i);
    until actual = primero;

    WriteLn(archivo, '}');
    CloseFile(archivo);

    Result := True;
  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

end.
