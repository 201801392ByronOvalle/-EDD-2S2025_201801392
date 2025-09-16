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
  actual, siguiente: PNodoContacto;
  i: Integer;
begin
  Result := False;
  if lista.cabeza = nil then
  begin
    ShowMessage('No hay contactos para generar el reporte');
    Exit;
  end;

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
    i := 1;
    repeat
      WriteLn(archivo, '  node', i, ' [label="');
      WriteLn(archivo, '    Nombre: ', actual^.usuario^.nombre, '\n');
      WriteLn(archivo, '    Usuario: ', actual^.usuario^.usuario, '\n');
      WriteLn(archivo, '    Email: ', actual^.usuario^.email, '\n');
      WriteLn(archivo, '    Teléfono: ', actual^.usuario^.telefono);
      WriteLn(archivo, '  "];');

      actual := actual^.siguiente;
      Inc(i);
    until actual = lista.cabeza;

    WriteLn(archivo, '');
    WriteLn(archivo, '  // Conexiones bidireccionales entre nodos consecutivos');

    // Generar conexiones bidireccionales
    actual := lista.cabeza;
    i := 1;
    repeat
      siguiente := actual^.siguiente;

      // Si no es el último nodo, conectar con el siguiente
      if siguiente <> lista.cabeza then
      begin
        WriteLn(archivo, '  node', i, ' -> node', i + 1, ';');
        WriteLn(archivo, '  node', i + 1, ' -> node', i, ';');
      end
      else
      begin
        // Conexión circular del último al primero
        WriteLn(archivo, '  node', i, ' -> node1;');
        WriteLn(archivo, '  node1 -> node', i, ';');
      end;

      actual := actual^.siguiente;
      Inc(i);
    until actual = lista.cabeza;

    WriteLn(archivo, '}');
    CloseFile(archivo);

    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Error al generar reporte: ' + E.Message);
    end;
  end;
end;

end.
