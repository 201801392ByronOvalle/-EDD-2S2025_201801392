unit EDDEstructuras;

{$mode objfpc}{$H+}

interface

uses                 // lib de JSON de pascal
  Classes, SysUtils,Dialogs, fpjson, jsonparser;

type
  // Registro para almacenar datos del usuario
  TUsuario = record
    id: Integer;
    nombre: string;
    usuario: string;
    email: string;
    telefono: string;
    contrasenia: string;
  end;
  PUsuario = ^TUsuario;

  // Nodo de la lista simple
  TNodoUsuario = record
    dato: PUsuario;
    siguiente: ^TNodoUsuario;
  end;
  PNodoUsuario = ^TNodoUsuario;

  // Lista simple de usuarios
  TListaUsuarios = record
    cabeza: PNodoUsuario;
    tamanio: Integer;
  end;
  PListaUsuarios = ^TListaUsuarios;

// Operaciones de la lista
procedure InicializarListaUsuarios(var lista: TListaUsuarios);
function CrearUsuario(id: Integer;
  nombre, usuario, email, telefono, contrasenia: string): PUsuario;
procedure InsertarUsuario(var lista: TListaUsuarios; usuario: PUsuario);
function BuscarUsuarioPorEmail(lista: TListaUsuarios;
  email: string): PUsuario;
function ValidarLogin(lista: TListaUsuarios;
  email, contrasenia: string): Boolean;
procedure MostrarUsuarios(lista: TListaUsuarios);

// Para carga Masiva (root/admin)
function CargarUsuariosDesdeJSON(var lista: TListaUsuarios;
  const filename: string): Boolean;

// Función auxiliar para leer archivo a string
function ReadFileToString(const filename: string): string;

//Funcion para generar el reporte de usuario
function GenerarReporteUsuariosGraphviz(lista: TListaUsuarios;
  const filename: string): Boolean;

// para buscar mi usuario al actualizar
function BuscarUsuarioPorUsuario(lista: TListaUsuarios; usuario: string): PUsuario;

implementation

procedure InicializarListaUsuarios(var lista: TListaUsuarios);
begin
  lista.cabeza := nil;
  lista.tamanio := 0;
end;

function CrearUsuario(id: Integer; nombre, usuario, email, telefono,
  contrasenia: string): PUsuario;
var
  nuevoUsuario: PUsuario;
begin
  New(nuevoUsuario);
  nuevoUsuario^.id := id;
  nuevoUsuario^.nombre := nombre;
  nuevoUsuario^.usuario := usuario;
  nuevoUsuario^.email := email;
  nuevoUsuario^.telefono := telefono;
  nuevoUsuario^.contrasenia := contrasenia;
  Result := nuevoUsuario;
end;

procedure InsertarUsuario(var lista: TListaUsuarios; usuario: PUsuario);
var
  nuevoNodo, actual: PNodoUsuario;
begin
  New(nuevoNodo);
  nuevoNodo^.dato := usuario;
  nuevoNodo^.siguiente := nil;

  if lista.cabeza = nil then
    lista.cabeza := nuevoNodo
  else
  begin
    actual := lista.cabeza;
    while actual^.siguiente <> nil do
      actual := actual^.siguiente;
    actual^.siguiente := nuevoNodo;
  end;
  Inc(lista.tamanio);
end;

function BuscarUsuarioPorEmail(lista: TListaUsuarios; email: string): PUsuario;
var
  actual: PNodoUsuario;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    if actual^.dato^.email = email then
      Exit(actual^.dato);
    actual := actual^.siguiente;
  end;
  Result := nil;
end;

function BuscarUsuarioPorUsuario(lista: TListaUsuarios; usuario: string): PUsuario;
var
  actual: PNodoUsuario;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    if actual^.dato^.usuario = usuario then
      Exit(actual^.dato);
    actual := actual^.siguiente;
  end;
  Result := nil;
end;

function ValidarLogin(lista: TListaUsuarios; email,
  contrasenia: string): Boolean;
var
  usuario: PUsuario;
begin
  usuario := BuscarUsuarioPorEmail(lista, email);
  if (usuario <> nil) and (usuario^.contrasenia = contrasenia) then
    Result := True
  else
    Result := False;
end;

procedure MostrarUsuarios(lista: TListaUsuarios);
var
  actual: PNodoUsuario;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    WriteLn('ID: ', actual^.dato^.id);
    WriteLn('Nombre: ', actual^.dato^.nombre);
    WriteLn('Usuario: ', actual^.dato^.usuario);
    WriteLn('Email: ', actual^.dato^.email);
    WriteLn('Teléfono: ', actual^.dato^.telefono);
    WriteLn('---');
    actual := actual^.siguiente;
  end;
end;

function CargarUsuariosDesdeJSON(var lista: TListaUsuarios;
  const filename: string): Boolean;
var
  jsonData: TJSONData;
  jsonObject, usuarioObj: TJSONObject; // Declarar usuarioObj aquí
  jsonArray: TJSONArray;
  i: Integer;
  nuevoUsuario: PUsuario;
begin
  Result := False;

  // Verificar primero si existe el JSON
  if not FileExists(filename) then
  begin
    ShowMessage('El archivo ' + filename + ' no existe');
    Exit;
  end;

  try
    try
      // Leer y parsear el JSON
      jsonData := GetJSON(ReadFileToString(filename));

      // Verificar la estructura del JSON
      if (jsonData.JSONType = jtObject) and (TJSONObject(jsonData).Find('usuarios') <> nil) then
      begin
        jsonObject := TJSONObject(jsonData);
        jsonArray := jsonObject.Get('usuarios', TJSONArray(nil));

        if jsonArray = nil then
        begin
          ShowMessage('Formato JSON inválido: no se encontró el array "usuarios"');
          Exit;
        end;

        for i := 0 to jsonArray.Count - 1 do
        begin
          if jsonArray.Items[i].JSONType = jtObject then
          begin
            usuarioObj := TJSONObject(jsonArray.Items[i]);

            // Crear usuario desde JSON
            nuevoUsuario := CrearUsuario(
              usuarioObj.Get('id', 0),
              usuarioObj.Get('nombre', ''),
              usuarioObj.Get('usuario', ''),
              usuarioObj.Get('email', ''),
              usuarioObj.Get('telefono', ''),
              usuarioObj.Get('password', '')
            );

            // Verificar si el usuario ya existe
            if BuscarUsuarioPorEmail(lista, nuevoUsuario^.email) = nil then
            begin
              InsertarUsuario(lista, nuevoUsuario);
            end
            else
            begin
              ShowMessage('Usuario con email ' + nuevoUsuario^.email + ' ya existe. Se omitió.');
              Dispose(nuevoUsuario);
            end;
          end;
        end;

        Result := True;
        ShowMessage('Se cargaron ' + IntToStr(jsonArray.Count) + ' usuarios exitosamente');
      end
      else
      begin
        ShowMessage('Formato JSON inválido. Se esperaba un objeto con array "usuarios"');
      end;

    except
      on E: Exception do
      begin
        ShowMessage('Error al cargar JSON: ' + E.Message);
      end;
    end;
  finally
    if Assigned(jsonData) then
      jsonData.Free;
  end;
end;

// Función auxiliar para leer archivo a string
function ReadFileToString(const filename: string): string;
var
  fileStream: TFileStream;
  stringStream: TStringStream;
begin
  Result := '';
  fileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    stringStream := TStringStream.Create('');
    try
      stringStream.CopyFrom(fileStream, fileStream.Size);
      Result := stringStream.DataString;
    finally
      stringStream.Free;
    end;
  finally
    fileStream.Free;
  end;
end;

function GenerarReporteUsuariosGraphviz(lista: TListaUsuarios;
  const filename: string): Boolean;
var
  archivo: TextFile;
  actual: PNodoUsuario;
begin
  Result := False;

  if lista.cabeza = nil then
  begin
    ShowMessage('No hay usuarios para generar el reporte');
    Exit;
  end;

  try
    AssignFile(archivo, filename);
    Rewrite(archivo);

    // Encabezado del archivo DOT para Graphviz
    WriteLn(archivo, 'digraph ListaUsuarios {');
    WriteLn(archivo, '  rankdir=LR;'); // Dirección izquierda a derecha
    WriteLn(archivo, '  node [shape=record, style=filled, fillcolor=lightblue, fontname=Arial, fontsize=10];');
    WriteLn(archivo, '  edge [arrowhead=vee, color=blue];');
    WriteLn(archivo, '  graph [bgcolor=transparent];');
    WriteLn(archivo, '');

    // Generar todos los nodos primero
    actual := lista.cabeza;
    while actual <> nil do
    begin
      WriteLn(archivo, '  node', actual^.dato^.id, ' [label="');
      WriteLn(archivo, '    ID: ', actual^.dato^.id, '\n');
      WriteLn(archivo, '    Nombre: ', actual^.dato^.nombre, '\n');
      WriteLn(archivo, '    Usuario: ', actual^.dato^.usuario, '\n');
      WriteLn(archivo, '    Email: ', actual^.dato^.email, '\n');
      WriteLn(archivo, '    Teléfono: ', actual^.dato^.telefono);
      WriteLn(archivo, '  "];');

      actual := actual^.siguiente;
    end;

    WriteLn(archivo, '');

    // Generar conexiones entre nodos
    actual := lista.cabeza;
    while actual <> nil do
    begin
      if actual^.siguiente <> nil then
      begin
        WriteLn(archivo, '  node', actual^.dato^.id, ' -> node', actual^.siguiente^.dato^.id, ';');
      end;
      actual := actual^.siguiente;
    end;

    WriteLn(archivo, '}');
    CloseFile(archivo);

    Result := True;
    ShowMessage('Reporte generado exitosamente: ' + filename);

  except
    on E: Exception do
    begin
      ShowMessage('Error al generar reporte: ' + E.Message);
    end;
  end;
end;

end.
