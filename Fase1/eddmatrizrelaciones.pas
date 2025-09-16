unit EDDMatrizRelaciones;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, EDDCorreos;

type
  // Nodo para matriz dispersa
  PNodoCelda = ^TNodoCelda;
  TNodoCelda = record
    remitente: string;
    destinatario: string;
    cantidad: Integer;
    siguiente: PNodoCelda;
  end;

  // Matriz dispersa de relaciones
  TMatrizRelaciones = record
    cabeza: PNodoCelda;
    tamanio: Integer;
  end;

// Operaciones de matriz dispersa
procedure InicializarMatrizRelaciones(var matriz: TMatrizRelaciones);
procedure InsertarRelacion(var matriz: TMatrizRelaciones; remitente, destinatario: string);
function BuscarRelacion(matriz: TMatrizRelaciones; remitente, destinatario: string): PNodoCelda;
procedure GenerarReporteRelaciones(matriz: TMatrizRelaciones; const filename: string);
procedure ProcesarCorreosParaRelaciones(var matriz: TMatrizRelaciones; var colaCorreos: TColaCorreos); // Cambiar parámetro

implementation

procedure InicializarMatrizRelaciones(var matriz: TMatrizRelaciones);
begin
  matriz.cabeza := nil;
  matriz.tamanio := 0;
end;

procedure InsertarRelacion(var matriz: TMatrizRelaciones; remitente, destinatario: string);
var
  nodoExistente: PNodoCelda;
  nuevoNodo: PNodoCelda;
begin
  // Buscar si ya existe la relacion
  nodoExistente := BuscarRelacion(matriz, remitente, destinatario);

  if nodoExistente <> nil then
  begin
    // Incrementar contador si existe
    Inc(nodoExistente^.cantidad);
  end
  else
  begin
    // Crear nueva relacion
    New(nuevoNodo);
    nuevoNodo^.remitente := remitente;
    nuevoNodo^.destinatario := destinatario;
    nuevoNodo^.cantidad := 1;
    nuevoNodo^.siguiente := matriz.cabeza;
    matriz.cabeza := nuevoNodo;
    Inc(matriz.tamanio);
  end;
end;

function BuscarRelacion(matriz: TMatrizRelaciones; remitente, destinatario: string): PNodoCelda;
var
  actual: PNodoCelda;
begin
  actual := matriz.cabeza;
  while actual <> nil do
  begin
    if (actual^.remitente = remitente) and (actual^.destinatario = destinatario) then
      Exit(actual);
    actual := actual^.siguiente;
  end;
  Result := nil;
end;

procedure ProcesarCorreosParaRelaciones(var matriz: TMatrizRelaciones; var colaCorreos: TColaCorreos);
var
  actual: PNodoCorreo;
begin
  // Procesar correos programados
  actual := colaCorreos.frente;
  while actual <> nil do
  begin
    if actual^.dato^.estado = 'E' then // Solo correos enviados
    begin
      InsertarRelacion(matriz, actual^.dato^.remitente, actual^.dato^.destinatario);
    end;
    actual := actual^.siguiente;
  end;
end;

procedure GenerarReporteRelaciones(matriz: TMatrizRelaciones; const filename: string);
var
  archivo: TextFile;
  actual: PNodoCelda;
  remitentes, destinatarios: TStringList;
  i, j: Integer;
  filaNodos: array of TStringList;
begin
  if matriz.cabeza = nil then
  begin
    ShowMessage('No hay relaciones para generar el reporte');
    Exit;
  end;

  try
    AssignFile(archivo, filename);
    Rewrite(archivo);

    // Encabezado DOT
    WriteLn(archivo, 'digraph MatrizRelaciones {');
    WriteLn(archivo, '  node [shape=box, style=filled, fontname=Arial];');
    WriteLn(archivo, '  rankdir=LR;');
    WriteLn(archivo, '  graph [bgcolor=transparent, label="Matriz de Relaciones Remitente vs Destinatario"];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  R [label=" ", fillcolor=gray, width=0.5, height=0.5];');

    // Listas únicas de remitentes y destinatarios
    remitentes := TStringList.Create;
    destinatarios := TStringList.Create;
    remitentes.Sorted := True;
    remitentes.Duplicates := dupIgnore;
    destinatarios.Sorted := True;
    destinatarios.Duplicates := dupIgnore;

    actual := matriz.cabeza;
    while actual <> nil do
    begin
      remitentes.Add(actual^.remitente);
      destinatarios.Add(actual^.destinatario);
      actual := actual^.siguiente;
    end;

    // Crear un TStringList por cada fila para almacenar nodos existentes
    SetLength(filaNodos, remitentes.Count);
    for i := 0 to remitentes.Count - 1 do
      filaNodos[i] := TStringList.Create;

    // Nodos de destinatarios (arriba)
    WriteLn(archivo, '// Destinatarios (cabecera fila superior)');
    for j := 0 to destinatarios.Count - 1 do
      WriteLn(archivo, '  C', j, ' [label="', destinatarios[j], '", fillcolor=lightblue];');

    // Nodos de remitentes (izquierda)
    WriteLn(archivo, '');
    WriteLn(archivo, '// Remitentes (cabecera columna izquierda)');
    for i := 0 to remitentes.Count - 1 do
      WriteLn(archivo, '  F', i, ' [label="', remitentes[i], '", fillcolor=lightgreen];');

    // Celdas de relaciones y conexiones
    WriteLn(archivo, '');
    WriteLn(archivo, '// Celdas de relaciones (intersecciones)');
    actual := matriz.cabeza;
    while actual <> nil do
    begin
      i := remitentes.IndexOf(actual^.remitente);
      j := destinatarios.IndexOf(actual^.destinatario);

      WriteLn(archivo, '  N_', i, '_', j, ' [label="', actual^.cantidad, '", fillcolor=orange];');
      WriteLn(archivo, '  F', i, ' -> N_', i, '_', j, ';');
      WriteLn(archivo, '  N_', i, '_', j, ' -> C', j, ';');

      // Guardar el nodo en la lista de la fila
      filaNodos[i].Add('N_' + IntToStr(i) + '_' + IntToStr(j));

      actual := actual^.siguiente;
    end;

    // Conexión entre cabeceras
    WriteLn(archivo, '');
    WriteLn(archivo, '// Conectar nodo raíz con cabeceras');
    if destinatarios.Count > 0 then
      WriteLn(archivo, '  R -> C0;');
    for j := 0 to destinatarios.Count - 2 do
      WriteLn(archivo, '  C', j, ' -> C', j+1, ';');

    if remitentes.Count > 0 then
      WriteLn(archivo, '  R -> F0;');
    for i := 0 to remitentes.Count - 2 do
      WriteLn(archivo, '  F', i, ' -> F', i+1, ';');

    // Alineación de fila superior
    WriteLn(archivo, '');
    WriteLn(archivo, '{rank=same; R;');
    for j := 0 to destinatarios.Count - 1 do
      WriteLn(archivo, '  C', j, ';');
    WriteLn(archivo, '}');

    // Alineación de filas con solo nodos existentes
    for i := 0 to remitentes.Count - 1 do
    begin
      WriteLn(archivo, '{rank=same; F', i, ';');
      for j := 0 to filaNodos[i].Count - 1 do
        WriteLn(archivo, '  ', filaNodos[i][j], ';');
      WriteLn(archivo, '}');
    end;

    WriteLn(archivo, '}');
    CloseFile(archivo);

    // Liberar memoria
    remitentes.Free;
    destinatarios.Free;
    for i := 0 to High(filaNodos) do
      filaNodos[i].Free;

    ShowMessage('Reporte de relaciones generado: ' + filename);

  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

end.
