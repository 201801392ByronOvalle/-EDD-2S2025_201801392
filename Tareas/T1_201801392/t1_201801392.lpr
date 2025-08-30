program t1_201801392;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson, jsonparser, process;

type
  PCorreo = ^TCorreo;
  TCorreo = record
    id: Integer;
    remitente, estado, programado, asunto, fecha, mensaje: String;
    anterior, siguiente: PCorreo;
  end;

var
  raizCorreos: PCorreo = nil;
  totalCorreos: Integer = 0;

{ --------- LISTA DOBLEMENTE ENLAZADA --------- }
procedure AgregarCorreo(correoObj: TJSONObject);
var
  nuevo, tmp: PCorreo;
begin
  New(nuevo);
  nuevo^.id := correoObj.Integers['id'];
  nuevo^.remitente := correoObj.Strings['remitente'];
  nuevo^.estado := correoObj.Strings['estado'];
  nuevo^.programado := correoObj.Strings['programado'];
  nuevo^.asunto := correoObj.Strings['asunto'];
  nuevo^.fecha := correoObj.Strings['fecha'];
  nuevo^.mensaje := correoObj.Strings['mensaje'];
  nuevo^.anterior := nil;
  nuevo^.siguiente := nil;

  if raizCorreos = nil then
    raizCorreos := nuevo
  else
  begin
    tmp := raizCorreos;
    while tmp^.siguiente <> nil do
      tmp := tmp^.siguiente;
    tmp^.siguiente := nuevo;
    nuevo^.anterior := tmp;
  end;

  Inc(totalCorreos);
end;

{ --------- CARGAR JSON --------- }
procedure CargarCorreos(const filename: String);
var
  sl: TStringList;
  jsonData: TJSONData;
  correosArray, bandeja: TJSONArray;
  usuarioObj, correoObj: TJSONObject;
  i, j: Integer;
begin
  if not FileExists(filename) then
  begin
    WriteLn('Archivo no encontrado: ', filename);
    Exit;
  end;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    jsonData := GetJSON(sl.Text);
  finally
    sl.Free;
  end;

  correosArray := jsonData.FindPath('correos') as TJSONArray;

  for i := 0 to correosArray.Count - 1 do
  begin
    usuarioObj := correosArray.Objects[i];
    bandeja := usuarioObj.Arrays['bandeja_entrada'];

    for j := 0 to bandeja.Count - 1 do
    begin
      correoObj := bandeja.Objects[j];
      AgregarCorreo(correoObj);
    end;
  end;

  WriteLn('Se cargó correos.json');
  WriteLn('Cantidad de correos: ', totalCorreos);

  jsonData.Free;
end;

{ --------- GENERAR DOT Y PNG --------- }
procedure GenerarDot(const filename: String);
var
  dot: TStringList;
  tmp: PCorreo;
  nodo, enlace: String;
  AProcess: TProcess;
begin
  dot := TStringList.Create;
  try
    dot.Add('digraph ListaCorreos {');
    dot.Add('  rankdir=LR;');
    dot.Add('  node [shape=record, style=filled, fillcolor=lightblue];');

    tmp := raizCorreos;
    while tmp <> nil do
    begin
      nodo := Format('N%d [label="Id: %d\nRemitente: %s\nEstado: %s\nProgramado: %s\nAsunto: %s\nFecha: %s\nMensaje: %s"];',
        [tmp^.id, tmp^.id, tmp^.remitente, tmp^.estado, tmp^.programado,
         tmp^.asunto, tmp^.fecha, tmp^.mensaje]);
      dot.Add(nodo);

      if tmp^.siguiente <> nil then
      begin
        enlace := Format('N%d -> N%d [dir=both];', [tmp^.id, tmp^.siguiente^.id]);
        dot.Add(enlace);
      end;

      tmp := tmp^.siguiente;
    end;

    dot.Add('}');
    dot.SaveToFile(filename);
  finally
    dot.Free;
  end;

  WriteLn('Archivo DOT generado: ', filename);

  // Ejecutar Graphviz con TProcess
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'dot';
    AProcess.Parameters.Add('-Tpng');
    AProcess.Parameters.Add(filename);
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('correos.png');
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.Execute;
  finally
    AProcess.Free;
  end;

  WriteLn('Gráfico generado: correos.png');
end;

{ --------- MAIN --------- }
begin
  CargarCorreos('correos.json');
  GenerarDot('correos.dot');
end.

