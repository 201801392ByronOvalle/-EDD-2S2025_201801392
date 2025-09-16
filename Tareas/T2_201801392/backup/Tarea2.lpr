program Tarea2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, fpjson, jsonparser, process;

type
  TPersona = record
    id: Integer;
    first_name: string;
    last_name: string;
    email: string;
  end;

  PNode = ^TNode;
  TNode = record
    data: TPersona;
    left, right: PNode;
  end;

  { TArbolBST }

  TArbolBST = class
  private
    root: PNode;
    procedure InsertarNodo(var node: PNode; persona: TPersona);
    procedure LiberarArbol(node: PNode);
    procedure GenerarGraphvizRec(node: PNode; var dotFile: TextFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insertar(persona: TPersona);
    procedure GenerarGraphviz(filename: string);
    procedure GenerarPNG(dotFileName: string);
  end;

  { T2_201801392 }

  T2_201801392 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    procedure CargarJSONyConstruirArbol;
  end;

{ TArbolBST }

constructor TArbolBST.Create;
begin
  root := nil;
end;

destructor TArbolBST.Destroy;
begin
  LiberarArbol(root);
  inherited Destroy;
end;

procedure TArbolBST.InsertarNodo(var node: PNode; persona: TPersona);
begin
  if node = nil then
  begin
    New(node);
    node^.data := persona;
    node^.left := nil;
    node^.right := nil;
  end
  else if persona.id < node^.data.id then
    InsertarNodo(node^.left, persona)
  else
    InsertarNodo(node^.right, persona);
end;

procedure TArbolBST.Insertar(persona: TPersona);
begin
  InsertarNodo(root, persona);
end;

procedure TArbolBST.LiberarArbol(node: PNode);
begin
  if node <> nil then
  begin
    LiberarArbol(node^.left);
    LiberarArbol(node^.right);
    Dispose(node);
  end;
end;

procedure TArbolBST.GenerarGraphvizRec(node: PNode; var dotFile: TextFile);
begin
  if node <> nil then
  begin
    // Escribir el nodo actual
    WriteLn(dotFile, '  ', node^.data.id, ' [label="ID: ', node^.data.id,
                    '\nNombre: ', node^.data.first_name, ' ', node^.data.last_name,
                    '\nEmail: ', node^.data.email, '"];');

    // Escribir las conexiones con los hijos
    if node^.left <> nil then
      WriteLn(dotFile, '  ', node^.data.id, ' -> ', node^.left^.data.id, ';');

    if node^.right <> nil then
      WriteLn(dotFile, '  ', node^.data.id, ' -> ', node^.right^.data.id, ';');

    // Recorrer hijos
    GenerarGraphvizRec(node^.left, dotFile);
    GenerarGraphvizRec(node^.right, dotFile);
  end;
end;

procedure TArbolBST.GenerarGraphviz(filename: string);
var
  dotFile: TextFile;
begin
  AssignFile(dotFile, filename);
  try
    Rewrite(dotFile);
    WriteLn(dotFile, 'digraph ArbolBST {');
    WriteLn(dotFile, '  node [shape=rectangle, style=filled, color=lightblue2];');

    if root <> nil then
      GenerarGraphvizRec(root, dotFile);

    WriteLn(dotFile, '}');
    WriteLn('Archivo Graphviz generado: ', filename);
  finally
    CloseFile(dotFile);
  end;
end;

procedure TArbolBST.GenerarPNG(dotFileName: string);
var
  Process: TProcess;
  pngFileName: string;
begin
  pngFileName := ChangeFileExt(dotFileName, '.png');

  Process := TProcess.Create(nil);
  try
    Process.Executable := 'dot';
    Process.Parameters.Add('-Tpng');
    Process.Parameters.Add(dotFileName);
    Process.Parameters.Add('-o');
    Process.Parameters.Add(pngFileName);

    Process.Options := Process.Options + [poWaitOnExit];

    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('Archivo PNG generado: ', pngFileName)
    else
      WriteLn('Error al generar el archivo PNG');
  finally
    Process.Free;
  end;
end;

{ T2_201801392 }

procedure T2_201801392.DoRun;
begin
  CargarJSONyConstruirArbol;
  Terminate;
end;

procedure T2_201801392.CargarJSONyConstruirArbol;
var
  jsonFile: TStringList;
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  i: Integer;
  persona: TPersona;
  arbol: TArbolBST;
  dotFileName: string;
begin
  // Nombre del archivo JSON
  dotFileName := 'arbol_bst.dot';

  try
    // Cargar archivo JSON
    jsonFile := TStringList.Create;
    jsonFile.LoadFromFile('/home/baof/Descargas/EDD_RE2/-EDD-2S2025_201801392/Tareas/T2_201801392/datos.json');

    // Parsear JSON
    jsonData := GetJSON(jsonFile.Text);
    jsonArray := jsonData as TJSONArray;

    // Crear árbol BST
    arbol := TArbolBST.Create;

    try
      // Insertar cada elemento en el árbol
      for i := 0 to jsonArray.Count - 1 do
      begin
        persona.id := (jsonArray.Objects[i] as TJSONObject).Get('id', 0);
        persona.first_name := (jsonArray.Objects[i] as TJSONObject).Get('first_name', '');
        persona.last_name := (jsonArray.Objects[i] as TJSONObject).Get('last_name', '');
        persona.email := (jsonArray.Objects[i] as TJSONObject).Get('email', '');

        arbol.Insertar(persona);
      end;

      WriteLn('Árbol BST construido con ', jsonArray.Count, ' elementos.');

      // Generar archivo Graphviz
      arbol.GenerarGraphviz(dotFileName);

      // Generar imagen PNG
      arbol.GenerarPNG(dotFileName);

    finally
      arbol.Free;
      jsonData.Free;
      jsonFile.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
    end;
  end;

  WriteLn('Presiona Enter para salir...');
  ReadLn;
end;

var
  Application: T2_201801392;
begin
  Application := T2_201801392.Create(nil);
  Application.Title := 'Cargador de JSON a Árbol BST';
  Application.Run;
  Application.Free;
end.
