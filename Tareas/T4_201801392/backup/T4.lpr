program T4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Contnrs, Process;

type
  TCity = class
  public
    Name: string;
    constructor Create(CityName: string);
  end;

  TConnection = class
  public
    Destination: TCity;
    Weight: Integer;
    constructor Create(Dest: TCity; W: Integer = 1);
  end;

  TUndirectedGraph = class
  private
    FCities: TObjectList;
    FAdjacencyList: TObjectList;
    function FindCityIndex(CityName: string): Integer;
    function GetCityByName(CityName: string): TCity;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCity(CityName: string);
    procedure AddConnection(City1, City2: string; Weight: Integer = 1);
    procedure ShowAdjacencyList;
    procedure GenerateGraphvizFile(Filename: string);
  end;

  { T4_201801392 }

  T4_201801392 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
  end;

{ TCity }

constructor TCity.Create(CityName: string);
begin
  inherited Create;
  Name := CityName;
end;

{ TConnection }

constructor TConnection.Create(Dest: TCity; W: Integer = 1);
begin
  inherited Create;
  Destination := Dest;
  Weight := W;
end;

{ TUndirectedGraph }

constructor TUndirectedGraph.Create;
begin
  inherited Create;
  FCities := TObjectList.Create(True);
  FAdjacencyList := TObjectList.Create(True);
end;

destructor TUndirectedGraph.Destroy;
begin
  FCities.Free;
  FAdjacencyList.Free;
  inherited Destroy;
end;

function TUndirectedGraph.FindCityIndex(CityName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCities.Count - 1 do
  begin
    if TCity(FCities[I]).Name = CityName then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TUndirectedGraph.GetCityByName(CityName: string): TCity;
var
  Index: Integer;
begin
  Index := FindCityIndex(CityName);
  if Index <> -1 then
    Result := TCity(FCities[Index])
  else
    Result := nil;
end;

procedure TUndirectedGraph.AddCity(CityName: string);
begin
  if FindCityIndex(CityName) = -1 then
  begin
    FCities.Add(TCity.Create(CityName));
    FAdjacencyList.Add(TObjectList.Create(True));
    WriteLn('Ciudad "', CityName, '" agregada correctamente.');
  end
  else
    WriteLn('Error: La ciudad "', CityName, '" ya existe.');
end;

procedure TUndirectedGraph.AddConnection(City1, City2: string; Weight: Integer = 1);
var
  CityObj1, CityObj2: TCity;
  Index1, Index2: Integer;
begin
  CityObj1 := GetCityByName(City1);
  CityObj2 := GetCityByName(City2);

  if (CityObj1 = nil) or (CityObj2 = nil) then
  begin
    WriteLn('Error: Una o ambas ciudades no existen.');
    Exit;
  end;

  Index1 := FindCityIndex(City1);
  Index2 := FindCityIndex(City2);

  TObjectList(FAdjacencyList[Index1]).Add(TConnection.Create(CityObj2, Weight));
  TObjectList(FAdjacencyList[Index2]).Add(TConnection.Create(CityObj1, Weight));

  WriteLn('Conexión agregada: "', City1, '" <--> "', City2, '" (Peso: ', Weight, ')');
end;

procedure TUndirectedGraph.ShowAdjacencyList;
var
  I, J: Integer;
  CurrentCity: TCity;
  Connections: TObjectList;
  Conn: TConnection;
begin
  WriteLn;
  WriteLn('=== LISTA DE ADYACENCIA ===');

  for I := 0 to FCities.Count - 1 do
  begin
    CurrentCity := TCity(FCities[I]);
    Write(CurrentCity.Name, ' -> ');

    Connections := TObjectList(FAdjacencyList[I]);
    if Connections.Count = 0 then
      Write('Sin conexiones')
    else
    begin
      for J := 0 to Connections.Count - 1 do
      begin
        Conn := TConnection(Connections[J]);
        Write(Conn.Destination.Name, '(', Conn.Weight, ')');
        if J < Connections.Count - 1 then
          Write(', ');
      end;
    end;
    WriteLn;
  end;
  WriteLn;
end;

procedure TUndirectedGraph.GenerateGraphvizFile(Filename: string);
var
  I, J: Integer;
  Lines: TStringList;
  CurrentCity: TCity;
  Connections: TObjectList;
  Conn: TConnection;
  ProcessedPairs: TStringList;
  Pair: string;
  AProcess: TProcess;
begin
  Lines := TStringList.Create;
  ProcessedPairs := TStringList.Create;
  try
    Lines.Add('graph CitiesGraph {');
    Lines.Add('    rankdir=LR;');
    Lines.Add('    node [shape=circle, style=filled, fillcolor=lightblue];');
    Lines.Add('');

    for I := 0 to FCities.Count - 1 do
    begin
      CurrentCity := TCity(FCities[I]);
      Lines.Add('    ' + CurrentCity.Name + ';');
    end;

    Lines.Add('');

    for I := 0 to FCities.Count - 1 do
    begin
      CurrentCity := TCity(FCities[I]);
      Connections := TObjectList(FAdjacencyList[I]);

      for J := 0 to Connections.Count - 1 do
      begin
        Conn := TConnection(Connections[J]);

        if CurrentCity.Name < Conn.Destination.Name then
          Pair := CurrentCity.Name + '_' + Conn.Destination.Name
        else
          Pair := Conn.Destination.Name + '_' + CurrentCity.Name;

        if ProcessedPairs.IndexOf(Pair) = -1 then
        begin
          Lines.Add('    ' + CurrentCity.Name + ' -- ' + Conn.Destination.Name +
                   ' [label="' + IntToStr(Conn.Weight) + '"];');
          ProcessedPairs.Add(Pair);
        end;
      end;
    end;

    Lines.Add('}');

    Lines.SaveToFile(Filename + '.dot');
    WriteLn('Archivo Graphviz guardado como: ', Filename + '.dot');

    // Generar PNG automáticamente
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'dot';
      AProcess.Parameters.Add('-Tpng');
      AProcess.Parameters.Add(Filename + '.dot');
      AProcess.Parameters.Add('-o');
      AProcess.Parameters.Add(Filename + '.png');
      AProcess.Options := AProcess.Options + [poWaitOnExit, poNoConsole];

      AProcess.Execute;
      WriteLn('Imagen PNG generada: ', Filename + '.png');

    finally
      AProcess.Free;
    end;

  finally
    Lines.Free;
    ProcessedPairs.Free;
  end;
end;

{ T4_201801392 }

procedure T4_201801392.DoRun;
var
  Graph: TUndirectedGraph;
begin
  WriteLn('=== TAREA #4 - GRAFO NO DIRIGIDO ===');
  WriteLn;

  Graph := TUndirectedGraph.Create;
  try
    WriteLn('Agregando ciudades...');
    Graph.AddCity('A');
    Graph.AddCity('B');
    Graph.AddCity('C');
    Graph.AddCity('D');

    WriteLn;

    WriteLn('Agregando conexiones...');
    Graph.AddConnection('A', 'B');
    Graph.AddConnection('A', 'C');
    Graph.AddConnection('B', 'D');

    WriteLn;

    Graph.ShowAdjacencyList;

    Graph.GenerateGraphvizFile('grafo');

    WriteLn;
    WriteLn('Programa ejecutado correctamente.');

  finally
    Graph.Free;
  end;

  WriteLn('Presione ENTER para salir...');
  ReadLn;

  Terminate;
end;

var
  Application: T4_201801392;
begin
  Application:=T4_201801392.Create(nil);
  Application.Title:='Tarea 4 - Grafo No Dirigido';
  Application.Run;
  Application.Free;
end.
