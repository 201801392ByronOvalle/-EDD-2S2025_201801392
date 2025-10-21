unit UControlLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, dateutils;

type
  TRegistroLog = record
    Usuario: string;
    Entrada: TDateTime;
    Salida: TDateTime;
  end;

  TControlLog = class
  private
    FRegistros: array of TRegistroLog;
    function BuscarRegistroAbierto(Usuario: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegistrarEntrada(Usuario: string);
    procedure RegistrarSalida(Usuario: string);
    function ExportarJSON: string;
    function ObtenerRegistros: TJSONArray;
  end;

var
  ControlLogGlobal: TControlLog;

implementation

constructor TControlLog.Create;
begin
  inherited Create;
  SetLength(FRegistros, 0);
end;

destructor TControlLog.Destroy;
begin
  SetLength(FRegistros, 0);
  inherited Destroy;
end;

procedure TControlLog.RegistrarEntrada(Usuario: string);
var
  Index: Integer;
begin
  Index := BuscarRegistroAbierto(Usuario);
  if Index = -1 then
  begin
    // Nuevo registro
    SetLength(FRegistros, Length(FRegistros) + 1);
    FRegistros[High(FRegistros)].Usuario := Usuario;
    FRegistros[High(FRegistros)].Entrada := Now;
    FRegistros[High(FRegistros)].Salida := 0; // 0 indica que aún no ha salido
  end;
end;

procedure TControlLog.RegistrarSalida(Usuario: string);
var
  Index: Integer;
begin
  Index := BuscarRegistroAbierto(Usuario);
  if Index <> -1 then
  begin
    FRegistros[Index].Salida := Now;
  end;
end;

function TControlLog.BuscarRegistroAbierto(Usuario: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FRegistros) do
  begin
    if (FRegistros[I].Usuario = Usuario) and (FRegistros[I].Salida = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TControlLog.ExportarJSON: string;
var
  JSONArray: TJSONArray;
  I: Integer;
  JSONObj: TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    for I := 0 to High(FRegistros) do
    begin
      JSONObj := TJSONObject.Create;
      JSONObj.Add('usuario', FRegistros[I].Usuario);
      JSONObj.Add('entrada', FormatDateTime('yyyy-mm-dd hh:nn:ss.zz', FRegistros[I].Entrada));
      if FRegistros[I].Salida <> 0 then
        JSONObj.Add('salida', FormatDateTime('yyyy-mm-dd hh:nn:ss.zz', FRegistros[I].Salida))
      else
        JSONObj.Add('salida', '');
      JSONArray.Add(JSONObj);
    end;
    Result := JSONArray.FormatJSON;
  finally
    JSONArray.Free;
  end;
end;

function TControlLog.ObtenerRegistros: TJSONArray;
var
  I: Integer;
  JSONObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for I := 0 to High(FRegistros) do
  begin
    JSONObj := TJSONObject.Create;
    JSONObj.Add('usuario', FRegistros[I].Usuario);
    JSONObj.Add('entrada', FormatDateTime('yyyy-mm-dd hh:nn:ss.zz', FRegistros[I].Entrada));
    if FRegistros[I].Salida <> 0 then
      JSONObj.Add('salida', FormatDateTime('yyyy-mm-dd hh:nn:ss.zz', FRegistros[I].Salida))
    else
      JSONObj.Add('salida', '');
    Result.Add(JSONObj);
  end;
end;

initialization
  ControlLogGlobal := TControlLog.Create;

finalization
  if Assigned(ControlLogGlobal) then
    ControlLogGlobal.Free;

end.
