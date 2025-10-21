unit UFormControlLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UControlLog, fpjson, jsonparser;  // Agregar fpjson y jsonparser aquí

type

  { TfrmControlLogeo }

  TfrmControlLogeo = class(TForm)
    btnExportarJSON: TButton;
    btnVisualizarControl: TButton;
    btnVolver: TButton;
    Label1: TLabel;
    ListViewLog: TListView;
    SaveDialog1: TSaveDialog;
    procedure btnExportarJSONClick(Sender: TObject);
    procedure btnVisualizarControlClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure MostrarControlLog;
  public
  end;

var
  frmControlLogeo: TfrmControlLogeo;

implementation

{$R *.lfm}

{ TfrmControlLogeo }

procedure TfrmControlLogeo.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Control de Logueo';
  Label1.Caption := 'Control de Logueo';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];
end;

procedure TfrmControlLogeo.btnVisualizarControlClick(Sender: TObject);
begin
  MostrarControlLog;
end;

procedure TfrmControlLogeo.btnExportarJSONClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      if Assigned(ControlLogGlobal) then
      begin
        // Exportar el log a JSON
        with TStringList.Create do
        try
          Text := ControlLogGlobal.ExportarJSON;
          SaveToFile(SaveDialog1.FileName);
          ShowMessage('Log exportado exitosamente a: ' + SaveDialog1.FileName);
        finally
          Free;
        end;
      end;
    except
      on E: Exception do
        ShowMessage('Error al exportar el log: ' + E.Message);
    end;
  end;
end;

procedure TfrmControlLogeo.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmControlLogeo.MostrarControlLog;
var
  JSONArray: TJSONArray;
  I: Integer;
  JSONObj: TJSONObject;
  Item: TListItem;
begin
  if not Assigned(ControlLogGlobal) then
  begin
    ShowMessage('Sistema de log no disponible.');
    Exit;
  end;

  ListViewLog.Items.BeginUpdate;
  try
    ListViewLog.Items.Clear;

    // Configurar columnas
    ListViewLog.Columns.Clear;
    with ListViewLog.Columns.Add do
    begin
      Caption := 'Usuario';
      Width := 200;
    end;
    with ListViewLog.Columns.Add do
    begin
      Caption := 'Entrada';
      Width := 200;
    end;
    with ListViewLog.Columns.Add do
    begin
      Caption := 'Salida';
      Width := 200;
    end;

    // Obtener registros y mostrar en ListView
    JSONArray := ControlLogGlobal.ObtenerRegistros;
    try
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObj := JSONArray.Objects[I];
        Item := ListViewLog.Items.Add;
        Item.Caption := JSONObj.Get('usuario', '');
        Item.SubItems.Add(JSONObj.Get('entrada', ''));
        Item.SubItems.Add(JSONObj.Get('salida', ''));
      end;
    finally
      JSONArray.Free;
    end;

    ShowMessage('Se mostraron ' + IntToStr(ListViewLog.Items.Count) + ' registros de logueo.');

  finally
    ListViewLog.Items.EndUpdate;
  end;
end;

end.
