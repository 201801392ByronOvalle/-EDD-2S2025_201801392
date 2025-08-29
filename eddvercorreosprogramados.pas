unit EDDVerCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  EDDGlobal, EDDCorreos;

type

  { TfrmVerCorreosProgramados }

  TfrmVerCorreosProgramados = class(TForm)
    btnEnviarTodos: TButton;
    btnCerrar: TButton;
    lblTitulo: TLabel;
    sgCorreos: TStringGrid;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarTodosClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ActualizarTabla;
    procedure EnviarCorreosPendientes;
  public
  end;

var
  frmVerCorreosProgramados: TfrmVerCorreosProgramados;

implementation

{$R *.lfm}

{ TfrmVerCorreosProgramados }

procedure TfrmVerCorreosProgramados.FormShow(Sender: TObject);
begin
  ActualizarTabla;
end;

procedure TfrmVerCorreosProgramados.ActualizarTabla;
var
  actual: PNodoCorreo;
  i: Integer;
  estadoTexto: string;
begin
  // Configurar StringGrid
  sgCorreos.Clear;
  sgCorreos.RowCount := 1; // Solo la fila de encabezados
  sgCorreos.ColCount := 6; // 6 columnas

  // Configurar encabezados
  sgCorreos.Cells[0, 0] := 'ID';
  sgCorreos.Cells[1, 0] := 'Remitente';
  sgCorreos.Cells[2, 0] := 'Destinatario';
  sgCorreos.Cells[3, 0] := 'Asunto';
  sgCorreos.Cells[4, 0] := 'Programado para';
  sgCorreos.Cells[5, 0] := 'Enviado'; // Columna de estado

  // Llenar tabla con correos programados
  actual := ColaCorreosGlobal.frente;
  i := 1;

  while actual <> nil do
  begin
    sgCorreos.RowCount := sgCorreos.RowCount + 1;

    // Convertir estado a texto legible
    if actual^.dato^.estado = 'P' then
      estadoTexto := 'No'
    else
      estadoTexto := 'Si';

    sgCorreos.Cells[0, i] := IntToStr(actual^.dato^.id);
    sgCorreos.Cells[1, i] := actual^.dato^.remitente;
    sgCorreos.Cells[2, i] := actual^.dato^.destinatario;
    sgCorreos.Cells[3, i] := actual^.dato^.asunto;
    sgCorreos.Cells[4, i] := DateTimeToStr(actual^.dato^.fechaHoraProgramada);
    sgCorreos.Cells[5, i] := estadoTexto; // Estado como Si/No

    actual := actual^.siguiente;
    Inc(i);
  end;

  // Ajustar ancho de columnas
  sgCorreos.AutoSizeColumns;
end;

procedure TfrmVerCorreosProgramados.btnEnviarTodosClick(Sender: TObject);
begin
  if ColaVacia(ColaCorreosGlobal) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  if MessageDlg('Confirmar', '¿Enviar todos los correos programados?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    EnviarCorreosPendientes;
    ActualizarTabla;
  end;
end;

procedure TfrmVerCorreosProgramados.EnviarCorreosPendientes;
var
  actual: PNodoCorreo;
  enviados: Integer;
begin
  enviados := 0;
  actual := ColaCorreosGlobal.frente;

  // Cambiar estado de todos los correos programados a enviados
  while actual <> nil do
  begin
    if actual^.dato^.estado = 'P' then // Solo los programados
    begin
      actual^.dato^.estado := 'E'; // Cambiar a Enviado
      Inc(enviados);
    end;
    actual := actual^.siguiente;
  end;

  ShowMessage('Se marcaron ' + IntToStr(enviados) + ' correos como enviados');
end;

procedure TfrmVerCorreosProgramados.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.
