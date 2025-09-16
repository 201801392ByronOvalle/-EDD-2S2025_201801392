unit EDDVerCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  EDDEstructuras, EDDGlobal, EDDCorreos, EDDMatrizRelaciones;

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
    UsuarioActual: PUsuario;
    procedure ActualizarTabla;
    procedure EnviarCorreosPendientes;
  public
    procedure SetUsuarioActual(usuario: PUsuario);
  end;

var
  frmVerCorreosProgramados: TfrmVerCorreosProgramados;

implementation

{$R *.lfm}

{ TfrmVerCorreosProgramados }

procedure TfrmVerCorreosProgramados.SetUsuarioActual(usuario: PUsuario);
begin
  UsuarioActual := usuario;
end;

procedure TfrmVerCorreosProgramados.FormShow(Sender: TObject);
begin
  if UsuarioActual <> nil then
    lblTitulo.Caption := 'Correos Programados - ' + UsuarioActual^.nombre;
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
  sgCorreos.ColCount := 6;

  // Configurar encabezados
  sgCorreos.Cells[0, 0] := 'ID';
  sgCorreos.Cells[1, 0] := 'Remitente';
  sgCorreos.Cells[2, 0] := 'Destinatario';
  sgCorreos.Cells[3, 0] := 'Asunto';
  sgCorreos.Cells[4, 0] := 'Programado para';
  sgCorreos.Cells[5, 0] := 'Enviado';

  // Llenar tabla solo con correos del usuario actual
  actual := ColaCorreosGlobal.frente;
  i := 1;

  while actual <> nil do
  begin
    // Filtrar por remitente (solo correos del usuario actual)
    if (UsuarioActual <> nil) and (actual^.dato^.remitente = UsuarioActual^.email) then
    begin
      sgCorreos.RowCount := sgCorreos.RowCount + 1;

      // Convertir estado a texto legible
      if actual^.dato^.estado = 'P' then
        estadoTexto := 'No'
      else
        estadoTexto := 'Sí';

      sgCorreos.Cells[0, i] := IntToStr(actual^.dato^.id);
      sgCorreos.Cells[1, i] := actual^.dato^.remitente;
      sgCorreos.Cells[2, i] := actual^.dato^.destinatario;
      sgCorreos.Cells[3, i] := actual^.dato^.asunto;
      sgCorreos.Cells[4, i] := DateTimeToStr(actual^.dato^.fechaHoraProgramada);
      sgCorreos.Cells[5, i] := estadoTexto;

      Inc(i);
    end;

    actual := actual^.siguiente;
  end;

  // Si no hay correos, mostrar mensaje
  if sgCorreos.RowCount = 1 then
  begin
    ShowMessage('No tienes correos programados');
    Close;
  end;

  // Ajustar anchos de columnas
  sgCorreos.AutoSizeColumns;
end;

procedure TfrmVerCorreosProgramados.btnEnviarTodosClick(Sender: TObject);
begin
  if (UsuarioActual = nil) or (sgCorreos.RowCount <= 1) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  if MessageDlg('Confirmar', '¿Enviar todos tus correos programados?',
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

  // Cambiar estado de los correos programados del usuario actual a enviados
  while actual <> nil do
  begin
    if (actual^.dato^.estado = 'P') and
       (UsuarioActual <> nil) and
       (actual^.dato^.remitente = UsuarioActual^.email) then
    begin
      actual^.dato^.estado := 'E'; // Cambiar a Enviado

      // Registrar relación en la matriz dispersa
      InsertarRelacion(MatrizRelacionesGlobal,
                      actual^.dato^.remitente,
                      actual^.dato^.destinatario);

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
