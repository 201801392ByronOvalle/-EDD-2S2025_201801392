unit UBandejaEntrada;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, UColaCorreos;

type

  { TFormBandejaEntrada }

  TFormBandejaEntrada = class(TForm)
    btnVolver: TButton;
    Label1: TLabel;
    lblTotalCorreos: TLabel;
    lblNoLeidos: TLabel;
    ListViewCorreos: TListView;
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewCorreosDblClick(Sender: TObject);
  private
    FEmailUsuario: string;
    procedure CargarCorreosUsuario;
    procedure ActualizarContadores;
    function ObtenerCorreosParaUsuario(EmailUsuario: string): TStringList;
  public
    property EmailUsuario: string read FEmailUsuario write FEmailUsuario;
  end;

var
  FormBandejaEntrada: TFormBandejaEntrada;

implementation

uses
  UFormDetalleCorreo;

{$R *.lfm}

{ TFormBandejaEntrada }

procedure TFormBandejaEntrada.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Bandeja de Entrada - ' + FEmailUsuario;
  Label1.Caption := 'Bandeja de Entrada - ' + FEmailUsuario;

  // Configurar columnas del ListView
  ListViewCorreos.Columns.Clear;
  with ListViewCorreos.Columns.Add do
  begin
    Caption := 'Estado';
    Width := 50;
  end;
  with ListViewCorreos.Columns.Add do
  begin
    Caption := 'Asunto';
    Width := 200;
  end;
  with ListViewCorreos.Columns.Add do
  begin
    Caption := 'Remitente';
    Width := 150;
  end;
  with ListViewCorreos.Columns.Add do
  begin
    Caption := 'Fecha';
    Width := 120;
  end;

  CargarCorreosUsuario;
  ActualizarContadores;
end;

procedure TFormBandejaEntrada.ListViewCorreosDblClick(Sender: TObject);
var
  CorreoID: Integer;
  CorreosUsuario: TStringList;
  I: Integer;
  Campos: TStringArray;
  CorreoEncontrado: Boolean;
begin
  if ListViewCorreos.Selected = nil then Exit;

  // Obtener el ID del correo seleccionado (está en la última columna)
  CorreoID := StrToIntDef(ListViewCorreos.Selected.SubItems[3], -1);

  if CorreoID = -1 then Exit;

  // Buscar el correo en la cola global
  if Assigned(ColaCorreosGlobal) then
  begin
    // Obtener todos los correos para este usuario
    CorreosUsuario := ObtenerCorreosParaUsuario(FEmailUsuario);
    try
      CorreoEncontrado := False;
      for I := 0 to CorreosUsuario.Count - 1 do
      begin
        Campos := CorreosUsuario[I].Split(['|']);
        if (Length(Campos) >= 7) and (Campos[0] = IntToStr(CorreoID)) then
        begin
          // Mostrar el formulario de detalles
          frmDetalleCorreo := TfrmDetalleCorreo.Create(Application);
          frmDetalleCorreo.Remitente := Campos[2];
          frmDetalleCorreo.Asunto := Campos[4];
          frmDetalleCorreo.Fecha := Campos[6];
          frmDetalleCorreo.Mensaje := Campos[5];
          frmDetalleCorreo.ShowModal;
          frmDetalleCorreo.Free;

          // Marcar como leído si no lo está
          if ListViewCorreos.Selected.Caption = 'NL' then
          begin
            ListViewCorreos.Selected.Caption := 'L';
            ActualizarContadores;
          end;

          CorreoEncontrado := True;
          Break;
        end;
      end;

      if not CorreoEncontrado then
        ShowMessage('No se pudo encontrar el correo seleccionado.');

    finally
      CorreosUsuario.Free;
    end;
  end;
end;

procedure TFormBandejaEntrada.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBandejaEntrada.CargarCorreosUsuario;
var
  ColaTemp: TColaCorreos;
  Correo: TDatoCorreo;
  Item: TListItem;
  FechaActual: string;
begin
  ListViewCorreos.Items.BeginUpdate;
  try
    ListViewCorreos.Items.Clear;

    if not Assigned(ColaCorreosGlobal) or ColaCorreosGlobal.EstaVacia then
    begin
      ShowMessage('No hay correos cargados en el sistema.');
      Exit;
    end;

    // Obtener fecha actual para los correos (simulada)
    FechaActual := FormatDateTime('dd/mm/yyyy', Now);

    // Crear una cola temporal para procesar
    ColaTemp := TColaCorreos.Create;
    try
      // Procesar todos los correos
      while not ColaCorreosGlobal.EstaVacia do
      begin
        Correo := ColaCorreosGlobal.Desencolar;
        ColaTemp.Encolar(Correo.Id, Correo.Remitente, Correo.Destinatario,
                         Correo.Estado, Correo.Asunto, Correo.Mensaje);

        // Si el correo es para este usuario, mostrarlo
        if Correo.Destinatario = FEmailUsuario then
        begin
          Item := ListViewCorreos.Items.Add;
          Item.Caption := Correo.Estado;
          Item.SubItems.Add(Correo.Asunto);
          Item.SubItems.Add(Correo.Remitente);
          Item.SubItems.Add(FechaActual); // Fecha simulada
          Item.SubItems.Add(IntToStr(Correo.Id)); // ID oculto para referencia
        end;
      end;

      // Restaurar la cola original
      while not ColaTemp.EstaVacia do
      begin
        Correo := ColaTemp.Desencolar;
        ColaCorreosGlobal.Encolar(Correo.Id, Correo.Remitente, Correo.Destinatario,
                                 Correo.Estado, Correo.Asunto, Correo.Mensaje);
      end;

    finally
      ColaTemp.Free;
    end;

  finally
    ListViewCorreos.Items.EndUpdate;
  end;
end;

procedure TFormBandejaEntrada.ActualizarContadores;
var
  Total, NoLeidos: Integer;
  I: Integer;
begin
  Total := ListViewCorreos.Items.Count;
  NoLeidos := 0;

  for I := 0 to Total - 1 do
  begin
    if ListViewCorreos.Items[I].Caption = 'NL' then
      Inc(NoLeidos);
  end;

  lblTotalCorreos.Caption := 'Total: ' + IntToStr(Total);
  lblNoLeidos.Caption := 'No leídos: ' + IntToStr(NoLeidos);
end;

function TFormBandejaEntrada.ObtenerCorreosParaUsuario(EmailUsuario: string): TStringList;
var
  ColaTemp: TColaCorreos;
  Correo: TDatoCorreo;
  FechaActual: string;
begin
  Result := TStringList.Create;

  if not Assigned(ColaCorreosGlobal) or ColaCorreosGlobal.EstaVacia then
    Exit;

  // Fecha actual simulada
  FechaActual := FormatDateTime('dd/mm/yyyy', Now);

  // Crear una cola temporal para no perder los datos originales
  ColaTemp := TColaCorreos.Create;
  try
    // Copiar todos los correos a la cola temporal
    while not ColaCorreosGlobal.EstaVacia do
    begin
      Correo := ColaCorreosGlobal.Desencolar;
      ColaTemp.Encolar(Correo.Id, Correo.Remitente, Correo.Destinatario,
                       Correo.Estado, Correo.Asunto, Correo.Mensaje);

      // Si el correo es para este usuario, agregarlo a la lista
      if Correo.Destinatario = EmailUsuario then
      begin
        Result.Add(Format('%d|%s|%s|%s|%s|%s|%s',
          [Correo.Id, Correo.Remitente, Correo.Destinatario,
           Correo.Estado, Correo.Asunto, Correo.Mensaje, FechaActual]));
      end;
    end;

    // Restaurar la cola original
    while not ColaTemp.EstaVacia do
    begin
      Correo := ColaTemp.Desencolar;
      ColaCorreosGlobal.Encolar(Correo.Id, Correo.Remitente, Correo.Destinatario,
                               Correo.Estado, Correo.Asunto, Correo.Mensaje);
    end;

  finally
    ColaTemp.Free;
  end;
end;

end.
