unit UBandejaEntrada;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, UColaCorreos;

type

  { TFormBandejaEntrada }

  TFormBandejaEntrada = class(TForm)
    btnVolver: TButton;
    ListViewCorreos: TListView;
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEmailUsuario: string;
    procedure CargarCorreosUsuario;
  public
    property EmailUsuario: string read FEmailUsuario write FEmailUsuario;
  end;

var
  FormBandejaEntrada: TFormBandejaEntrada;

implementation

{$R *.lfm}

{ TFormBandejaEntrada }

procedure TFormBandejaEntrada.FormShow(Sender: TObject);
begin
  Caption := 'Bandeja de Entrada - ' + FEmailUsuario;
  CargarCorreosUsuario;
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
begin
  ListViewCorreos.Items.BeginUpdate;
  try
    ListViewCorreos.Items.Clear;

    if not Assigned(ColaCorreosGlobal) or ColaCorreosGlobal.EstaVacia then
    begin
      ShowMessage('No hay correos cargados en el sistema.');
      Exit;
    end;

    // Configurar columnas simples
    ListViewCorreos.Columns.Clear;
    with ListViewCorreos.Columns.Add do
    begin
      Caption := 'De';
      Width := 150;
    end;
    with ListViewCorreos.Columns.Add do
    begin
      Caption := 'Asunto';
      Width := 250;
    end;
    with ListViewCorreos.Columns.Add do
    begin
      Caption := 'Estado';
      Width := 80;
    end;

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
          Item.Caption := Correo.Remitente;
          Item.SubItems.Add(Correo.Asunto);
          Item.SubItems.Add(Correo.Estado);
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

end.
