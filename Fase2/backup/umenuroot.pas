unit UMenuRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaSimple, fpjson, jsonparser;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnComunidades: TButton;
    btnReporteComunidades: TButton;
    btnVerMensajesComunidad: TButton;
    btnSalir: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnComunidadesClick(Sender: TObject);
    procedure btnReporteComunidadesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure btnVerMensajesComunidadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON(ArchivoJSON: string);
  public
  end;

var
  FormMenuRoot: TFormMenuRoot;

implementation

uses
  ProyectoF2;

{$R *.lfm}

{ TFormMenuRoot }

procedure TFormMenuRoot.FormShow(Sender: TObject);
begin
  // Configurar el formulario cuando se muestra
  Caption := 'EDDMail - Menú Administrador (ROOT)';
  Label1.Caption := 'Menú Principal - Usuario ROOT';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];
end;

procedure TFormMenuRoot.btnCargaMasivaClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      CargarUsuariosDesdeJSON(OpenDialog1.FileName);
      ShowMessage('Carga masiva completada exitosamente.' + sLineBreak +
                 'Se cargaron ' + IntToStr(ListaUsuariosGlobal.Count) + ' usuarios.');
    except
      on E: Exception do
        ShowMessage('Error al cargar el archivo JSON: ' + E.Message);
    end;
  end;
end;

procedure TFormMenuRoot.btnComunidadesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Gestión de Comunidades en desarrollo...');
end;

procedure TFormMenuRoot.btnReporteComunidadesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Reporte de Comunidades en desarrollo...');
end;

procedure TFormMenuRoot.btnVerMensajesComunidadClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de Ver Mensajes de Comunidad en desarrollo...');
end;

procedure TFormMenuRoot.btnSalirClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMenuRoot.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(FormLogin) then
    FormLogin.Show;
end;

procedure TFormMenuRoot.CargarUsuariosDesdeJSON(ArchivoJSON: string);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  UsuariosArray: TJSONArray;
  I: Integer;
  UsuarioObj: TJSONObject;
  FileStream: TFileStream;
begin
  // Limpiar lista existente
  if Assigned(ListaUsuariosGlobal) then
    ListaUsuariosGlobal.LimpiarLista;

  // Cargar y parsear JSON
  FileStream := TFileStream.Create(ArchivoJSON, fmOpenRead);
  try
    JSONData := GetJSON(FileStream);
    try
      JSONObject := TJSONObject(JSONData);
      UsuariosArray := JSONObject.Get('usuarios', TJSONArray(nil)) as TJSONArray;

      if UsuariosArray <> nil then
      begin
        for I := 0 to UsuariosArray.Count - 1 do
        begin
          UsuarioObj := UsuariosArray.Objects[I];
          ListaUsuariosGlobal.AgregarUsuario(
            UsuarioObj.Get('id', 0),
            UsuarioObj.Get('nombre', ''),
            UsuarioObj.Get('usuario', ''),
            UsuarioObj.Get('password', ''),
            UsuarioObj.Get('email', ''),
            UsuarioObj.Get('telefono', '')
          );
        end;
      end;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
