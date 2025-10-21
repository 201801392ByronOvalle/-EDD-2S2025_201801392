unit UMenuRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaSimple, UColaCorreos, UControlLog, UFormControlLog, fpjson, jsonparser;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnComunidades: TButton;
    btnReporteComunidades: TButton;
    btnVerMensajesComunidad: TButton;
    btnControlLog: TButton;  // Nuevo botón
    btnSalir: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnComunidadesClick(Sender: TObject);
    procedure btnReporteComunidadesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure btnVerMensajesComunidadClick(Sender: TObject);
    procedure btnControlLogClick(Sender: TObject);  // Nuevo método
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON(ArchivoJSON: string);
    procedure CargarCorreosDesdeJSON(ArchivoJSON: string);
    function EsArchivoUsuarios(NombreArchivo: string): Boolean;
    function EsArchivoCorreos(NombreArchivo: string): Boolean;
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
      // Determinar qué tipo de archivo es por el nombre
      if EsArchivoUsuarios(OpenDialog1.FileName) then
      begin
        CargarUsuariosDesdeJSON(OpenDialog1.FileName);
        ShowMessage('Carga de usuarios completada exitosamente.' + sLineBreak +
                   'Usuarios cargados: ' + IntToStr(ListaUsuariosGlobal.Count));
      end
      else if EsArchivoCorreos(OpenDialog1.FileName) then
      begin
        CargarCorreosDesdeJSON(OpenDialog1.FileName);
        ShowMessage('Carga de correos completada exitosamente.' + sLineBreak +
                   'Correos cargados: ' + IntToStr(ColaCorreosGlobal.Count));
      end
      else
      begin
        ShowMessage('El archivo no coincide con los patrones esperados.' + sLineBreak +
                   'Nombres esperados: usuarios.json, correos.json');
      end;
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

procedure TFormMenuRoot.btnControlLogClick(Sender: TObject);
begin
  // Mostrar el formulario de Control de Logueo
  frmControlLogeo := TfrmControlLogeo.Create(Application);
  frmControlLogeo.ShowModal;
  frmControlLogeo.Free;
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
  // Registrar salida del usuario root
  if Assigned(ControlLogGlobal) then
    ControlLogGlobal.RegistrarSalida('root@edd.com');
  Close;
end;

procedure TFormMenuRoot.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(FormLogin) then
    FormLogin.Show;
end;

function TFormMenuRoot.EsArchivoUsuarios(NombreArchivo: string): Boolean;
var
  Nombre: string;
begin
  Nombre := ExtractFileName(LowerCase(NombreArchivo));
  // Verificar si el nombre contiene "usuarios" o "users"
  Result := (Pos('usuarios', Nombre) > 0) or (Pos('users', Nombre) > 0);
end;

function TFormMenuRoot.EsArchivoCorreos(NombreArchivo: string): Boolean;
var
  Nombre: string;
begin
  Nombre := ExtractFileName(LowerCase(NombreArchivo));
  // Verificar si el nombre contiene "correos" o "mails" o "emails"
  Result := (Pos('correos', Nombre) > 0) or (Pos('mails', Nombre) > 0) or
            (Pos('emails', Nombre) > 0);
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

      // Cargar usuarios
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
      end
      else
      begin
        ShowMessage('El archivo no contiene una sección "usuarios" válida.');
      end;

    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TFormMenuRoot.CargarCorreosDesdeJSON(ArchivoJSON: string);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  CorreosArray: TJSONArray;
  I: Integer;
  CorreoObj: TJSONObject;
  FileStream: TFileStream;
begin
  // Limpiar cola existente
  if Assigned(ColaCorreosGlobal) then
    ColaCorreosGlobal.LimpiarCola;

  // Cargar y parsear JSON
  FileStream := TFileStream.Create(ArchivoJSON, fmOpenRead);
  try
    JSONData := GetJSON(FileStream);
    try
      JSONObject := TJSONObject(JSONData);

      // Cargar correos
      CorreosArray := JSONObject.Get('correos', TJSONArray(nil)) as TJSONArray;
      if CorreosArray <> nil then
      begin
        for I := 0 to CorreosArray.Count - 1 do
        begin
          CorreoObj := CorreosArray.Objects[I];
          ColaCorreosGlobal.Encolar(
            CorreoObj.Get('id', 0),
            CorreoObj.Get('remitente', ''),
            CorreoObj.Get('destinatario', ''),
            CorreoObj.Get('estado', 'NL'), // Por defecto No Leído
            CorreoObj.Get('asunto', ''),
            CorreoObj.Get('mensaje', '')
          );
        end;
      end
      else
      begin
        ShowMessage('El archivo no contiene una sección "correos" válida.');
      end;

    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
