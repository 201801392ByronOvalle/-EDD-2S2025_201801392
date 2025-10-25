unit UMenuRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process,
  UListaSimple, UColaCorreos, UControlLog, UFormControlLog, fpjson, jsonparser,
  UGrafoContactos,  UFormComunidades, UArbolComunidades, UFormVerMensajesComunidad;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnComunidades: TButton;
    btnVerMensajesComunidad: TButton;
    btnControlLog: TButton;
    btnSalir: TButton;
    btnReportes: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnComunidadesClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure btnVerMensajesComunidadClick(Sender: TObject);
    procedure btnControlLogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON(ArchivoJSON: string);
    procedure CargarCorreosDesdeJSON(ArchivoJSON: string);
    procedure CargarContactosDesdeJSON(ArchivoJSON: string);
    function EsArchivoUsuarios(NombreArchivo: string): Boolean;
    function EsArchivoCorreos(NombreArchivo: string): Boolean;
    function EsArchivoContactos(NombreArchivo: string): Boolean;
    procedure GenerarReportesCompletos;
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
      else if EsArchivoContactos(OpenDialog1.FileName) then
      begin
        CargarContactosDesdeJSON(OpenDialog1.FileName);
        ShowMessage('Carga de contactos completada exitosamente.' + sLineBreak +
                   'Contactos cargados en el grafo.' + sLineBreak +
                   GrafoContactosGlobal.ToStringGrafo);
      end
      else
      begin
        ShowMessage('El archivo no coincide con los patrones esperados.' + sLineBreak +
                   'Nombres esperados: usuarios.json, correos.json, contactos.json');
      end;
    except
      on E: Exception do
        ShowMessage('Error al cargar el archivo JSON: ' + E.Message);
    end;
  end;
end;

procedure TFormMenuRoot.btnComunidadesClick(Sender: TObject);
begin
  // Mostrar formulario de gestión de comunidades
  frmComunidades := TfrmComunidades.Create(Application);
  frmComunidades.ShowModal;
  frmComunidades.Free;
end;

procedure TFormMenuRoot.btnControlLogClick(Sender: TObject);
begin
  // Mostrar el formulario de Control de Logueo
  frmControlLogeo := TfrmControlLogeo.Create(Application);
  frmControlLogeo.ShowModal;
  frmControlLogeo.Free;
end;

procedure TFormMenuRoot.btnReportesClick(Sender: TObject);
begin
  GenerarReportesCompletos;
end;

procedure TFormMenuRoot.btnVerMensajesComunidadClick(Sender: TObject);
begin
  // Mostrar formulario para ver mensajes de comunidades
  frmVerMensajesComunidad := TfrmVerMensajesComunidad.Create(Application);
  frmVerMensajesComunidad.ShowModal;
  frmVerMensajesComunidad.Free;
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

function TFormMenuRoot.EsArchivoContactos(NombreArchivo: string): Boolean;
var
  Nombre: string;
begin
  Nombre := ExtractFileName(LowerCase(NombreArchivo));
  // Verificar si el nombre contiene "contactos" o "contacts"
  Result := (Pos('contactos', Nombre) > 0) or (Pos('contacts', Nombre) > 0);
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

procedure TFormMenuRoot.CargarContactosDesdeJSON(ArchivoJSON: string);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  UsuariosArray: TJSONArray;
  I, J: Integer;
  UsuarioObj: TJSONObject;
  ContactosArray: TJSONArray;
  UsuarioNombre, ContactoUsuario: string;
  UsuarioEncontrado, ContactoEncontrado: TDatoUsuario;
  FileStream: TFileStream;
begin
  // Limpiar grafo existente
  if Assigned(GrafoContactosGlobal) then
    GrafoContactosGlobal.LimpiarGrafo;

  // Cargar y parsear JSON
  FileStream := TFileStream.Create(ArchivoJSON, fmOpenRead);
  try
    JSONData := GetJSON(FileStream);
    try
      JSONObject := TJSONObject(JSONData);

      // Cargar usuarios y contactos
      UsuariosArray := JSONObject.Get('Usuarios', TJSONArray(nil)) as TJSONArray;
      if UsuariosArray <> nil then
      begin
        for I := 0 to UsuariosArray.Count - 1 do
        begin
          UsuarioObj := UsuariosArray.Objects[I];
          UsuarioNombre := UsuarioObj.Get('Usuario', '');

          // Buscar el usuario en la lista global por nombre de usuario
          UsuarioEncontrado := ListaUsuariosGlobal.ObtenerUsuarioPorNombreUsuario(UsuarioNombre);

          if UsuarioEncontrado.Id <> -1 then
          begin
            // Agregar usuario al grafo con datos reales (usar email como identificador)
            GrafoContactosGlobal.AgregarUsuario(
              UsuarioEncontrado.Id,
              UsuarioEncontrado.Nombre,
              UsuarioEncontrado.Email  // Usar email como identificador
            );

            // Agregar contactos
            ContactosArray := UsuarioObj.Get('Contactos', TJSONArray(nil)) as TJSONArray;
            if ContactosArray <> nil then
            begin
              for J := 0 to ContactosArray.Count - 1 do
              begin
                ContactoUsuario := ContactosArray.Strings[J];

                // Buscar el contacto en la lista global por nombre de usuario
                ContactoEncontrado := ListaUsuariosGlobal.ObtenerUsuarioPorNombreUsuario(ContactoUsuario);

                if ContactoEncontrado.Id <> -1 then
                begin
                  // Agregar contacto al grafo si no existe
                  if GrafoContactosGlobal.ObtenerNodoPorUsuario(ContactoEncontrado.Email) = nil then
                  begin
                    GrafoContactosGlobal.AgregarUsuario(
                      ContactoEncontrado.Id,
                      ContactoEncontrado.Nombre,
                      ContactoEncontrado.Email  // Usar email como identificador
                    );
                  end;

                  // Agregar la relación usando emails
                  GrafoContactosGlobal.AgregarContacto(UsuarioEncontrado.Email, ContactoEncontrado.Email);
                end
                else
                begin
                  // Mostrar advertencia si no se encuentra el contacto
                  ShowMessage('Advertencia: Contacto "' + ContactoUsuario + '" no encontrado en el sistema.');
                end;
              end;
            end;
          end
          else
          begin
            // Mostrar advertencia si no se encuentra el usuario
            ShowMessage('Advertencia: Usuario "' + UsuarioNombre + '" no encontrado en el sistema.');
          end;
        end;
      end
      else
      begin
        ShowMessage('El archivo no contiene una sección "Usuarios" válida.');
      end;

    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TFormMenuRoot.GenerarReportesCompletos;
var
  ReportDir: string;
begin
  // Crear directorio de reportes
  ReportDir := 'Reportes-Root';
  if not DirectoryExists(ReportDir) then
    CreateDir(ReportDir);

  try
    // 1. Generar reporte de contactos
    GenerarReporteContactos(ReportDir);

    // 2. Generar reporte de logueo
    GenerarReporteLogueo(ReportDir);

    ShowMessage('Reportes generados exitosamente en:' + sLineBreak +
               ReportDir + sLineBreak +
               'Archivos creados:' + sLineBreak +
               '- reporte_contactos.dot' + sLineBreak +
               '- reporte_contactos.png' + sLineBreak +
               '- reporte_logueo.dot' + sLineBreak +
               '- reporte_logueo.png' + sLineBreak +
               '- logueo.json');

  except
    on E: Exception do
      ShowMessage('Error al generar los reportes: ' + E.Message);
  end;
end;

// Separa el reporte de contactos en un método aparte
procedure TFormMenuRoot.GenerarReporteContactos(ReportDir: string);
var
  DotContent: string;
  DotFileName, PngFileName: string;
  AProcess: TProcess;
begin
  if not Assigned(GrafoContactosGlobal) or (GrafoContactosGlobal.Count = 0) then
  begin
    ShowMessage('No hay contactos cargados en el sistema para generar el reporte.');
    Exit;
  end;

  try
    // Generar contenido DOT
    DotContent := GrafoContactosGlobal.GenerarReporteGraphviz;

    // Guardar archivo .dot
    DotFileName := ReportDir + '/reporte_contactos.dot';
    with TStringList.Create do
    try
      Text := DotContent;
      SaveToFile(DotFileName);
    finally
      Free;
    end;

    // Generar imagen PNG con Graphviz
    PngFileName := ReportDir + '/reporte_contactos.png';
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'dot';
      AProcess.Parameters.Add('-Tpng');
      AProcess.Parameters.Add(DotFileName);
      AProcess.Parameters.Add('-o');
      AProcess.Parameters.Add(PngFileName);
      AProcess.Options := AProcess.Options + [poWaitOnExit];
      AProcess.Execute;

      if AProcess.ExitStatus <> 0 then
      begin
        ShowMessage('Error al generar la imagen PNG de contactos. Verifique que Graphviz esté instalado.');
      end;
    finally
      AProcess.Free;
    end;

  except
    on E: Exception do
      ShowMessage('Error al generar el reporte de contactos: ' + E.Message);
  end;
end;

// Nuevo método para generar reporte de logueo
procedure TFormMenuRoot.GenerarReporteLogueo(ReportDir: string);
var
  DotContent: string;
  DotFileName, PngFileName, JsonFileName: string;
  AProcess: TProcess;
begin
  if not Assigned(ControlLogGlobal) then
  begin
    ShowMessage('No hay datos de logueo para generar el reporte.');
    Exit;
  end;

  try
    // 1. Generar JSON del logueo
    JsonFileName := ReportDir + '/logueo.json';
    with TStringList.Create do
    try
      Text := ControlLogGlobal.ExportarJSON;
      SaveToFile(JsonFileName);
    finally
      Free;
    end;

    // 2. Generar reporte gráfico del logueo
    DotContent := GenerarReporteLogueoGraphviz;

    // Guardar archivo .dot
    DotFileName := ReportDir + '/reporte_logueo.dot';
    with TStringList.Create do
    try
      Text := DotContent;
      SaveToFile(DotFileName);
    finally
      Free;
    end;

    // Generar imagen PNG con Graphviz
    PngFileName := ReportDir + '/reporte_logueo.png';
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'dot';
      AProcess.Parameters.Add('-Tpng');
      AProcess.Parameters.Add(DotFileName);
      AProcess.Parameters.Add('-o');
      AProcess.Parameters.Add(PngFileName);
      AProcess.Options := AProcess.Options + [poWaitOnExit];
      AProcess.Execute;

      if AProcess.ExitStatus <> 0 then
      begin
        ShowMessage('Error al generar la imagen PNG del logueo. Verifique que Graphviz esté instalado.');
      end;
    finally
      AProcess.Free;
    end;

  except
    on E: Exception do
      ShowMessage('Error al generar el reporte de logueo: ' + E.Message);
  end;
end;

// Método para generar el contenido DOT del reporte de logueo
function TFormMenuRoot.GenerarReporteLogueoGraphviz: string;
var
  JSONArray: TJSONArray;
  I: Integer;
  JSONObj: TJSONObject;
begin
  Result := 'digraph Logueo {' + sLineBreak;
  Result := Result + '  rankdir=TB;' + sLineBreak;
  Result := Result + '  node [shape=record, style=filled, color=lightgreen];' + sLineBreak;
  Result := Result + '  edge [color=gray];' + sLineBreak + sLineBreak;

  if not Assigned(ControlLogGlobal) then
  begin
    Result := Result + '  "NoData" [label="No hay datos de logueo"];' + sLineBreak;
    Result := Result + '}' + sLineBreak;
    Exit;
  end;

  // Obtener registros del logueo
  JSONArray := ControlLogGlobal.ObtenerRegistros;
  try
    if JSONArray.Count = 0 then
    begin
      Result := Result + '  "NoData" [label="No hay registros de logueo"];' + sLineBreak;
    end
    else
    begin
      // Crear nodos para cada registro de logueo
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObj := JSONArray.Objects[I];
        Result := Result + '  "Registro' + IntToStr(I) + '" [label="{' +
                   JSONObj.Get('usuario', '') + '|' +
                   'Entrada: ' + JSONObj.Get('entrada', '') + '|' +
                   'Salida: ' + JSONObj.Get('salida', '') + '}"];' + sLineBreak;
      end;

      // Conectar nodos en orden
      Result := Result + sLineBreak + '  // Conexiones temporales' + sLineBreak;
      for I := 0 to JSONArray.Count - 2 do
      begin
        Result := Result + '  "Registro' + IntToStr(I) + '" -> "Registro' + IntToStr(I + 1) + '";' + sLineBreak;
      end;
    end;
  finally
    JSONArray.Free;
  end;

  Result := Result + '}' + sLineBreak;
end;

end.
