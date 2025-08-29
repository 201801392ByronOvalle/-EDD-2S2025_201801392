unit EDDMenuUsuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process,
  EDDEstructuras, EDDGlobal, EDDContactos, EDDAgregarContacto, EDDVerContactos,
  EDDActualizarPerfil, EDDProgramarCorreo,EDDCorreos, EDDVerCorreosProgramados;

type

  { TfrmMenuUsuario }

  TfrmMenuUsuario = class(TForm)
    btnBandejaEntrada: TButton;
    btnSalir: TButton;
    btnEnviarCorreo: TButton;
    btnPapelera: TButton;
    btnProgramarCorreo: TButton;
    btnCorreosProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnActualizarPerfil: TButton;
    btnReportes: TButton;
    lblSaludo: TLabel;
    lblTitulo: TLabel;
    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnCorreosProgramadosClick(Sender: TObject);
    procedure btnEnviarCorreoClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    UsuarioActual: PUsuario;
  public
    procedure SetDatosUsuario(usuario: PUsuario);
  end;

var
  frmMenuUsuario: TfrmMenuUsuario;

implementation

{$R *.lfm}

{ TfrmMenuUsuario }

procedure TfrmMenuUsuario.FormShow(Sender: TObject);
begin
  // Personalizar el saludo con el nombre del usuario
  if UsuarioActual <> nil then
    lblSaludo.Caption := '¡Hola, ' + UsuarioActual^.nombre + '!';
end;

procedure TfrmMenuUsuario.SetDatosUsuario(usuario: PUsuario);
begin
  UsuarioActual := usuario;
end;

procedure TfrmMenuUsuario.btnBandejaEntradaClick(Sender: TObject);
begin
  ShowMessage('Bandeja de entrada - Pendiente de implementar');
end;

procedure TfrmMenuUsuario.btnEnviarCorreoClick(Sender: TObject);
begin
  ShowMessage('Enviar correo - Pendiente de implementar');
end;

procedure TfrmMenuUsuario.btnPapeleraClick(Sender: TObject);
begin
  ShowMessage('Papelera - Pendiente de implementar');
end;

procedure TfrmMenuUsuario.btnProgramarCorreoClick(Sender: TObject);
var
  formProgramarCorreo: TfrmProgramarCorreo;
begin
  formProgramarCorreo := TfrmProgramarCorreo.Create(nil);
  try
    formProgramarCorreo.SetUsuarioActual(UsuarioActual);
    if formProgramarCorreo.ShowModal = mrOk then
    begin
      ShowMessage('Correo programado correctamente');
    end;
  finally
    formProgramarCorreo.Free;
  end;
end;

procedure TfrmMenuUsuario.btnCorreosProgramadosClick(Sender: TObject);
var
  formVerCorreosProgramados: TfrmVerCorreosProgramados;
begin
  formVerCorreosProgramados := TfrmVerCorreosProgramados.Create(nil);
  try
    formVerCorreosProgramados.SetUsuarioActual(UsuarioActual);
    formVerCorreosProgramados.ShowModal;
  finally
    formVerCorreosProgramados.Free;
  end;
end;

procedure TfrmMenuUsuario.btnAgregarContactoClick(Sender: TObject);
var
  formAgregarContacto: TfrmAgregarContacto;
begin
  formAgregarContacto := TfrmAgregarContacto.Create(nil);
  try
    formAgregarContacto.ShowModal;
  finally
    formAgregarContacto.Free;
  end;
end;

procedure TfrmMenuUsuario.btnContactosClick(Sender: TObject);
var
  formVerContactos: TfrmVerContactos;
begin
  formVerContactos := TfrmVerContactos.Create(nil);
  try
    formVerContactos.ShowModal;
  finally
    formVerContactos.Free;
  end;
end;

procedure TfrmMenuUsuario.btnActualizarPerfilClick(Sender: TObject);
var
  formActualizarPerfil: TfrmActualizarPerfil;
begin
  formActualizarPerfil := TfrmActualizarPerfil.Create(nil);
  try
    formActualizarPerfil.SetUsuarioActual(UsuarioActual);
    if formActualizarPerfil.ShowModal = mrOk then
    begin
      ShowMessage('Perfil actualizado correctamente');
      // Actualizar el saludo por si cambió el nombre de usuario
      lblSaludo.Caption := '¡Hola, ' + UsuarioActual^.nombre + '!';
    end;
  finally
    formActualizarPerfil.Free;
  end;
end;

procedure TfrmMenuUsuario.btnReportesClick(Sender: TObject);
var
  carpetaReportes, archivoContactos, archivoCorreos: string;
  AProcess: TProcess;
begin
  // Crear carpeta de reportes del usuario
  carpetaReportes := UsuarioActual^.usuario + '-Reportes';
  if not DirectoryExists(carpetaReportes) then
    CreateDir(carpetaReportes);

  // 1. Reporte de Contactos
  if ListaContactosGlobal.tamanio > 0 then
  begin
    archivoContactos := carpetaReportes + '/reporte_contactos.dot';
    if GenerarReporteContactosGraphviz(ListaContactosGlobal, archivoContactos) then
    begin
      // Generar PNG automáticamente
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := 'dot';
        AProcess.Parameters.Add('-Tpng');
        AProcess.Parameters.Add(archivoContactos);
        AProcess.Parameters.Add('-o');
        AProcess.Parameters.Add(carpetaReportes + '/reporte_contactos.png');
        AProcess.Options := [poWaitOnExit, poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end;
  end;

   // 2. Reporte de Correos Programados
  if not ColaVacia(ColaCorreosGlobal) then
  begin
    archivoCorreos := carpetaReportes + '/reporte_correos_programados.dot';
    if GenerarReporteCorreosProgramadosGraphviz(ColaCorreosGlobal, archivoCorreos) then
    begin
      // Generar PNG automaticamente
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := 'dot';
        AProcess.Parameters.Add('-Tpng');
        AProcess.Parameters.Add(archivoCorreos);
        AProcess.Parameters.Add('-o');
        AProcess.Parameters.Add(carpetaReportes + '/reporte_correos_programados.png');
        AProcess.Options := [poWaitOnExit, poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end;
  end;

  // 3. Reporte de Papelera
  // 4. Reporte de Correos Programados

  ShowMessage('Reportes generados en la carpeta: ' + carpetaReportes);
end;

procedure TfrmMenuUsuario.btnSalirClick(Sender: TObject);
begin
  Close;
end;

end.
