program EDDMail_Fase1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, EDDMail, EDDEstructuras, EDDRegistro, EDDMenuRoot,
  EDDMenuUsuario, EDDGlobal, EDDContactos, EDDAgregarContacto, EDDVerContactos,
  EDDActualizarPerfil, EDDCorreos, EDDProgramarCorreo, EDDVerCorreosProgramados,
  EDDMatrizRelaciones;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmRegistro, frmRegistro);
  Application.CreateForm(TfrmMenuRoot, frmMenuRoot);
  Application.CreateForm(TfrmMenuUsuario, frmMenuUsuario);
  Application.CreateForm(TfrmAgregarContacto, frmAgregarContacto);
  Application.CreateForm(TfrmVerContactos, frmVerContactos);
  Application.CreateForm(TfrmActualizarPerfil, frmActualizarPerfil);
  Application.CreateForm(TfrmProgramarCorreo, frmProgramarCorreo);
  Application.CreateForm(TfrmVerCorreosProgramados, frmVerCorreosProgramados);
  Application.Run;
end.

