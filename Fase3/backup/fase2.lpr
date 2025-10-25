program fase2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ProyectoF2, UMenuRoot, UListaSimple, UColaCorreos, UMenuUsuario,
  UBandejaEntrada, UControlLog, UFormControlLog, UGrafoContactos,
  UFormAgregarContacto, UFormContactos, UArbolComunidades, UFormComunidades,
  UFormPublicarMensaje, UFormVerMensajesComunidad;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormMenuRoot, FormMenuRoot);
  Application.CreateForm(TFormMenuUsuario, FormMenuUsuario);
  Application.CreateForm(TFormBandejaEntrada, FormBandejaEntrada);
  Application.CreateForm(TfrmControlLogeo, frmControlLogeo);
  Application.CreateForm(TfrmAgregarContacto, frmAgregarContacto);
  Application.CreateForm(TfrmContactos, frmContactos);
  Application.CreateForm(TfrmComunidades, frmComunidades);
  Application.CreateForm(TfrmPublicarMensaje, frmPublicarMensaje);
  Application.Run;
end.

