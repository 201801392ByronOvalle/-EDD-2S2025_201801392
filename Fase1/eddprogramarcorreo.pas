unit EDDProgramarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils,
  DateTimePicker, EDDEstructuras, EDDGlobal, EDDCorreos;

type

  { TfrmProgramarCorreo }

  TfrmProgramarCorreo = class(TForm)
    btnProgramar: TButton;
    btnCancelar: TButton;
    dtpFechaHora: TDateTimePicker;
    edtDestinatario: TEdit;
    edtAsunto: TEdit;
    lblFechaHora: TLabel;
    lblMensaje: TLabel;
    lblAsunto: TLabel;
    lblDestinatario: TLabel;
    lblTitulo: TLabel;
    memMensaje: TMemo;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     UsuarioActual: PUsuario;
     function GenerarNuevoIDCorreo: Integer;
   public
     procedure SetUsuarioActual(usuario: PUsuario);
   end;

var
  frmProgramarCorreo: TfrmProgramarCorreo;

implementation

{$R *.lfm}

{ TfrmProgramarCorreo }

procedure TfrmProgramarCorreo.SetUsuarioActual(usuario: PUsuario);
begin
  UsuarioActual := usuario;
end;

procedure TfrmProgramarCorreo.FormShow(Sender: TObject);
begin
  // Configurar fecha/hora
  dtpFechaHora.DateTime := IncHour(Now, 1);
  edtAsunto.Text := '';
  edtDestinatario.Text := '';
  memMensaje.Lines.Clear;
end;

function TfrmProgramarCorreo.GenerarNuevoIDCorreo: Integer;
var
  maxID: Integer;
  actual: PNodoCorreo;
begin
  maxID := 0;
  actual := ColaCorreosGlobal.frente;

  while actual <> nil do
  begin
    if actual^.dato^.id > maxID then
      maxID := actual^.dato^.id;
    actual := actual^.siguiente;
  end;

  Result := maxID + 1;
end;

procedure TfrmProgramarCorreo.btnProgramarClick(Sender: TObject);
var
  nuevoCorreo: PCorreo;
  destinatario, asunto, mensaje: string;
  fechaHora: TDateTime;
begin
  if UsuarioActual = nil then Exit;

  destinatario := Trim(edtDestinatario.Text);
  asunto := Trim(edtAsunto.Text);
  mensaje := Trim(memMensaje.Text);
  fechaHora := dtpFechaHora.DateTime;

  // Validaciones
  if destinatario = '' then
  begin
    ShowMessage('Ingrese el destinatario');
    edtDestinatario.SetFocus;
    Exit;
  end;

  if asunto = '' then
  begin
    ShowMessage('Ingrese el asunto');
    edtAsunto.SetFocus;
    Exit;
  end;

  if mensaje = '' then
  begin
    ShowMessage('Ingrese el mensaje');
    memMensaje.SetFocus;
    Exit;
  end;

  if fechaHora <= Now then
  begin
    ShowMessage('La fecha/hora debe ser futura');
    dtpFechaHora.SetFocus;
    Exit;
  end;

  // Verificar si el destinatario existe en el sistema
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, destinatario) = nil then
  begin
    ShowMessage('El destinatario no existe en el sistema');
    edtDestinatario.SetFocus;
    Exit;
  end;

  // Crear y encolar correo
  nuevoCorreo := CrearCorreo(
    GenerarNuevoIDCorreo,
    UsuarioActual^.email,
    destinatario,
    asunto,
    mensaje,
    fechaHora
  );

  EncolarCorreo(ColaCorreosGlobal, nuevoCorreo);

  ShowMessage('Correo programado exitosamente para: ' + DateTimeToStr(fechaHora));
  ModalResult := mrOk;
end;

procedure TfrmProgramarCorreo.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

