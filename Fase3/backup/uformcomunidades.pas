unit UFormComunidades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UArbolComunidades;

type

  { TfrmComunidades }

  TfrmComunidades = class(TForm)
    btnCrearComunidad: TButton;
    btnVolver: TButton;
    edtNombreComunidad: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnCrearComunidadClick(Sender: TObject);
    procedure btnVolverClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure MostrarComunidadesExistentes;
  public
  end;

var
  frmComunidades: TfrmComunidades;

implementation

{$R *.lfm}

{ TfrmComunidades }

procedure TfrmComunidades.FormShow(Sender: TObject);
begin
  Caption := 'EDDMail - Gestión de Comunidades';
  Label1.Caption := 'Gestión de Comunidades';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];

  Label2.Caption := 'Crear Comunidad';
  Label3.Caption := 'Nombre de la comunidad:';
  btnCrearComunidad.Caption := 'Crear Comunidad';
  btnVolver.Caption := 'Volver';

  MostrarComunidadesExistentes;
end;

procedure TfrmComunidades.btnCrearComunidadClick(Sender: TObject);
var
  NombreComunidad: string;
begin
  NombreComunidad := Trim(edtNombreComunidad.Text);

  if NombreComunidad = '' then
  begin
    ShowMessage('Por favor, ingrese un nombre para la comunidad.');
    Exit;
  end;

  if ArbolComunidadesGlobal.ExisteComunidad(NombreComunidad) then
  begin
    ShowMessage('La comunidad "' + NombreComunidad + '" ya existe.');
    Exit;
  end;

  // Crear la comunidad
  ArbolComunidadesGlobal.InsertarComunidad(NombreComunidad);
  ShowMessage('Comunidad "' + NombreComunidad + '" creada exitosamente.');

  edtNombreComunidad.Text := '';
  MostrarComunidadesExistentes;
end;

procedure TfrmComunidades.btnVolverClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmComunidades.MostrarComunidadesExistentes;
var
  Comunidades: TStringList;
begin
  Comunidades := ArbolComunidadesGlobal.ObtenerComunidades;
  try
    if Comunidades.Count > 0 then
    begin
      ShowMessage('Comunidades existentes:' + sLineBreak + Comunidades.Text);
    end
    else
    begin
      ShowMessage('No hay comunidades creadas.');
    end;
  finally
    Comunidades.Free;
  end;
end;

end.
