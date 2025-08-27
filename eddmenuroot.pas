unit EDDMenuRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil,
  Process, EDDEstructuras, EDDGlobal;

type

  { TfrmMenuRoot }

  TfrmMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    btnSalir: TButton;
    lblTitulo: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMenuRoot: TfrmMenuRoot;

implementation

{$R *.lfm}

{ TfrmMenuRoot }

procedure TfrmMenuRoot.FormCreate(Sender: TObject);
begin
  // Configurar de los Dialogs
  OpenDialog := TOpenDialog.Create(Self);
  OpenDialog.Title := 'Seleccionar archivo JSON';
  OpenDialog.Filter := 'Archivos JSON (*.json)|*.json|Todos los archivos (*.*)|*.*';
  OpenDialog.DefaultExt := 'json';

  SaveDialog := TSaveDialog.Create(Self);
  SaveDialog.Title := 'Guardar reporte Graphviz';
  SaveDialog.Filter := 'Archivos DOT (*.dot)|*.dot|Todos los archivos (*.*)|*.*';
  SaveDialog.DefaultExt := 'dot';
  SaveDialog.FileName := 'reporte_usuarios.dot';
end;

procedure TfrmMenuRoot.btnCargaMasivaClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if CargarUsuariosDesdeJSON(ListaUsuariosGlobal, OpenDialog.FileName) then
    begin
      ShowMessage('Carga masiva completada exitosamente');
    end;
  end;
end;

procedure TfrmMenuRoot.btnReporteRelacionesClick(Sender: TObject);
begin
  ShowMessage('Generando reporte de relaciones...');
  // Lgica del reporte de relaciones
end;

procedure TfrmMenuRoot.btnReporteUsuariosClick(Sender: TObject);
var
  outputPath, pngPath: string;
  AProcess: TProcess;
begin
  // Carpeta Root-Reportes si no existe
  if not DirectoryExists('Root-Reportes') then
    CreateDir('Root-Reportes');

  // Ruta por defecto
  SaveDialog.FileName := 'Root-Reportes/usuarios.dot';
  if SaveDialog.Execute then
  begin
    outputPath := SaveDialog.FileName;
    if GenerarReporteUsuariosGraphviz(ListaUsuariosGlobal, outputPath) then
    begin
      // Definir el nombre del PNG
      pngPath := ChangeFileExt(outputPath, '.png');

      // Ejecutar Graphviz automáticamente
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := 'dot';
        AProcess.Parameters.Add('-Tpng');
        AProcess.Parameters.Add(outputPath);
        AProcess.Parameters.Add('-o');
        AProcess.Parameters.Add(pngPath);
        AProcess.Options := [poWaitOnExit]; // Esperar a que termine
        AProcess.Execute;

        ShowMessage('Reporte generado correctamente en:' + LineEnding +
                    outputPath + LineEnding);
      finally
        AProcess.Free;
      end;
    end;
  end;
end;

procedure TfrmMenuRoot.btnSalirClick(Sender: TObject);
begin
  Close;
end;

end.

