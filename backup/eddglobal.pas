unit EDDGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EDDEstructuras;

var
  // Lista global de usuarios para todo el proyecto
  ListaUsuariosGlobal: TListaUsuarios;

implementation

initialization
// Inicializar la lista global al iniciar la aplicacion
InicializarListaUsuarios(ListaUsuariosGlobal);

end.

