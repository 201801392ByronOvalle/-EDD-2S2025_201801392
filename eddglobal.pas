unit EDDGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EDDEstructuras, EDDContactos;

var
  // Lista global de usuarios para todo el proyecto
  ListaUsuariosGlobal: TListaUsuarios;

  // Lista global de contactos del usuario actual
  ListaContactosGlobal: TListaContactos;

implementation

initialization
// Inicializar las listas global al iniciar la aplicacion
InicializarListaUsuarios(ListaUsuariosGlobal);
InicializarListaContactos(ListaContactosGlobal);

end.

