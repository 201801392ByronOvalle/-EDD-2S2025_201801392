unit EDDGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EDDEstructuras, EDDContactos, EDDCorreos;

var
  // Lista global de usuarios para todo el proyecto
  ListaUsuariosGlobal: TListaUsuarios;

  // Lista global de contactos del usuario actual
  ListaContactosGlobal: TListaContactos;

  // Lista global de correos programados
  ColaCorreosGlobal: TColaCorreos;

implementation

initialization
// Inicializar las listas global al iniciar la aplicacion
InicializarListaUsuarios(ListaUsuariosGlobal);
InicializarListaContactos(ListaContactosGlobal);
InicializarColaCorreos(ColaCorreosGlobal)

end.

