unit EDDGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EDDEstructuras, EDDContactos, EDDCorreos,
  EDDMatrizRelaciones;

var
  // Lista global de usuarios para todo el proyecto
  ListaUsuariosGlobal: TListaUsuarios;

  // Lista global de contactos del usuario actual
  ListaContactosGlobal: TListaContactos;

  // Lista global de correos programados
  ColaCorreosGlobal: TColaCorreos;

  // Matriz global de relaciones
  MatrizRelacionesGlobal: TMatrizRelaciones;

implementation

initialization
// Inicializar las listas global al iniciar la aplicacion
InicializarListaUsuarios(ListaUsuariosGlobal);
InicializarListaContactos(ListaContactosGlobal);
InicializarColaCorreos(ColaCorreosGlobal)
InicializarMatrizRelaciones(MatrizRelacionesGlobal);

end.

