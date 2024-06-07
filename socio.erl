% Actividad Integradora 6.2
% Modulo Socio

% Marco Antonio Lucio Sosa
% Gabriel Ernesto Mujica Proulx
% Germán Salas
% Alejandro Charles

-module(socio).
-export([suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0]).

% Se agrega un nuevo socio a la lista
suscribir_socio(Socio) ->
    Caller = self(),
    io:format("Manda: ~p solicita suscripción~n", [Socio]),
    tienda ! {suscribir_socio, Socio, Caller}, % Se comunica con el proceso tienda
    receive
        {ok, Socio} -> {ok, Socio};
        {error, Reason} -> {error, Reason}
    end.

% Se elimina un socio seleccionado de la lista de socios
elimina_socio(Socio) ->
    Caller = self(),
    io:format("Manda: ~p solicita eliminación de suscripción~n", [Socio]),
    tienda ! {elimina_socio, Socio, Caller}, % Se comunica con el proceso tienda para realizar el cambio
    receive
        {ok, Socio} -> {ok, Socio};
        {error, Reason} -> {error, Reason}
    end.

% Se crea un pedido de productos, con una lista de tuplas que incluyen producto y cantidad
crea_pedido(Socio, ListaDeProductos) ->
    Caller = self(),
    io:format("Manda: ~p crea pedido con productos ~p~n", [Socio, ListaDeProductos]),
    tienda ! {crea_pedido, Socio, ListaDeProductos, Caller},
    receive
        {ok, Pedido} -> {ok, Pedido};
        {error, Reason} -> {error, Reason}
    end. 

% Se manda a imprimir una lista de los productos disponibles y sus cantidades
lista_existencias() ->
    Caller = self(),
    io:format("Manda: solicita lista de existencias~n"),
    tienda ! {listar_existencias, Caller},
    receive
        {ok, Existencias} -> {ok, Existencias};
        {error, Reason} -> {error, Reason}
    end.
