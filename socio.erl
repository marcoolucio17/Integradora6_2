-module(socio).
-export([suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0]).

suscribir_socio(Socio) ->
    Caller = self(),
    io:format("Manda: ~p solicita suscripción~n", [Socio]),
    tienda ! {suscribir_socio, Socio, Caller},
    receive
        {ok, Socio} -> {ok, Socio};
        {error, Reason} -> {error, Reason}
    end.

elimina_socio(Socio) ->
    Caller = self(),
    io:format("Manda: ~p solicita eliminación de suscripción~n", [Socio]),
    tienda ! {elimina_socio, Socio, Caller},
    receive
        {ok, Socio} -> {ok, Socio};
        {error, Reason} -> {error, Reason}
    end.

crea_pedido(Socio, ListaDeProductos) ->
    Caller = self(),
    io:format("Manda: ~p crea pedido con productos ~p~n", [Socio, ListaDeProductos]),
    tienda ! {crea_pedido, Socio, ListaDeProductos, Caller},
    receive
        {ok, Pedido} -> {ok, Pedido};
        {error, Reason} -> {error, Reason}
    end. 

lista_existencias() ->
    Caller = self(),
    io:format("Manda: solicita lista de existencias~n"),
    tienda ! {listar_existencias, Caller},
    receive
        {ok, Existencias} -> {ok, Existencias};
        {error, Reason} -> {error, Reason}
    end.
