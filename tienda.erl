% Marco Antonio Lucio Sosa A01285589
% Gabriel Ernesto Mujica Proulx
% Germán Salas
% Alejandro Charles

-module(tienda).
-export([abre_tienda/0, cierra_tienda/0, registra_producto/2, elimina_producto/1, modifica_producto/2, listar_existencias/0, lista_socios/0, productos_vendidos/0]).

abre_tienda() -> 
    register(tienda, spawn(fun() -> tienda_loop([], [], [], 0) end)),
    io:format("Tienda iniciada~n").

cierra_tienda() ->
    tienda ! exit.

registra_producto(Producto, Cantidad) ->
    Caller = self(),
    io:format("Manda: ~p solicita registrar producto con cantidad ~p~n", [Producto, Cantidad]),
    tienda ! {registra_producto, Producto, Cantidad, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

elimina_producto(Producto) ->
    Caller = self(),
    io:format("Manda: ~p solicita eliminar producto~n", [Producto]),
    tienda ! {elimina_producto, Producto, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

modifica_producto(Producto, Cantidad) ->
    Caller = self(),
    io:format("Manda: ~p solicita modificar producto con cantidad ~p~n", [Producto, Cantidad]),
    tienda ! {modifica_producto, Producto, Cantidad, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

listar_existencias() ->
    Caller = self(),
    io:format("Solicita lista de existencias~n"),
    tienda ! {listar_existencias, Caller},
    receive
        {ok, Existencias} -> {ok, Existencias};
        {error, Reason} -> {error, Reason}
    end.

lista_socios() ->
    Caller = self(),
    io:format("Solicita lista de socios~n"),
    tienda ! {lista_socios, Caller},
    receive
        {ok, Socios} -> {ok, Socios};
        {error, Reason} -> {error, Reason}
    end.

productos_vendidos() ->
    Caller = self(),
    io:format("Solicita lista de productos vendidos~n"),
    tienda ! {productos_vendidos, Caller},
    receive
        {ok, ProductosVendidos} -> {ok, ProductosVendidos};
        {error, Reason} -> {error, Reason}
    end.

tienda_loop(Socios, Productos, Pedidos, ContadorPedidos) ->
    receive
        exit ->
            io:format("Tienda cerrada~n"),
            terminate_productos(Productos),
            unregister(tienda),
            exit(normal);
        {registra_producto, Producto, Cantidad, Caller} -> 
            io:format("Recibe: solicitud de registrar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
            Pid = spawn(fun() -> producto_loop(Producto, Cantidad) end),
            Caller ! {ok, Producto},
            tienda_loop(Socios, [{Producto, Pid} | Productos], Pedidos, ContadorPedidos);
        {elimina_producto, Producto, Caller} ->
            io:format("Recibe: solicitud de eliminar producto ~p~n", [Producto]),
            case lists:keyfind(Producto, 1, Productos) of
                false ->
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                {Producto, Pid} ->
                    exit(Pid, kill),
                    Caller ! {ok, Producto},
                    tienda_loop(Socios, lists:keydelete(Producto, 1, Productos), Pedidos, ContadorPedidos)
            end;
        {modifica_producto, Producto, Cantidad, Caller} ->
            io:format("Recibe: solicitud de modificar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
            case lists:keyfind(Producto, 1, Productos) of
                false ->
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                {Producto, Pid} ->
                    Pid ! {modificar, Cantidad},
                    Caller ! {ok, Producto},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
            end;
        {listar_existencias, Caller} ->
            io:format("Recibe: solicitud de lista de existencias~n"),
            Existencias = [{Producto, get_cantidad(Pid)} || {Producto, Pid} <- Productos],
            Caller ! {ok, Existencias},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
        {suscribir_socio, Socio, Caller} ->
            io:format("Recibe: solicitud de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                true ->
                    Caller ! {error, already_exists},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                false ->
                    Caller ! {ok, Socio},
                    tienda_loop([Socio | Socios], Productos, Pedidos, ContadorPedidos)
            end;
        {elimina_socio, Socio, Caller} ->
            io:format("Recibe: solicitud de eliminación de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                true ->
                    Caller ! {ok, Socio},
                    tienda_loop(lists:delete(Socio, Socios), Productos, Pedidos, ContadorPedidos);
                false ->
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
            end;
        {crea_pedido, Socio, ListaDeProductos, Caller} ->
            io:format("Recibe: solicitud de creación de pedido de ~p con productos ~p~n", [Socio, ListaDeProductos]),
            case lists:member(Socio, Socios) of
                false ->
                    Caller ! {error, not_a_member},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                true ->
                    NuevoContadorPedidos = ContadorPedidos + 1,
                    Pedido = {NuevoContadorPedidos, Socio, ListaDeProductos},
                    Caller ! {ok, Pedido},
                    tienda_loop(Socios, Productos, [Pedido | Pedidos], NuevoContadorPedidos)
            end;
        {lista_socios, Caller} ->
            io:format("Recibe: solicitud de lista de socios~n"),
            Caller ! {ok, Socios},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
        {productos_vendidos, Caller} ->
            io:format("Recibe: solicitud de productos vendidos~n"),
            Caller ! {ok, Pedidos},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
    end.

terminate_productos([]) -> ok;
terminate_productos([{_, Pid} | T]) ->
    exit(Pid, kill),
    terminate_productos(T).

producto_loop(Producto, Cantidad) ->
    receive
        {modificar, NuevaCantidad} ->
            io:format("Producto ~p modificado a ~p~n", [Producto, NuevaCantidad]),
            producto_loop(Producto, max(0, Cantidad + NuevaCantidad));
        {get_cantidad, Caller} ->
            Caller ! {cantidad, Cantidad},
            producto_loop(Producto, Cantidad);
        stop ->
            io:format("Producto ~p detenido~n", [Producto]),
            exit(normal)
    end.

get_cantidad(Pid) ->
    Pid ! {get_cantidad, self()},
    receive
        {cantidad, Cantidad} -> Cantidad
    end.
