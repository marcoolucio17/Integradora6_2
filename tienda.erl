% Marco Antonio Lucio Sosa A01285589
% Gabriel Ernesto Mujica Proulx
% Germán Salas
% Alejandro Charles


-module(tienda).
-export([abre_tienda/0, cierra_tienda/0, registra_producto/2, elimina_producto/1, modifica_producto/2, listar_existencias/0]).

% Registramos el producto
registra_producto(Producto, Cantidad) ->
    Caller = self(),
    io:format("Manda: ~p solicita registrar producto con cantidad ~p~n", [Producto, Cantidad]),
    tienda ! {registra_producto, Producto, Cantidad, Caller}, % Include the caller in the message    
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

% Eliminamos el producto
elimina_producto(Producto) ->
    Caller = self(),
    io:format("Manda: ~p solicita eliminar producto~n", [Producto]),
    tienda ! {elimina_producto, Producto, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


% Modificamos cierto producto en su cantidad
modifica_producto(Producto, Cantidad) ->
    Caller = self(),
    io:format("Manda: ~p solicita modificar producto con cantidad ~p~n", [Producto, Cantidad]),
    tienda ! {modifica_producto, Producto, Cantidad, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

% Mandamos a imprimir la lista de todos los productos en existencia
listar_existencias() ->
    Caller = self(),
    io:format("Solicita lista de existencias~n"),
    tienda ! {listar_existencias, Caller},
    receive
        {ok, Existencias} -> {ok, Existencias};
        {error, Reason} -> {error, Reason}
    end.


% Se registra el proceso tienda
abre_tienda() -> 
    register(tienda, spawn(fun() -> tienda_loop([], [], [], 0) end)),
    io:format("Tienda iniciada~n").

% Detenemos la tienda
cierra_tienda() ->
    tienda ! exit.

% Loop del proceso tienda
tienda_loop(Socios, Productos, Pedidos, ContadorPedidos) ->
    receive
        % Detenemos el proceso
        exit ->
            io:format("Tienda cerrada~n"),
            terminate_productos(Productos), % borramos a todos los productos
            unregister(tienda),
            exit(normal);
        % Creamos el proceso de producto y registramos un producto con cierta cantidad
        {registra_producto, Producto, Cantidad, Caller} -> 
            io:format("Recibe: solicitud de registrar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
            Pid = spawn(fun() -> producto_loop(Producto) end), % Creamos el proceso y guardamos le pid
            Caller ! {ok, Producto}, % Send the message to the caller
            tienda_loop(Socios, [{Producto, Pid } | Productos], Pedidos, ContadorPedidos); % Guardamos el producto y su Pid en la lista
        % Eliminamos cierto producto de nuestras existencias
        {elimina_producto, Producto, Caller} ->
            io:format("Recibe: solicitud de eliminar producto ~p~n", [Producto]),
            % Buscamos el producto en nuestro inventario
            case lists:keyfind(Producto, 1, Productos) of
                false ->
                    % De NO encontrarlo, se lo decimos al proceso y continuamos
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                {Producto, Pid} ->
                    % De encontrarlo, terminamos dicho proceso y continuamos
                    exit(Pid, kill),
                    tienda ! {ok, Producto},
                    tienda_loop(Socios, lists:keydelete(Producto, 1, Productos), Pedidos, ContadorPedidos)
            end;
        % En base a cierta cantidad, modificamos un mensaje a cierto Pid especifico de modificar su cantidad
        {modifica_producto, Producto, Cantidad, Caller} ->
            io:format("Recibe: solicitud de modificar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
            case lists:keyfind(Producto, 1, Productos) of
                % Si el producto NO existe, mandamos dicho mensaje y reiniciamos el loop
                false ->
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                % De existir, mandamos a modificar dicha cantidad
                {Producto, Pid} ->
                    Pid ! {modificar, Cantidad},
                    Caller ! {ok, Producto},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
            end;
        % Imprimimos los productos existentes
        {listar_existencias, Caller} ->
            io:format("Recibe: solicitud de lista de existencias~n"),
            Existencias = [Producto || {Producto, _Pid} <- Productos],
            Caller ! {ok, Existencias},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
    end.

% Terminar procesos de productos
terminate_productos([]) -> ok; % Si no hay nada, regresamos ok
% Si existen procesos de productos aún, los eliminamos
terminate_productos([{_, Pid} | T]) ->
    exit(Pid, kill),
    terminate_productos(T).

% Bucle del proceso del producto
producto_loop(Producto) ->
    receive
        {modificar, NuevaCantidad} ->
            io:format("Producto ~p modificado a ~p~n", [Producto, NuevaCantidad]),
            producto_loop(Producto);
        stop ->
            io:format("Producto ~p detenido~n", [Producto]),
            exit(normal)
    end.