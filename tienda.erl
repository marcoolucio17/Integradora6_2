% Actividad Integradora 6.2
% Modulo Tienda

% Marco Antonio Lucio Sosa
% Gabriel Ernesto Mujica Proulx
% Germán Salas
% Alejandro Charles

% Para inicializar el programa, es necesario primero llamar la funcion "tienda:abre_tienda()", que comienza el proceso de la tienda.
% Para poder probar el resto de los procesos, tambien es necesario registrar un socio con "socio:suscribir_socio()", y registrar productos a la 
% tienda con "tienda:registra_producto(producto, cantidad)". El resto de las funciones sirven para modificar la informacion ya sea de los socios, 
% sus pedidos, o los productos, que incluyen su eliminacion, registro, modificacion, o consulta. Tambien es importante cargar el modulo de este archivo, 
% "tienda" en la terminal, junto con el de "socio" del otro archivo, antes de utilizar cualquiera de los procesos. 

-module(tienda).
-export([abre_tienda/0, cierra_tienda/0, registra_producto/2, elimina_producto/1, modifica_producto/2, listar_existencias/0, lista_socios/0, productos_vendidos/0]).

% Inicializamos el proceso de la tienda
abre_tienda() -> 
    register(tienda, spawn(fun() -> tienda_loop([], [], [], 0) end)),
    io:format("Tienda iniciada~n").

% Se manda mensaje para terminar la tienda
cierra_tienda() ->
    tienda ! exit.

    
% Registramos el producto
registra_producto(Producto, Cantidad) ->
    Caller = self(),
    io:format("Manda: Se solicita registrar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    tienda ! {registra_producto, Producto, Cantidad, Caller},
    receive
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

% Se elimina el producto ingresado
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
    io:format("Manda: ~p solicita modificar producto por cantidad ~p~n", [Producto, Cantidad]),
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

% Se manda a imprimir una lista de todos los socios suscritos
lista_socios() ->
    Caller = self(),
    io:format("Solicita lista de socios~n"),
    tienda ! {lista_socios, Caller},
    receive
        {ok, Socios} -> {ok, Socios};
        {error, Reason} -> {error, Reason}
    end.

% Se manda a imprimir una lista con todos los pedidos realizados
productos_vendidos() ->
    Caller = self(),
    io:format("Solicita lista de productos vendidos~n"),
    tienda ! {productos_vendidos, Caller},
    receive
        {ok, ProductosVendidos} -> {ok, ProductosVendidos};
        {error, Reason} -> {error, Reason}
    end.

% Loop del proceso tienda
tienda_loop(Socios, Productos, Pedidos, ContadorPedidos) ->
    receive
        % Se detiene el proceso
        exit ->
            io:format("Tienda cerrada~n"),
            terminate_productos(Productos), % Se detienen todos los procesos de productos
            unregister(tienda),
            exit(normal);
        % Creamos el proceso de producto y registramos un producto con cierta cantidad
        {registra_producto, Producto, Cantidad, Caller} -> 
            io:format("Recibe: solicitud de registrar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
            Pid = spawn(fun() -> producto_loop(Producto, Cantidad) end), % Creamos el proceso y guardamos el pid
            Caller ! {ok, Producto},
            tienda_loop(Socios, [{Producto, Pid} | Productos], Pedidos, ContadorPedidos); % El producto y su pid se guarda en la lista
        % Se elimina el producto seleccionado de existencias
        {elimina_producto, Producto, Caller} ->
            io:format("Recibe: solicitud de eliminar producto ~p~n", [Producto]),
            % Se busca el producto en el inventario, validando que existe
            case lists:keyfind(Producto, 1, Productos) of
                false ->
                    % En caso de NO encontrarlo, se le informa al proceso y reinicia
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                {Producto, Pid} ->
                    % En caso de encontrarlo, se elimina
                    exit(Pid, kill),
                    Caller ! {ok, Producto},
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
        % Se imprimen los productos existentes
        {listar_existencias, Caller} ->
            io:format("Recibe: solicitud de lista de existencias~n"),
            Existencias = [{Producto, get_cantidad(Pid)} || {Producto, Pid} <- Productos],
            Caller ! {ok, Existencias},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
        % Se agrega un socio por su nombre a la lista de socios
        {suscribir_socio, Socio, Caller} ->
            io:format("Recibe: solicitud de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                % Si el socio ya existe, marca error
                true ->
                    Caller ! {error, already_exists},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                % Si no, se agrega a la lista
                false ->
                    Caller ! {ok, Socio},
                    tienda_loop([Socio | Socios], Productos, Pedidos, ContadorPedidos)
            end;
        % Se elimina un socio seleccionado de la lista de socios
        {elimina_socio, Socio, Caller} ->
            io:format("Recibe: solicitud de eliminación de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                % Se valida la existencia del socio en la lista
                true ->
                    Caller ! {ok, Socio},
                    tienda_loop(lists:delete(Socio, Socios), Productos, Pedidos, ContadorPedidos);
                false ->
                    Caller ! {error, not_found},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
            end;
        % Se crea un pedido de un socio, con una lista de tuplas
        {crea_pedido, Socio, ListaDeProductos, Caller} ->
            io:format("Recibe: solicitud de creación de pedido de ~p con productos ~p~n", [Socio, ListaDeProductos]),
            case lists:member(Socio, Socios) of
                % Si el socio no existe, no se realiza el pedido
                false ->
                    Caller ! {error, not_a_member},
                    tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
                true ->
                    NuevoContadorPedidos = ContadorPedidos + 1,
                    % Se crea una lista con el nombre del producto, el pid de su proceso, y la cantidad a modificar
                    Venta = [{Pid, Cantidad} || {Producto, Pid} <- Productos, {ok, Cantidad} <- [{ok, C} || {P, C} <- ListaDeProductos, P =:= Producto]],                    modificar_productos(Venta), % Funcion que modifica productos de un pedido
                    Pedido = {NuevoContadorPedidos, Socio, ListaDeProductos},
                    Caller ! {ok, Pedido},
                    % Se reinicia el loop con la nueva informacion
                    tienda_loop(Socios, Productos, [Pedido | Pedidos], NuevoContadorPedidos)
            end;
        % Se imprime una lista de los socios en existencia
        {lista_socios, Caller} ->
            io:format("Recibe: solicitud de lista de socios~n"),
            Caller ! {ok, Socios},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos);
        % Se imprime una lista con todos los pedidos realizados
        {productos_vendidos, Caller} ->
            io:format("Recibe: solicitud de productos vendidos~n"),
            Caller ! {ok, Pedidos},
            tienda_loop(Socios, Productos, Pedidos, ContadorPedidos)
    end.

% Terminar procesos de productos
terminate_productos([]) -> ok; % Si no hay nada, regresamos ok
terminate_productos([{_, Pid} | T]) -> 
    % Si existen procesos de productos aún, los eliminamos
    exit(Pid, kill),
    terminate_productos(T).

% Se modifican las cantidades de cada producto en un pedido recursivamente
modificar_productos([]) -> ok; % Si la lista no tiene elementos, regresa ok
modificar_productos([{Pid, Cant} | T]) ->
    Pid ! {modificar, -Cant}, % Se manda al proceso del producto a modificar
    modificar_productos(T).

% Bucle del proceso del producto
% Se llama para crear el proceso, y recibe mensajes para modificar o conseguir informacion
producto_loop(Producto, Cantidad) ->
    receive
        % Se modifica la cantidad del producto
        {modificar, NuevaCantidad} ->
            io:format("Producto ~p modificado por ~p~n", [Producto, NuevaCantidad]),
            producto_loop(Producto, max(0, Cantidad + NuevaCantidad));
        % Manda la cantidad del producto a la funcion caller
        {get_cantidad, Caller} ->
            Caller ! {cantidad, Cantidad},
            producto_loop(Producto, Cantidad);
        % Se termina el proceso
        stop ->
            io:format("Producto ~p detenido~n", [Producto]),
            exit(normal)
    end.

%Se obtiene la cantidad del producto
get_cantidad(Pid) ->
    Pid ! {get_cantidad, self()},
    receive
        {cantidad, Cantidad} -> Cantidad
    end.
