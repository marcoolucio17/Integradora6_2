Marco Lucio
Gabriel Mujica
Alejandro Charles
Germán Salas

Instrucciones Act 6.2 integradora

Socios: compradores suscritos en la tienda

socio:suscribir_socio(Socio: átomo identificador) 
socio:elimina_socio(Socio: átomo identificador)
socio:crea_pedido(Socio: átomo identificador, ListaDeProductos: tuplas de la forma {Producto, Cantidad})
socio:lista_existencias()

Productos: artículos registrados en la tienda que pueden ser comprados por los socios

tienda:registra_producto(Producto: átomo, Cantidad: entero mayor o igual a 0)
tienda:elimina_producto(Producto: átomo)
tienda:modifica_producto(Producto: átomo, Cantidad: entero mayor o igual a 0)

Tienda: servidor que administra las suscripciones de socios y la venta de productos

tienda:abre_tienda()
tienda:cierra_tienda()
tienda:lista_socios()
tienda:productos_vendidos()

La tienda se deberá manejar como un proceso que concentre la información
de socios suscritos, productos y pedidos. De los socios solo debe guardar
una lista con nombres. De los productos solo debe guardar la lista de
nombres y Pids de los que tienen un proceso vivo. Y de los pedidos debe
guardar el contador para controlar el # de pedido y una lista con el registro
histórico del total de cada producto vendido (nombre y cantidad). El
contador de pedidos se incrementa con cada pedido.

El proceso de la tienda y los procesos de todos los productos estarán en el
mismo nodo, pero los compradores pueden suscribirse y comprar productos
desde el mismo u otros nodos. No está de más mencionar que si el proceso
de la tienda termina, todos los procesos de los productos también deben
terminar automáticamente. En cambio, si el proceso de un producto termina,
solo su registro en la lista de productos activos debe eliminarse.

Incluir código de
despliegue que indiquen clara y detalladamente lo que está pasando en cada
nodo del sistema. Particularmente cuando se manden y reciban mensajes.
Por ejemplo, cuando un comprador haga una solicitud de suscripción, este
debe desplegar algo como “Manda: <fulanito> solicita suscripción”, antes de
suspenderse para esperar la respuesta de la tienda. 

