module Solucion where 

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--[EJERCICIO 1]
{-Devuelve un numero entero que representa la cantidad de usuarios de una red social dada, que cumplan con la condicion de ser Amigos del Usuario especificado -}
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres red

{-dada una redsocial, extrae todos los nombres sin repetir en una lista de string-}
proyectarNombres :: RedSocial -> [String]
proyectarNombres red = obtenerProyectarNombres (usuarios red) []

{- funcion auxiliar para nombresDeUsuarios. recorre los usuarios y acumula los nombres que no esten ya la lista proyectados-}
obtenerProyectarNombres :: [Usuario] -> [String] ->[String]
obtenerProyectarNombres [] _ =[] 
obtenerProyectarNombres (( _ , nombre): restoDeUsuarios) yaProyectados
    | pertenece nombre yaProyectados = obtenerProyectarNombres restoDeUsuarios  yaProyectados
    | otherwise = nombre : obtenerProyectarNombres restoDeUsuarios (nombre:yaProyectados)  

--[EJERCICIO 2]
{- funcion auxiliar que por recursividad busca los amigos del usuario en la lista de relaciones.Toma relacion, usuario y uana lista de amigos del usuario en cuestion.
Comprueba cada relacion de la lista y si el usuario que tomamos cumple estar en una relacion y el segundo no esta en la lista de amistad, entonces el contrario se añade 
a la lista de amigos. Si el primero de los usuarios no esta en la relacion, entonces se omite. -}
amigosDelUsuario ::[Relacion] -> Usuario -> [Usuario] ->[Usuario]
amigosDelUsuario [] _ amigos = amigos
amigosDelUsuario ((relacion1, relacion2): rs) usuario amigos
    | relacion1 == usuario && not(pertenece relacion2 amigos) = amigosDelUsuario rs usuario (relacion2 : amigos)
    |relacion2 == usuario && not(pertenece relacion1 amigos) = amigosDelUsuario rs usuario (relacion1 : amigos)
    |otherwise = amigosDelUsuario rs usuario amigos

{-Toma redSocial y usuario, comprueba si la red y el usuario son validos; y si dicho usuario es de la red. si ninguna de las condiciones se cumple devuelve una lista vacia
si todas se cumplen llama a la funcion aux con las relaciones de la red, el usuario y una lista vacia de amigos.Finalmente devuelve la lsita de los amigos del usuario  -}
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe redSocial usuario = amigosDelUsuario (relaciones redSocial) usuario []
          
--[EJERCICIO 3]
{-Dada una red social y un usuario retorna la cantidad de amigos de ese usuario en dicha red social -}          
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario= cantidadDeUsuarios ( amigosDe red usuario)

{-Funcion que devuelve la cantidad de Usuarios en un lista de Usuarios -}
cantidadDeUsuarios :: [Usuario] -> Int
cantidadDeUsuarios [] = 0
cantidadDeUsuarios (_:xs) = 1 + cantidadDeUsuarios xs 

--[EJERCICIO 4]
{-Retorna el usuario de una red social dada que tenga la mayor cantidad de Amigos -}
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararCantidadDeAmigos (head listaUsuarios) listaUsuarios red
        where listaUsuarios = usuarios red

{-Evalua, dado el usuario inicial de la lista, la lista de usuarios y la red, cual es el usuario con mas amigos y lo retorna-}
compararCantidadDeAmigos :: Usuario -> [Usuario] -> RedSocial -> Usuario
compararCantidadDeAmigos usuarioMayor [] _ = usuarioMayor
compararCantidadDeAmigos usuarioMayor (usuario:us) red
    | cantidadDeAmigos red usuario >= cantidadDeAmigos red usuarioMayor = compararCantidadDeAmigos usuario us red
    | otherwise = compararCantidadDeAmigos usuarioMayor us red

--[EJERCICIO 5]
{-Evalua que en una red social exista un usuario con mas de un millon de amigos, en cuyo caso retorna True, caso contrario False -}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = masDeUnMillonDeAmigos (usuarios red) red

{-Evalua, dada una lista de Usuarios pertenecientes a una red social y dicha red social, si alguno de los usuarios posee mas de un millon de amigos, en cuyo caso retorna True, caso contrario False -}
masDeUnMillonDeAmigos :: [Usuario] -> RedSocial -> Bool
masDeUnMillonDeAmigos [] red = False
masDeUnMillonDeAmigos (x:xs) red
    |cantidadDeAmigos red x <= 10 = masDeUnMillonDeAmigos xs red
    |otherwise = True

--[EJERCICIO 6]
-- Dada una red social y un usuario, nos devuelve una lista de publicaciones asociadas a dicho usuario en la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = publicacionesDe' (publicaciones red) usuario
  where
    publicacionesDe' [] _ = []
    publicacionesDe' (p:ps) u
      | usuarioDePublicacion p == u && not (pertenece p ps) = p : publicacionesDe' ps u
      | otherwise = publicacionesDe' ps u

--[EJERCICIO 7]
-- describir qué hace la función: Devuelve el resultado de la función obtenerPublicacionesQueLeGustanA, se pasa una lista vacía que sirve para mantener referencia de res
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = obtenerPublicacionesQueLeGustanA red us []

-- Devuelve todas las publicaciones de la red que tienen en su lista de likes a u:Usuario y que no pertenezcan a res:[Publicacion]
obtenerPublicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion]
obtenerPublicacionesQueLeGustanA (_,_,[]) _ _ = []
obtenerPublicacionesQueLeGustanA (users,rels,(p:pubs)) u res
    | pertenece u (likesDePublicacion p) && pertenece p res == False = p : obtenerPublicacionesQueLeGustanA (users,rels,pubs) u nuevoRes
    | otherwise = obtenerPublicacionesQueLeGustanA (users,rels,pubs) u res
    where nuevoRes = (p:res)

--[EJERCICIO 8]
-- describir qué hace la función: Conmpara los elementos de la lista publicacionesQueLeGustanA red:RedSocial u1:Usuario con publicacionesQueLeGustanA red u2:Usuario
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 =
    mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

--[Ejercicio 9]
--Dada una red social y un usuario, determina si el usuario tiene al menos un seguidor fiel en la red social. Un seguidor fiel es aquel que dio like a todas las publicaciones del usuario
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red usuario = tieneUnSeguidorFiel' (usuarios red) (publicacionesDe red usuario)
-- funcion auxiliar que toma una lista de Usuario y Publicacion. Comprueba que haya almenos un seguidor fiel en la lista de Usuarios verificando la lista de Publicacion
tieneUnSeguidorFiel' :: [Usuario] -> [Publicacion] -> Bool
tieneUnSeguidorFiel' [] _ = False
tieneUnSeguidorFiel' _ [] = False
tieneUnSeguidorFiel' (u:us) ps
  | esSeguidorFiel u ps = True
  | otherwise = tieneUnSeguidorFiel' us ps
--funcion auxiliar que toma un Usuario y Publicacion, comprueba que el usuario le haya dado like a todas las publicaciones de lista de Publicacion
esSeguidorFiel :: Usuario -> [Publicacion] -> Bool
esSeguidorFiel _ [] = True
esSeguidorFiel usuario (p:ps)
  | pertenece usuario (likesDePublicacion p) = esSeguidorFiel usuario ps
  | otherwise = False

--[EJERCICIO 10]
-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = esSecuenciaAmigos secuencia u1 u2 red
  where
    secuencia = obtenerSecuenciaAmigos red u1 u2

-- Devuelve una lista en la que el primer elemento es u1, el último es u2, y todos los que hay entre ellos son la secuencia obtenida por obtenerAmigosEnComun
obtenerSecuenciaAmigos :: RedSocial -> Usuario -> Usuario -> [Usuario]
obtenerSecuenciaAmigos red u1 u2 = u1 : amigosEnComun
  where
    amigosEnComun = obtenerAmigosEnComun red red u1 u2

-- Devulve una lista en donde están todos los relacionados directos (vease relacionadosDirectos) de u1 y u2
obtenerAmigosEnComun :: RedSocial -> RedSocial-> Usuario -> Usuario -> [Usuario]
obtenerAmigosEnComun ([],_,_) _ _ _ = []
obtenerAmigosEnComun (u:us,rels,pubs) red u1 u2
    | relacionadosDirecto u u2 (u:us,rels,pubs) = [u2]
    | relacionadosDirecto u1 u (u:us,rels,pubs) = u : obtenerAmigosEnComun (us,rels,pubs) red u u2
    | otherwise = obtenerAmigosEnComun (us,rels,pubs) red u1 u2

-- Devuelve True <=> todos los elementos de la lista tienen relacion directa con el siguiente elemnto en la lista.
esSecuenciaAmigos :: [Usuario] -> Usuario -> Usuario-> RedSocial -> Bool
esSecuenciaAmigos us u1 u2 red = cantidadDeUsuarios us >= 2 && empiezaCon u1 us && terminaCon u2 us


{- Evalua, dada una lista de Usuarios y una lista de Publicaciones, las funciones: 
usuariosDePublicacionSonUsuariosDeRed, usuariosDeLikeDePublicacionSonUsuariosDeRed, noHayPublicacionesRepetidas
retornen True,en dicho caso, la funcion retorna True, en caso de que alguna de ellas retorne False, la funcion publicacionValidas retorna False -}
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas users pubs
    |   usuariosDePublicacionSonUsuariosDeRed users pubs && usuariosDeLikeDePublicacionSonUsuariosDeRed users pubs && noHayPublicacionesRepetidas pubs = True
    |   otherwise = False

{- Evalua que, dada dos listas una de usuarios y otra de publicaciones, los usuarios de la lista de publicaciones pertenezcan a la lista de Usuarios,
en cuyo caso, la funcion retorna True, en caso contrario False -}
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed users (pub:pubs)
    |   pertenece (usuarioDePublicacion pub) users = usuariosDePublicacionSonUsuariosDeRed users pubs
    |   otherwise = False

{- Evalua que, dada dos listas una de usuarios y otra de publicaciones, los likes de la lista de publicaciones, sean dados por usuarios de la lista de Usuarios
en cuyo caso, la funcion retorna True, en caso contrario False -}
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed _ [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed users (pub:pubs)
    |   usuariosLikeValidos users (likesDePublicacion pub) = usuariosDeLikeDePublicacionSonUsuariosDeRed users pubs
    |   otherwise = False

{- Evalua que, dada dos listas de Usuarios, todos los usuarios de la segunda lista, pertenezcan a la primera.
En este caso la funcion retorna True, en caso de que alguno no pertenezca, retorna False -}
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos _ [] = True
usuariosLikeValidos users (userL:userLs)
    |   pertenece userL users = usuariosLikeValidos users userLs
    |   otherwise = False

{- Evalua que no existan publicaciones repetidas en una misma lista de publicaciones
en caso de existir publicaciones iguales retorna False, caso de no encontrar ninguna, retorna True -}
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas [pub] = True
noHayPublicacionesRepetidas (pub:pubs) 
    | pertenece pub pubs = False
    | otherwise = noHayPublicacionesRepetidas pubs

--Evalua que la lista dada comience con el valor indicado, en cuyo caso devuelve True, caso contrario devuelve False    
empiezaCon :: (Eq x) => x -> [x] -> Bool
empiezaCon t xs
    |head xs == t = True
    |otherwise = False


mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos [] []= True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos (x:xs) ys
    |pertenece x ys = mismosElementos xs (eliminar x ys) --verificamos que el primer elemento de la lsita xs('x') esta en la lista ys de forma recursiva, si coinciden los valores llamamos recursivamente a la lista sin esos valores ya comparados
    |otherwise = False
    where
        eliminar :: (Eq t) => t -> [t] -> [t]
        eliminar _ [] = []
        eliminar t (x:xs)
            |t == x = xs --si t es igual a x devolvemos el resto de la lista con el elemento en comun
            |otherwise = x : eliminar t xs --armamos una nueva lista con el elemento x a comparar y llamamos a la recursion 


sinRepetidos :: Eq a => [a] -> Bool 
sinRepetidos [] = True
--llamo a estaRepetido para ver si el primer elemento se repite en el resto de la lista
-- luego llamamos recursivamente la funcion para chequear el resto de la lista xs
sinRepetidos (x:xs) = not(estaRepetido x xs) && sinRepetidos xs
    where
        estaRepetido :: Eq a => a -> [a] -> Bool
        estaRepetido _ [] = False
        estaRepetido e (y:ys)
            |e == y = True
            |otherwise = estaRepetido e ys

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 rs = pertenece (u1,u2) (relaciones rs) || pertenece (u2, u1) (relaciones rs)

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (u1:u2:us) rs 
    |relacionadosDirecto u1 u2 rs = cadenaDeAmigos (u2:us) rs 
    |otherwise = False
cadenaDeAmigos _ _ = True   

redSocialValida :: RedSocial -> Bool
redSocialValida ( usuarios, relaciones, publicaciones) =
    usuariosValidos usuarios && relacionesValidas usuarios relaciones && publicacionesValidas usuarios publicaciones

--esta funcion toma como entrada Usuario y se asegura que su resultado:hace recursion continua hasta que se agoten los usuarios de la lista. 
--Si se encuentra con repetidos devolvera False. Si todos los ids son distintos devuelve True
noHayIdRepetidos :: [Usuario] -> Bool
noHayIdRepetidos [] = True -- cuando la lista de usuarios sin ids repetidos es vacia se considera que es Verdadero y cumple
noHayIdRepetidos (x:xs) = verificarUsuario x xs && noHayIdRepetidos xs --cuando sucede lo contrario, hay que verificar los ids en la lista con el primer usuario x con el resto de la lista xs.
                                                                       -- Luego verifica si los ids son iguales usando verificarUsuario.Por ultimo hace una recursion si pasa con el resto de xs
        where
            verificarUsuario _ [] = True --Cuando la lista de xs(lista restante) es vacia; se concidera a los ids como unicos y devuelve True
            verificarUsuario numeroID  (y:ys) = idDeUsuario numeroID /= idDeUsuario y && verificarUsuario numeroID ys -- toma un usuario con su "numeroID" y una lista de mas usuarios (y:ys) con su numeroID 
                                                                                                                          --en donde los compara con la posicion "y" de la lista. Si son distintas se realiza la comparacion con ys.
 

 --funcion que toma Usuario y verifica si es valido.    
usuarioValido:: Usuario ->Bool
usuarioValido numeroID = idDeUsuario numeroID >0 && nombreValido (nombreDeUsuario numeroID)--Toma un usuario con su numeroID y verifica que el ID>0 y si su nombre es valido, si se cumplen ambas es True
    where nombreValido [] = False -- condicion para la lista de nombre
          nombreValido (_:_) = True --condicion de la lista si tiene al menos un elem 


--funcion que verifica que los usuarios sean validos y no haya repetidos
usuariosValidos ::[Usuario] -> Bool
usuariosValidos []= True -- no tiene usuarios invalidos ni repetidos
usuariosValidos (x: xs) = usuarioValido x && noHayIdRepetidos (x:xs) && usuariosValidos xs -- comprueba que la primer posicion se unica, si lo hace llama a usuarioValido y noHayIdRp. Si se cumple llama a la fucion usuariosValidos y devuelve true


--funcion que toma una red y una lista de usuarioas y devuelve true si todos los usuarios pertenecen 
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _[] = True -- si el usuario esta en la red devuelve True, sino:
sonDeLaRed red (x:xs) = pertenece x (usuarios red) && sonDeLaRed red xs -- va verificando usuario por usuario sacando el primer elemento x si esta o no en la red, luego hace recursion con el resto de la lista xs


--Devuelve True <=> existe un elemento e:t en l:[t]
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (t:l)
    | e == t = True
    | otherwise = pertenece e l

-- Devuelve True <=> los elementos de rels:[Relacion] son válidos y todos los usuarios participantes en las relaciones pertenencen a us:[Usuario]
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = 
    usuariosDeRelacionValida us rels && 
    relacionesAsimetricas rels && 
    noHayRelacionesRepetidas rels

-- Devuelve True <=> para todo elemento (a,b) en rels:[Relacion], a y b pertenecen a us:[Usuario]
usuariosDeRelacionValida :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValida _ [] = True
usuariosDeRelacionValida us ((u1, u2):rels) 
    | u1 /= u2 && pertenece u1 us && pertenece u2 us = usuariosDeRelacionValida us rels
    | otherwise = False

-- Devuelve True <=> para todo elemento (a,b) en rels:[Relacion], no existe (b,a)
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas ((u1, u2):rels)
    | pertenece (u2, u1) rels == False = relacionesAsimetricas rels
    | otherwise = False

-- Devuelve True <=> no hay elementos que se repitan en rels:[Relacion]
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (r:rels)
    | pertenece r rels == False = noHayRelacionesRepetidas rels
    | otherwise = False

-- Devuelve True <=> el último elemento de l:[t] es igual a e:t 
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon _ [] = False
terminaCon e [t] = e == t
terminaCon e (t:l) = terminaCon e l

