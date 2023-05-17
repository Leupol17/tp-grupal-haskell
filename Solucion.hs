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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = publicacionesDe' (publicaciones red) usuario
  where
    publicacionesDe' [] _ = []
    publicacionesDe' (p:ps) u
      | usuarioDePublicacion p == u && not (pertenece p ps) = p : publicacionesDe' ps u
      | otherwise = publicacionesDe' ps u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red usuario = tieneUnSeguidorFiel' (usuarios red) (publicacionesDe red usuario)
  where
    tieneUnSeguidorFiel' [] _ = False
    tieneUnSeguidorFiel' (u:us) ps
      | esSeguidorFiel u ps = True
      | otherwise = tieneUnSeguidorFiel' us ps

    esSeguidorFiel :: Usuario -> [Publicacion] -> Bool
    esSeguidorFiel _ [] = True
    esSeguidorFiel u (p:ps)
      | not (pertenece u (likesDePublicacion p)) = False
      | otherwise = esSeguidorFiel u ps




-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = esSecuenciaAmigos secuencia red
  where
    secuencia = obtenerSecuenciaAmigos red u1 u2
-- Devuelve una lista en la que el primer elemento es u1, el último es u2, y todos los que hay entre ellos son la secuencia obtenida por obtenerAmigosEnComun
obtenerSecuenciaAmigos :: RedSocial -> Usuario -> Usuario -> [Usuario]
obtenerSecuenciaAmigos red u1 u2 = u1 : amigosEnComun ++ [u2]
  where
    amigosEnComun = obtenerAmigosEnComun red u1 u2

-- Devulve una lista en donde están todos los relacionados directos (vease relacionadosDirectos) de u1 y u2
obtenerAmigosEnComun :: RedSocial -> Usuario -> Usuario -> [Usuario]
obtenerAmigosEnComun ([],_,_) _ _ = []
obtenerAmigosEnComun (u:us,rels,pubs) u1 u2 
    | relacionadosDirecto u1 u (u:us,rels,pubs) && relacionadosDirecto u u2 (u:us,rels,pubs) = u : obtenerAmigosEnComun (us,rels,pubs) u1 u2 
    | otherwise = obtenerAmigosEnComun (us,rels,pubs) u1 u2

-- Devuelve True <=> todos los elementos de la lista tienen relacion directa con el siguiente elemnto en la lista.
esSecuenciaAmigos :: [Usuario] -> RedSocial -> Bool
esSecuenciaAmigos [] _ = True
esSecuenciaAmigos [_] _ = True
esSecuenciaAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && esSecuenciaAmigos (u2:us) red

-- Funciones auxiliares

------------------------------------------Leo------------------------------------------

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

-----------------------------------------------------Sebastian----------------------------------------------------------------

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
    
---------------------------Agus------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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


--funcion que toma una red y una lista de usuarios y devuelve treu si todos los usuarios pertenecen 
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _[] = True -- si el usuario esta en la red devuelve True, sino:
sonDeLaRed red (x:xs) = pertenece x (usuarios red) && sonDeLaRed red xs -- va verificando usuario por usuario sacando el primer elemento x si esta o no en la red, luego hace recursion con el resto de la lista xs


---------------------------Maty------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

----------Ejercicio 10----------

