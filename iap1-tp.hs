
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
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

-- Funciones auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs)
    | t == x = True
    | otherwise = pertenece t xs
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
--let redSocialEjemplo = ([(1,"Pedro"),(2,"Ana"),(3,"Martin")],
--                        [((1,"Pedro"),(2,"Ana")),((2,"Ana"),(3,"Martin"))],
--                        [((1,"Pedro"),"Hola a todos",[(2,"Ana"),(3,"Martin")])])

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (u1:u2:us) rs 
    |relacionadosDirecto u1 u2 rs = cadenaDeAmigos (u2:us) rs 
    |otherwise = False
cadenaDeAmigos _ _ = True   


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


--funcion que toma una red y una lista de usuarioas y devuelve treu si todos los usuarios pertenecen 
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _[] = True -- si el usuario esta en la red devuelve True, sino:
sonDeLaRed red (x:xs) = pertenece x (usuarios red) && sonDeLaRed red xs -- va verificando usuario por usuario sacando el primer elemento x si esta o no en la red, luego hace recursion con el resto de la lista xs