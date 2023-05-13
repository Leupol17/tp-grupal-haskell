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

-- Funciones auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs)
    | t == x = True
    | otherwise = pertenece t xs
    
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


