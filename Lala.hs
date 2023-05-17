module Lala where

import Solucion


usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_2 = (usuario2, usuario2)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_2 = (usuario4, usuario2)
relacion4_5 = (usuario4, usuario5)
relacion2_1 = (usuario2,usuario1)



usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_2, relacion2_3]

usuariosC = []
relacionesC = []

usuariosD = [usuario3, usuario4, usuario5]
relacionesD = [relacion3_4, relacion4_5, relacion2_4]
relacionesD2 = [relacion3_4, relacion4_5, relacion2_3]
relacionesD3 = [relacion3_4, relacion4_5, relacion1_2]


relacionesE = [relacion1_4, relacion2_3, relacion2_4, relacion3_4, relacion4_2]

relacionesF = [relacion1_3, relacion3_4, relacion1_2]
relacionesF2 = [relacion1_2, relacion1_4, relacion2_3, relacion1_2]
relacionesF3 = [relacion1_2, relacion2_3, relacion4_5]



publicacion1_0_1 = (usuario1,"hola a todos",[])

publicacion1_0_2 = (usuario1,"Que tal",[])

publicacion1_0_3 = (usuario1,"Como estan",[])



publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


publicacion1_2_3 = (usuario1,"hola a todos",[usuario2,usuario3]) --publica usuario 1, dan mg usuario2 y 3

redSocial1 = ([usuario1, usuario2], [relacion1_2, relacion2_1], [publicacion1_2_3])
redSocial2 = ([usuario1, usuario2], [relacion1_2, relacion2_1], [publicacion1_2_3])
redSocial3 = ([usuario1], [], [])
redSocial4 = ([usuario1], [], [publicacion1_0_1])
redSocial5 = ([usuario1], [], [publicacion1_0_1,publicacion1_0_2,publicacion1_0_3])






-- Definición de usuarios, relaciones y publicaciones de ejemplo
usuariosEjemplo :: [Usuario]
usuariosEjemplo = [(1, "usuario1"), (2, "usuario2"), (3, "usuario3"), (4, "usuario4")]

relacionesEjemplo :: [Relacion]
relacionesEjemplo = [((1, "usuario1"), (2, "usuario2")), ((2, "usuario2"), (3, "usuario3")), ((3, "usuario3"), (1, "usuario1"))]

publicacionesEjemplo :: [Publicacion]
publicacionesEjemplo = [((1, "usuario1"), "Publicación 1", [(2, "usuario2"), (3, "usuario3")])
                       , ((2, "usuario2"), "Publicación 2", [(1, "usuario1"), (3, "usuario3")])
                       , ((3, "usuario3"), "Publicación 3", [(1, "usuario1"), (2, "usuario2")])
                       , ((4, "usuario4"), "Publicación 4", [])
                       ]

redSocialEjemplo :: RedSocial
redSocialEjemplo = (usuariosEjemplo, relacionesEjemplo, publicacionesEjemplo)

-- Prueba de la función tieneUnSeguidorFiel
ejemploSeguidorFiel :: Bool
ejemploSeguidorFiel = tieneUnSeguidorFiel redSocialEjemplo (1, "usuario1")
-- Valor esperado: True, ya que el usuario (1, "usuario1") tiene al menos un seguidor fiel

-- Prueba de la función tieneUnSeguidorFiel con otro usuario
ejemploSeguidorFiel2 :: Bool
ejemploSeguidorFiel2 = tieneUnSeguidorFiel redSocialEjemplo (2, "usuario2")
-- Valor esperado: True, ya que el usuario (2, "usuario2") tiene al menos un seguidor fiel

-- Prueba de la función tieneUnSeguidorFiel con un usuario sin seguidores fieles
ejemploSeguidorFiel3 :: Bool
ejemploSeguidorFiel3 = tieneUnSeguidorFiel redSocialEjemplo (4, "usuario4")
-- Valor esperado: False, ya que el usuario (4, "usuario4") no tiene seguidores fieles
