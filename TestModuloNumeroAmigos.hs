module TestModuloNumeroAmigos where

import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [testsuiteCantidadDeAmigos, testsuiteUsuarioConMasAmigos,testsuiteEstaRobertoCarlos]
testsuiteCantidadDeAmigos = test [

    " cantidadDeAmigos 1: Red sin amigos " ~: (cantidadDeAmigos redSinUsuario4 usuario4) ~?= 0,

    " cantidadDeAmigos 2: Red sin usuarios " ~: (cantidadDeAmigos redSinUsuarios usuario4) ~?= 0,

    " cantidadDeAmigos 3: Red con usuarios y amigos" ~: (cantidadDeAmigos redSinUsuario4 usuario1) ~?= 2

    ]
testsuiteUsuarioConMasAmigos = test [

    " usuarioConMasAmigos 1: Red con 1 usuario " ~: (usuarioConMasAmigos redConUnUsuario) ~?= usuario1,

    " usuarioConMasAmigos 2: Red con 2 o mas usuarios con igual cant de amigos " ~: (usuarioConMasAmigos redConDosUsuarioMismosAmigos) ~?= usuario3,

    " usuarioConMasAmigos 3: Red con usuarios" ~: (usuarioConMasAmigos redNormalSinPublicaciones ) ~?= usuario1
    ]
testsuiteEstaRobertoCarlos = test [
    "estaRobertoCarlos 1: menos de 10 (ex un millon) amigos " ~: (estaRobertoCarlos redConMenosDe10Amigos) ~?= False,

    "estaRobertoCarlos 2: igual a 10 (ex un millon) amigos " ~: (estaRobertoCarlos redCon10Amigos) ~?= False,

    "estaRobertoCarlos 3: mayor a 10 (ex un millon) amigos " ~: (estaRobertoCarlos redConMasDe10Amigos) ~?= True
    ]

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Mirta")
usuario7 = (7, "Marcos")
usuario8 = (8, "Jorge")
usuario9 = (9, "Nacho")
usuario10 = (10, "Olivia")
usuario11 = (11, "Benjamin")
usuario12 = (12, "Melina")



usuariosA = [usuario1, usuario2, usuario3, usuario5]
usuariosB = [usuario1, usuario2, usuario3, usuario4]
todosLosUsuarios = [usuario1,usuario2,usuario3,usuario4,usuario5,usuario6,usuario7,usuario8,usuario9,usuario10,usuario11,usuario12]

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)
relacion2_3 = (usuario2, usuario3)

relacionAmigosCon10 = [relacion1_2,relacion1_3,relacion1_4,relacion1_5,relacion1_6,relacion1_7,relacion1_8,relacion1_9,relacion1_10,relacion1_11]
relacionAmigosConMasDe10 = [relacion1_2,relacion1_3,relacion1_4,relacion1_5,relacion1_6,relacion1_7,relacion1_8,relacion1_9,relacion1_10,relacion1_11, relacion1_12]

redSinUsuario4 = (usuariosA,[relacion1_2,relacion1_3], [] )
redSinUsuarios = ([], [], [])

redConUnUsuario = ([usuario1],[relacion1_2,relacion1_3], [] )
redConDosUsuarioMismosAmigos = ([usuario1, usuario2, usuario3, usuario4],[relacion1_2,relacion1_3, relacion2_3], [] )
redNormalSinPublicaciones = ([usuario1, usuario2, usuario3, usuario4],[relacion1_2,relacion1_3, relacion2_3,relacion1_4], [] )
redConMenosDe10Amigos = (todosLosUsuarios,[relacion1_2,relacion1_3, relacion2_3,relacion1_4], [] )
redCon10Amigos = (todosLosUsuarios,relacionAmigosCon10, [] )
redConMasDe10Amigos = (todosLosUsuarios,relacionAmigosConMasDe10, [] )


