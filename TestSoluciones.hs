module TestSoluciones where 

import Test.HUnit
import Solucion 

main = runTestTT tests
nombresDeUsuariosPrueba = test [
  " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
  "caso: Red vacia" ~: (nombreDeUsuarios redVacia) ~?= [],
  "caso: nombres de la redB" ~: (nombreDeUsuarios redB) ~?= ["Juan","Natalia","Pedro"],

]
proyectarNombresPrueba = test [
  "lista vacia" ~: (proyectarNombres []) ~?= [],
  "lista con usuariosA" ~: (proyectarNombres usuariosA []) ~?= ["Juan","Natalia","Pedro","Mariela"],
  "lista con usuariosB" ~: (proyectarNombres usuariosB []) ~?= ["Juan","Natalia","Pedro"],


]
amigosDePrueba  = test [
--" amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
   "caso1:RedSocial vacia" ~: (amigosDe redVacia usuario1) ~?=[],
   "caso2: el usuario1 tiene dos amigos" ~: (amigosde redA usuario1) ~?= [usuario2, usuario4],
    "caso3:el usuario2 tiene 3 amigos" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario3, usuario4],
    "caso4: el usuario5 no esta en la redA" ~: (amigosDe redA usuario5) ~?= [] "El usuario no esta en la red",
    "caso5: el usuario1 tiene solo un amigo" ~: (amigosDe redB usuario2) ~?=[usuario2],
    "caso6: el usuario5 esta presente en redB pero no tiene amigos" ~: (amigosDe redB usuario5) ~?= [] "no tiene amigos",
    "caso7: donde haya usuarios duplicados" ~: (amigosDe redDuplicada usuario1) ~?= "Error: Usuarios duplicados",
    "CasoExtra1: usuario3 tiene tres amigos" ~: (amigosDe redA usuario3) ~?= [usuario1, usuario2, usuario4],
    "CasoExtra2: usuario4 tiene tres amigos" ~: (amigosDe redA usuario4) ~?= [usuario1, usuario2, usuario3],
    "CasoExtra3: usuario2 tiene dos amigos" ~: (amigosDe redB usuario2) ~?= [usuario1, usuario3],
    "CasoExtra4: usuario3 tiene solo un amigo" ~: (amigosDe redB usuario3) ~?= [ usuario2],
    "CasoExtra5: usuario4 no esta en la red" ~: (amigosDe redB usuario4) ~?= [] "El usuario no esta en la red",




    ]   


-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

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


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redVacia _([],[],[])
relacionesDuplicadas = relacionesDuplicadas ++ [relacion1_2]
redDuplicada = (usuarioA, relacionesDuplicadas, publicacionesA)