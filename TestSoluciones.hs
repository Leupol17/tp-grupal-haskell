module TestSoluciones where

import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [testsuitenombresDeUsuariosPrueba,testsuiteamigosDePrueba] 

testsuitenombresDeUsuariosPrueba = test [
  " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
  "caso: nombres de la redB" ~: (nombresDeUsuarios redB) ~?= ["Juan","Natalia","Pedro","Federico"]
  ]


testsuiteamigosDePrueba  = test [
    "caso1:RedSocial vacia" ~: (amigosDe redVacia usuario1) ~?=[],
    "caso2: el usuario1 tiene dos amigos" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    "caso3:el usuario2 tiene 3 amigos" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario3, usuario4],
    "caso4: el usuario5 no esta en la redA" ~: (amigosDe redA usuario5) ~?= [] ,
    "caso5: el usuario5 tiene solo un amigo" ~: (amigosDe redB usuario5) ~?= [usuario6],
    "caso6: el usuario7 esta presente en redB pero no tiene amigos" ~: (amigosDe redB usuario7) ~?= []
    
 ]
-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Carlos")
usuario7 =(7,"Federico")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion5_6 = (usuario5, usuario6)
 -- Nueva relación

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

usuariosB = [usuario1, usuario2, usuario3, usuario5, usuario7]
relacionesB = [relacion1_2, relacion2_3,relacion5_6]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redVacia = ([], [], [])
relacionesDuplicadas = relacionesA ++ relacionesA
redDuplicada = (usuariosA, relacionesDuplicadas, publicacionesA)
red1 = ([usuario1, usuario2, usuario3, usuario4, usuario5], [relacion1_2, relacion2_3, relacion3_4, relacion1_4], [])
secuenciaUsuarios1 = [usuario1, usuario2, usuario3]
