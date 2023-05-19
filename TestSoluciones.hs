module TestSoluciones where 

import Test.HUnit
import Solucion 

main = runTestTT tests

tests = test [

    "cadenaAmigos1: " ~: (cadenaDeAmigos secuenciaUsuarios1 red1) ~?= True

    ]   


usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion2_3 = (usuario2, usuario3)
relacion3_4 = (usuario3, usuario4)
relacion1_4 = (usuario1, usuario4)

red1 = ([usuario1, usuario2, usuario3, usuario4, usuario5], [relacion1_2,relacion2_3,relacion3_4,relacion1_4], [])
secuenciaUsuarios1 = [usuario1, usuario2, usuario3]
