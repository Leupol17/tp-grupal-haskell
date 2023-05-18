module TestFuncionesAuxiliaresSeba where

import Test.HUnit
import FuncionesAuxiliares

main = runTestTT testsSeba

testsSeba = test [testsuite1Seba] --,testsuiteMismosElementos,testsuiteSinRepetidos,testSuiteCadenaDeAmigos

testsuite1Seba = test [
    "relacionadosDirecto Caso 1: tanto (u1,u2) como (u2,u1) pertenecen a la relacion"~:(relacionadosDirecto usuario1 usuario2 redSocial1 && relacionadosDirecto usuario2 usuario1 redSocial1)~?= True,
    "relacionadosDirecto Caso 2: tanto (u1,u2) como (u2,u1) no pertenecen a la relacion"~:(not(relacionadosDirecto usuario1 usuario2 redSocial1)&& not(relacionadosDirecto usuario2 usuario1 redSocial1))~?= False
    
    ]


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

publicacion1_2_3 = (usuario1,"hola a todos",[usuario2,usuario3]) --publica usuario 1, dan mg usuario2 y 3

redSocial1 = ([usuario1, usuario2], [relacion1_2, relacion2_1], [publicacion1_2_3])
redSocial2 = ([usuario1, usuario2], [relacion1_2, relacion2_1], [publicacion1_2_3])
