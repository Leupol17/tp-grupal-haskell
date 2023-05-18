module TestFuncionesAuxiliares where

import Test.HUnit
import FuncionesAuxiliares

main = runTestTT testsrelaciones

testsrelaciones = test [testsuite1relaciones, testsuite2relaciones, testsuite3relaciones]

testsuite1relaciones = test [

    " usuariosDeRelacionValida Caso 1: us no tiene elementos "  ~: (usuariosDeRelacionValida usuariosC relacionesA) ~?= False,
    " usuariosDeRelacionValida Caso 2: rels no tiene elementos " ~: (usuariosDeRelacionValida usuariosA relacionesC) ~?= True,
    " usuariosDeRelacionValida Caso 3: us y rels tienen elementos. para 0=<x<|rels|, rels[x]0 pertenece a us, rels[x]1 pertenece a us, rels[x]0 == rels[x]1 " ~: (usuariosDeRelacionValida usuariosB relacionesB) ~?= False,
    " usuariosDeRelacionValida Caso 4: us y rels tienen elementos. para 0=<x<|rels|, rels[x]0 no pertenece a us, rels[x]1 pertenece a us, rels[x]0 /= rels[x]1 "  ~: (usuariosDeRelacionValida usuariosD relacionesD) ~?= False,
    " usuariosDeRelacionValida Caso 5: us y rels tienen elementos. para 0=<x<|rels|, rels[x]0 pertenece a us, rels[x]1 no pertenece a us, rels[x]0 /= rels[x]1 "  ~: (usuariosDeRelacionValida usuariosD relacionesD2) ~?= False,
    " usuariosDeRelacionValida Caso 6: us y rels tienen elementos. para 0=<x<|rels|, rels[x]0 no pertenece a us, rels[x]1 no pertenece a us, rels[x]0 /= rels[x]1 "  ~: (usuariosDeRelacionValida usuariosD relacionesD3) ~?= False,
    " usuariosDeRelacionValida Caso 7: us y rels tienen elementos. para 0=<x<|rels|, rels[x]0 pertenece a us, rels[x]1 pertenece a us, rels[x]0 /= rels[x]1 " ~: (usuariosDeRelacionValida usuariosA relacionesA) ~?= True

    ]

testsuite2relaciones = test [

    " relacionesAsimetricas Caso 1: rels no tiene elementos " ~: (relacionesAsimetricas relacionesC) ~?= True,
    " relacionesAsimetricas Caso 2: rels tiene elementos. para 0=<x<|rels|, (rels[x]1, rels[x]0) pertenece a rels " ~: (relacionesAsimetricas relacionesE) ~?= False,
    " relacionesAsimetricas Caso 3: rels tiene elementos. para 0=<x<|rels|, (rels[x]1, rels[x]0) no pertenece a rels " ~: (relacionesAsimetricas relacionesA) ~?= True

    ]

testsuite3relaciones = test [

    " noHayRelacionesRepetidas Caso 1: rels no tiene elementos " ~: (noHayRelacionesRepetidas relacionesC) ~?= True,
    " noHayRelacionesRepetidas Caso 2: rels tiene elementos, para 0=<x<|rels| && 0=<y<|rels| x/=y, idDeUsuario(rels[x]0) /= idDeUsuario(rels[y]0) && idDeUsuario(rels[x]1) /= idDeUsuario(rels[y]1) " ~: (noHayRelacionesRepetidas relacionesF3) ~?= True,
    " noHayRelacionesRepetidas Caso 3: rels tiene elementos, para 0=<x<|rels| && 0=<y<|rels| x/=y, idDeUsuario(rels[x]0) == idDeUsuario(rels[y]0) && idDeUsuario(rels[x]1) /= idDeUsuario(rels[y]1) " ~: (noHayRelacionesRepetidas relacionesA) ~?= True,
    " noHayRelacionesRepetidas Caso 4: rels tiene elementos, para 0=<x<|rels| && 0=<y<|rels| x/=y, idDeUsuario(rels[x]0) /= idDeUsuario(rels[y]0) && idDeUsuario(rels[x]1) == idDeUsuario(rels[y]1) " ~: (noHayRelacionesRepetidas relacionesF) ~?= True,
    " noHayRelacionesRepetidas Caso 5: rels tiene elementos, para 0=<x<|rels| && 0=<y<|rels| x/=y, idDeUsuario(rels[x]0) == idDeUsuario(rels[y]0) && idDeUsuario(rels[x]1) == idDeUsuario(rels[y]1) " ~: (noHayRelacionesRepetidas relacionesF2) ~?= False

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