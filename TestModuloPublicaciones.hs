module TestModuloPublicaciones where

import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [testsuitePublicacionesQueLeGustanA, testsuiteLesGustanLasMismasPublicaciones]

testsuitePublicacionesQueLeGustanA = test [

    " publicacionesQueLeGustanA 1: red sin publicaciones " ~: (publicacionesQueLeGustanA redC usuario1) ~?= [],

    " publicacionesQueLeGustanA 2: No hay publicaciones con like del usuario " ~: (publicacionesQueLeGustanA redB usuario4) ~?= [],

    " publicacionesQueLeGustanA 3: Hay solo una publicación con like del usuario " ~: (publicacionesQueLeGustanA redA usuario5) ~?= [publicacion1_3],

    " publicacionesQueLeGustanA 4: Hay mas de una publicación con like del usuario " ~: (publicacionesQueLeGustanA redA usuario2) ~?= [publicacion1_1, publicacion1_3, publicacion3_2, publicacion4_1],

    " publicacionesQueLeGustanA 5: Hay mas de una publicación con like del usuario y hay publicaciones repetidas " ~: (publicacionesQueLeGustanA redD usuario2) ~?= [publicacion1_1, publicacion1_3, publicacion3_2, publicacion4_1]

    ]

testsuiteLesGustanLasMismasPublicaciones = test [

    " lesGustanLasMismasPublicaciones 1: red sin publicaciones " ~: (lesGustanLasMismasPublicaciones redC usuario1 usuario2) ~?= True,

    " lesGustanLasMismasPublicaciones 2: publicaciionesQueLeGustanA red u1 /= publicacionesQueLeGustanA red u2 " ~: (lesGustanLasMismasPublicaciones redB usuario2 usuario4) ~?= False,

    " lesGustanLasMismasPublicaciones 3: publicaciionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2 " ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True

    ]

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

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

usuariosA = [usuario1, usuario2, usuario3, usuario5]
usuariosB = [usuario1, usuario2, usuario3, usuario4]

publicacionesA = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
publicacionesD = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion2_1, publicacion1_1, publicacion2_2, publicacion3_1, publicacion1_3, publicacion3_2, publicacion4_1, publicacion4_2, publicacion3_1]

redA = (usuariosA, [], publicacionesA)
redB = (usuariosB, [], publicacionesB)
redC = (usuariosA, [], [])
redD = (usuariosA, [], publicacionesD)
