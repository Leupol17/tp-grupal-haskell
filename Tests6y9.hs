module Tests6y9 where

import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
        "publicacionesDe Caso 1: tiene 1 usuario sin publicacion " ~: (publicacionesDe redSocial3 usuario1) ~?= [],
        "publicacionesDe Caso 2: tiene 1 usuario con 1 publicacion " ~: (publicacionesDe redSocial4 usuario1) ~?= [((1,"Juan"),"hola a todos",[])],
        "publicacionesDe Caso 3: tiene 1 usuario con varias publicaciones " ~: (publicacionesDe redSocial5 usuario1) ~?= [((1,"Juan"),"hola a todos",[]),((1,"Juan"),"Que tal",[]),((1,"Juan"),"Como estan",[])],
        "publicacionesDe Caso 4: tiene varios usuarios sin publicaciones " ~: (publicacionesDe redSocial1 usuario1) ~?= [],
        "publicacionesDe Caso 5: tiene varios usuarios con 1 publicacion " ~: (publicacionesDe redSocial6 usuario3) ~?= [((3,"Pedro"),"consectetur adipiscing elit bis",[(2,"Natalia"),(1,"Juan")])],
        "publicacionesDe Caso 6: tiene varios usuarios con varias publicaciones " ~: (publicacionesDe redSocial7 usuario2) ~?= [((2,"Natalia"),"Good Bye World",[(1,"Juan"),(4,"Mariela")]),((2,"Natalia"),"Hello World",[(4,"Mariela")])],
        "publicacionesDe Caso 7.1: tiene elementos repetidos " ~: (publicacionesDe redSocial8 usuario1) ~?= [((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[])],
        "publicacionesDe Caso 7.2: tiene elementos repetidos " ~: (publicacionesDe redSocial8 usuario2) ~?= [((2,"Natalia"),"Good Bye World",[(1,"Juan"),(4,"Mariela")]),((2,"Natalia"),"Hello World",[(4,"Mariela")])],
        "publicacionesDe Caso 7.3: tiene elementos repetidos " ~: (publicacionesDe redSocial8 usuario3) ~?= [((3,"Pedro"),"consectetur adipiscing elit bis",[(2,"Natalia"),(1,"Juan")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia")])],
        "publicacionesDe caso8: el usuario no esta en la red " ~: (publicacionesDe redSocial2 usuario4) ~?= [],
        "tieneUnSeguidorFiel Caso 1: no hay publicaciones del Usuario en la Red Social" ~: tieneUnSeguidorFiel redSocial1 usuario1 ~?= False,
        "tieneUnSeguidorFiel Caso 2: hay una publicacion del usuario en la Red Social pero no tiene likes" ~: tieneUnSeguidorFiel redSocial4 usuario1 ~?= False,
        "tieneUnSeguidorFiel Caso 3: Red Social donde hay una publicación del usuario1 y un usuario le dio like " ~: tieneUnSeguidorFiel redSocial9 usuario1 ~?= True,
        "tieneUnSeguidorFiel Caso 4: Red Social donde hay varias publicaciones del usuario1 y todos le dieron like" ~: tieneUnSeguidorFiel redSocial10 usuario1 ~?= True,
        "tieneUnSeguidorFiel Caso 5: Red Social donde usuario1 tiene un seguidor fiel pero no todos los usuarios le dieron like a todas sus publicaciones" ~: tieneUnSeguidorFiel redSocial11 usuario1 ~?= True,
        "tieneUnSeguidorFiel Caso 6: Red Social donde usuario1 tiene publicaciones pero no todos le dieron like" ~: tieneUnSeguidorFiel redSocial7 usuario1 ~?= False,
        "tieneUnSeguidorFiel Caso 7: Red Social vacía " ~: tieneUnSeguidorFiel redSocial0 usuario1 ~?= False
  ]        
  

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Carlos")
usuario7 = (7, "Sofia")



relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion1_6 = (usuario1,usuario6)
relacion1_7 = (usuario1,usuario7)
relacion6_1 = (usuario6,usuario1)
relacion2_2 = (usuario2, usuario2)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_2 = (usuario4, usuario2)
relacion4_5 = (usuario4, usuario5)
relacion2_1 = (usuario2,usuario1)






publicacion1_0_1 = (usuario1,"hola a todos",[])

publicacion1_0_2 = (usuario1,"Que tal",[])

publicacion1_0_3 = (usuario1,"Como estan",[])



publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])           
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Este es como mi sexto post", [usuario2,usuario3])
publicacion1_6_1 = (usuario1,"Este post es bis",[usuario2,usuario3])
publicacion1_7 = (usuario1,"Este es mi septimo post",[usuario2])
publicacion1_8 = (usuario1, "Otro post de Juan", [usuario2, usuario6, usuario7])
publicacion1_9 = (usuario1, "LALALALA", [usuario3])


publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])
publicacion2_3 = (usuario2, "Hello Haskell",[usuario1])


publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])
publicacion3_4 = (usuario3, "consectetur adipiscing elit bis", [usuario2, usuario1])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

publicacion6_1 = (usuario6, "Post de Carlos", [usuario1, usuario2, usuario7])

publicacion7_1 = (usuario7, "Post de Lucia", [usuario1, usuario2, usuario6])


publicacion1_2_3 = (usuario1,"hola a todos",[usuario2]) --publica usuario 1, dan mg usuario2 y 3


redSocial0 = ([],[],[])
redSocial1 = ([usuario1,usuario2,usuario3], [relacion1_2,relacion1_3], [])
redSocial2 = ([usuario1, usuario2], [relacion1_2, relacion2_1], [publicacion1_2_3])
redSocial3 = ([usuario1], [], [])
redSocial4 = ([usuario1], [], [publicacion1_0_1])
redSocial5 = ([usuario1], [], [publicacion1_0_1,publicacion1_0_2,publicacion1_0_3])
redSocial6 = ([usuario1,usuario2,usuario3], [relacion1_2,relacion1_3,relacion2_3], [publicacion3_4,publicacion1_5,publicacion2_2])
redSocial7 = ([usuario1,usuario2,usuario3], [relacion1_2,relacion1_3,relacion2_3], [publicacion3_4,publicacion3_3,publicacion1_5,publicacion1_4,publicacion2_2,publicacion2_1])
redSocial8 = ([usuario1,usuario1,usuario2,usuario2,usuario3,usuario3], [relacion1_2,relacion1_3,relacion2_3], [publicacion3_4,publicacion3_3,publicacion1_5,publicacion1_4,publicacion2_2,publicacion2_1,publicacion3_4,publicacion3_3,publicacion1_5,publicacion1_4,publicacion2_2,publicacion2_1])
redSocial9 = ([usuario1, usuario2, usuario3,usuario4], [relacion1_2, relacion1_3], [publicacion1_7])
redSocial10 = ([usuario1, usuario2, usuario3, usuario4], [relacion1_2, relacion1_3, relacion1_4], [publicacion1_1,publicacion1_6,publicacion1_7 ])
redSocial11 = ([usuario1,usuario2,usuario3],[relacion1_2, relacion1_3],[publicacion1_9,publicacion1_6])