module ModuloPublicaciones where

import FuncionesBase
import FuncionesAuxiliares

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_,_,[]) _ = []
publicacionesQueLeGustanA (users,rels,(p:pubs)) u
    | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (users,rels,pubs) u
    | otherwise = publicacionesQueLeGustanA (users,rels,pubs) u

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 =
    mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)