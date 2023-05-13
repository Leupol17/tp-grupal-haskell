module FuncionesAuxiliares where

import FuncionesBase

-- Funciones auxiliares

--Devuelve True <=> existe un elemento e:t en l:[t]
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (t:l)
    | e == t = True
    | otherwise = pertenece e l

-- Devuelve True <=> los elementos de rels:[Relacion] son válidos y todos los usuarios participantes en las relaciones pertenencen a us:[Usuario]
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = 
    usuariosDeRelacionValida us rels && 
    relacionesAsimetricas rels && 
    noHayRelacionesRepetidas rels

-- Devuelve True <=> para todo elemento (a,b) en rels:[Relacion], a y b pertenecen a us:[Usuario]
usuariosDeRelacionValida :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValida _ [] = True
usuariosDeRelacionValida us ((u1, u2):rels) 
    | u1 /= u2 && pertenece u1 us && pertenece u2 us = usuariosDeRelacionValida us rels
    | otherwise = False

-- Devuelve True <=> para todo elemento (a,b) en rels:[Relacion], no existe (b,a)
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas ((u1, u2):rels)
    | pertenece (u2, u1) rels == False = relacionesAsimetricas rels
    | otherwise = False

-- Devuelve True <=> no hay elementos que se repitan en rels:[Relacion]
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (r:rels)
    | pertenece r rels == False = noHayRelacionesRepetidas rels
    | otherwise = False

-- Devuelve True <=> el último elemento de l:[t] es igual a e:t 
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon _ [] = False
terminaCon e [t] = e == t
terminaCon e (t:l) = terminaCon e l
