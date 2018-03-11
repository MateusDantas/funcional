module Listas where

{-
Implemente a função flatten, que recebe uma lista de listas e retorna uma única lista com todos os elementos.

Ex: flatten [[1, 2], [0, 3], [5]] retorna: [1,2,0,3,5]
-}
flatten [] = []
flatten (x:xs) = x ++ flatten xs

{-
Implemente a função whilePred, que recebe uma função 'pred' e uma lista x e retorna o maior segmento inicial dessa lista
tal que todos os elementos satisfazem a função 'pred'

Ex: whilePred even [2, 4, 6, 3, 2, 2, 2] retorna: [2, 4, 6]
-}
whilePred predF [] = []
whilePred predF (x:xs)
  | predF x = x : whilePred predF xs
  | otherwise = []

{-
Dado a função iSort (insertion sort) definida abaixo, crie duas funções minList e maxList
que usam a função iSort para retornar o menor e o maior valor de uma lista respectivamente.
-}
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins x [] = [x]
ins x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y:ins x ys

minList xs = head (iSort xs)

maxList xs = last (iSort xs)

{-
Implemente a função removePred que recebe uma lista x e uma função pred e retorna a lista inicial
sem os elementos que satisfazem a função pred.

Ex: removePred even [2, 5, 1, 3, 6] retorna: [5, 1, 3]
-}
removePred predF [] = []
removePred predF (x:xs)
  | predF x = removePred predF xs
  | otherwise = x : removePred predF xs

{-
Implemente a função doubleSum em uma linha, que recebe uma lista x e retorna a soma do dobro de
todos os múmeros nessa lista.

Obs: A sua função não pode conter mais do que uma linha.

Ex: doubleSum [1, 2, 3] = 1*2 + 2*2 + 3*2 = 12
-}
doubleSum x = foldr (+) 0 (map (2*) x)
