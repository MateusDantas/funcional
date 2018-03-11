module FuncoesSintaxe where

{-
Implemente a função initials que dado o primeiro e o ultimo nome de uma pessoa, retorna as suas iniciais separadas por um espaço
-}
initials firstname lastname = [f] ++ " " ++ [l]
  where (f:_) = firstname
        (l:_) = lastname

{-
Implemente a função distance, que recebe duas tuplas correspondendo a dois pontos no plano x-y e retorna a distância entre eles.

Obs: Tente utilizar a sintaxe let-in para deixar o código mais claro.
-}
distance (x1, y1) (x2, y2) =
  let xdistance = x2 - x1
      ydistance = y2 - y1
      sqr z = z * z
  in sqrt((sqr xdistance) + (sqr ydistance))

{-
Implemente a função testEvenLength que deve retornar uma função capaz de dizer se um array tem tamanho par ou não.
-}
testEvenLength :: [Integer] -> Bool
testEvenLength = even . length
