module Introducao where
{-
Implemente a função sumDivisor, que retorna a soma de todos os divisores de z, maiores ou iguais a x e menores ou iguais a y.
-}
sumDivisor _ _ 0 = 0
sumDivisor x y z
  | x > y = 0
  | (mod x z) == 0 = 1 + sumDivisor (x+1) y z
  | otherwise = sumDivisor (x+1) y z

{-
Implemente a função collatzSteps, que calcula a quantidade de passos na conjecture Collatz.

A conjecture Collatz é uma sequência que é definida da seguinte forma:
1) Começe com um número positivo n
2) Se n for igual a 1, pare
3) Se n for par, divida n por 2
4) Se n for ímpar, multiplique n por 3 e some 1.

A função collatzSteps deve calcular a quantidade de passos para se chegar de n até 1.

Por exemplo, para n igual a 5, temos os seguintes passos:
5, 16, 8, 4, 2, 1

Logo a sequência collatz de 5 tem exatamente 6 passos.
-}
collatzSteps 1 = 1
collatzSteps n
  | ((mod n 2) == 0) = 1 + (collatzSteps (quot n 2))
  | otherwise = 1 + (collatzSteps ((n * 3) + 1))


{-
Implemente a função reverseArray que recebe um array e retorna o mesmo invertido.

Obs: Não utilizar a função pre-existente reverse.
-}
reverseArray [] = []
reverseArray (x:xs) = (reverseArray xs) ++ [x]
