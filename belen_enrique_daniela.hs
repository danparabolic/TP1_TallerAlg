--Aux1
esSumaDeDosCubosDesde :: Integer -> Integer -> Bool
esSumaDeDosCubosDesde n z | z^3 > (div n 2) = False -- Esto se puede hacer de dos formas: si z^3 es mayor a "n" ya no tiene sentido seguir porque cualquier natural que sumemos nos alejará de n **
                          | esUnCubo (n - (z^3)) = True
                          | otherwise = esSumaDeDosCubosDesde n (z+1) -- Si no encontró suma de cubo con (z^3) y su par, intentará con el siguiente z. (1^3, sino 2^3, sino 3^3... etc.)
--n queremos ver si es la suma de dos cubos y z forma un primer cubo ya que usamos (z^3). El único número que se le puede sumar a (z^3) para llegar a "n" es (n - (z^3)).
--Si este último es cubo, entonces es verdadero que n es suma de cubos.
--Básicamente aportamos cubos de menor a mayor y lo único que la función hace es ver si (n - (z^3)) es cubo, porque en este caso es verdadero.
-- ** La otra forma es usar (div n 2), esto hace que pare una vez que z^3 es más de la mitad de n. Por qué? *
-- * Porque si un número mayor de la mitad de n es cubo, tendrá que comparar con un número menor de mitad de n para llegar a n, pero éstos números ya fueron probados porque la función comprueba los cubos y sus pares de menor a mayor. Esto quiere decir que una vez que la función comprueba los (z^3) que tengan hasta la mitad de valor de n, la otra mitad ya fue comprobada por buscar cada par que sumado a (z^3) de n.
--Ejemplo: si tengo 10 y quiero ver si es suma de cubos. Si pruebo con el 1 (asumamos que es un cubo), la función debe chequear el 9. Si pruebo con el 2, la función debe chequear el 8. El 3 con el 7, el 4 con el 6, el 5 con el 5. Al pasarnos de la mitad no tiene sentido seguir porque son mismos pares que ya se chequearon. (Aclaro que la función no busca numero por número, busca por z^3 que es mucho más rapido ya que todos los z^3 ya son cubos)
-- Entonces, la función da false cuando no encuentra una suma de cubos con los cubos (z^3) menores que mitad de n y sus pares de suma de la otra mitad de n.
-- Si es verdadero, habrá un cubo (z^3) menor que la mitad de n que tendrá su par de suma en la segunda mitad de n.



esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = esSumaDeDosCubosDesde n 1

--Aux2
descomposicionCubosDesde :: Integer -> Integer -> (Integer, Integer)
descomposicionCubosDesde n z | z^3 > (div n 2) = (0, 0)
                             | esUnCubo (n - (z^3)) == True = (z, round (fromIntegral (n - (z^3))**(1/3)))
                             | otherwise = descomposicionCubosDesde n (z+1)
--Ésta es una misma idea solo que en vez de arrojar False arroja (0, 0) y en vez de arrojar True arroja z y la raiz cubica del segundo cubo.




descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = descomposicionCubosDesde n 1


--Aux3
cantidadDeFormasDesde :: Integer -> Integer -> Integer
cantidadDeFormasDesde n z | z^3 > (div n 2) = 0
                          | esUnCubo (n - (z^3)) = 1 + cantidadDeFormasDesde n (z+1)
                          | otherwise = cantidadDeFormasDesde n (z+1)


cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = cantidadDeFormasDesde n 1

--especialDesde :: Integer -> Integer

--especialNumero :: Integer -> Integer

--esMuyEspecial :: Integer -> Bool


esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x




-- RES DANI
-- Ejer 5

especialNumero :: Integer -> Integer
especialNumero n = especialNumDesde 1 n

especialNumDesde k n | n == 1 = especialDesde k
                     | otherwise = especialNumDesde ((especialDesde k) + 1) (n-1)






minDivisorDesde :: Integer -> Integer -> Integer
minDivisorDesde n i | i == n = n
                    | (mod n i) == 0 = i
                    | otherwise = minDivisorDesde n (i+1)

verSiDivisorEsCuboDesde :: Integer -> Integer -> (Integer, Bool)
verSiDivisorEsCuboDesde n k | n == k = (n, esUnCubo n)
                            | (mod n k) == 0 = (k, esUnCubo k)
                            | otherwise = verSiDivisorEsCuboDesde n (k+1)





esEspecial :: Integer -> Bool
esEspecial n = especialDesde n == n
{-
esMuyEspecial:: Integer -> Integer -> Bool
-- k = 2
esMuyEspecial n k | esEspecial n == False = False
                  | n == k = True
                  | snd div_a == False = False
                  | ((mod b (fst div_a)) == 0) = False
                  | otherwise = esMuyEspecial n (k+1)
                  where d = descomposicionCubos n
                        a = fst d
                        b = snd d
                        div_a = verSiDivisorEsCuboDesde a k










minDivisorDesde :: Integer -> Integer -> Integer
minDivisorDesde n i | i == n = n
                    | (mod n i) == 0 = i
                    | otherwise = minDivisorDesde n (i+1)
-}




-- ACA COMIENZA EL EJER 6

-- aux que si le pasas 4 valores, te dice cual es el mas pequeño. Ejm: minCubo 1 2 3 4 debe devolver 1
minCubo:: Integer -> Integer -> Integer -> Integer -> Integer
minCubo a b c d | a <= b && a <= c && a <= d = a
                | c <= a && c <= b && c <= d = c
                | b <= a && b <= c && b <= d = b
                | otherwise = d

-- aux que si le doy 4 valores, me dice si es verdad o NO que hay un valor que sea minimo comun multiplo con los otros. Va probando con los divisores del valor mas pequeño que haya entre a,b,c,d. Contador comienza en x=2 y se detiene cuando es igual a elValorMasChico+1 (porque ya no sera divisor del valorMasChico)
-- x = 2
hayMcm :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
hayMcm a b c d x | x == (minVar+1) = False
                 | (mod a xDivisorDeLaMinVar == 0) && (mod b xDivisorDeLaMinVar == 0) && (mod c xDivisorDeLaMinVar == 0) && (mod d xDivisorDeLaMinVar == 0) = True
                 | otherwise = hayMcm a b c d (x+1)
                 where minVar = minCubo a b c d
                       xDivisorDeLaMinVar = minDivisorDesde minVar x


--Necesito los 4 valores a,b,c,d que cumplen que mi numero especial = a^3 + b^3 = c^3 + d^3. Esta funcion devuelve las dos tuplas (a,b) y (c,d) dentro de otra tupla, para poder manipularla despues

dameDosTuplas :: Integer -> Integer -> ( (Integer,Integer), (Integer,Integer) )
-- i = 1
dameDosTuplas n i = (d, descomposicionCubosDesde n ((fst d) + 1) )
                  where d = descomposicionCubosDesde n i 

esMuyEspecial:: Integer -> Bool
esMuyEspecial n | esEspecial n == False = False
                | hayMcm a b c d 2 = False
                | otherwise = True
                where laTupla = dameDosTuplas n 1
                      a = fst (fst laTupla)
                      b = snd (fst laTupla) 
                      c = fst (snd laTupla)
                      d = snd (snd laTupla)

