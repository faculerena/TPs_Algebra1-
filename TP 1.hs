-- D'alessandro Valentina 
-- Galizia Antonella 
-- Lerena Facundo 

-- EJERCICIO 1. sonCoprimos

menorDivDesde :: Integer -> Integer -> Integer
menorDivDesde n m | mod n m == 0 = m
                  | otherwise = menorDivDesde n (m + 1)

menorDiv :: Integer -> Integer
menorDiv 1 = 1
menorDiv n = menorDivDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = menorDiv n == n

mcd :: Integer -> Integer -> Integer --Algoritmo euclideo para mcd, recursivo: sean a, b dos numeros enteros
mcd x 0 = x                          --mod a b = r, realizar la funcion de manera recursiva con a = b y b = r, repetir hasta
mcd x y = mcd y (mod x y)            --que de 0


sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1 

-- EJE3RCICO 2: es2Pseudoprimo

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo a = mod b a == 0  && not (esPrimo a)
                where b = ((2 ^ (a - 1)) - 1)

-- EJERCICIO 3: cantidad3Pseudoprimos

esNPseudoprimo :: Integer -> Integer -> Bool
esNPseudoprimo a n = mod b a == 0 && not (esPrimo a)
                where b = ((n ^ (a - 1)) - 1)

cantidad3PseudoprimosHasta :: Integer -> Integer -> Integer -> Integer
cantidad3PseudoprimosHasta m c i | m == i - 1 = c -- se fija si ya llegaste a dar m pasos, le resta 1 a i porque si m es 3-Pseudoprimo va hasta el segundo renglon
                                 | esNPseudoprimo i 3 = cantidad3PseudoprimosHasta m (c + 1) (i + 1)
                                 | otherwise = cantidad3PseudoprimosHasta m c (i + 1)

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cantidad3PseudoprimosHasta m 0 1     --empezando con 0 3-Pseudoprimos en el contador, empezando a recorrer el intervalo [1..m]


-- EJERCICIO 4: kesimo2y3Pseudoprimo

kesimo2y3PseudoprimoDesde :: Integer -> Integer -> Integer -> Integer
kesimo2y3PseudoprimoDesde m k i | k == m = i - 1      -- cuando el contador k llega a mi valor deseado m, devuelve i-1 para corregir el indice que sufre la recurrencia 
                                | (esNPseudoprimo i 2 && esNPseudoprimo i 3) = kesimo2y3PseudoprimoDesde m (k + 1) (i + 1)
                                | otherwise = kesimo2y3PseudoprimoDesde m k (i + 1)

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = kesimo2y3PseudoprimoDesde n 0 1

-- EJERCICIO 5: esCarmichael

esCarmichaelAux :: Integer -> Integer -> Bool        
esCarmichaelAux n i | (not (esPrimo n) && n - 1 == i) = True
                    | not (sonCoprimos n i) = esCarmichaelAux n (i + 1)  --pasamos de largo los que no son coprimos con n
                    | (sonCoprimos n i && esNPseudoprimo n i) == True = esCarmichaelAux n (i + 1)  --nos fijamos que n i sean coprimos y n sea i-pseudoprimo
                    | otherwise = False  --sino, no es carmichael 
                       
esCarmichael :: Integer -> Bool
esCarmichael c = esCarmichaelAux c 1