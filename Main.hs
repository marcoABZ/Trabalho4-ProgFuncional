{-- 1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n. --}
fatorialln :: Int -> Int
fatorialln x = foldr (*) 1 [1..x]

{-- 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados. --}
quadrado :: Double -> Double
quadrado x = x * x

quadradoReal :: [Double] -> [Double]
quadradoReal = map quadrado

{-- 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. ---} 
comprimentoPalavra :: String -> Int
comprimentoPalavra "" = 0
comprimentoPalavra (x:y) = 1 + comprimentoPalavra y

comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map comprimentoPalavra

{-- 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29. --}
multiploDe29 :: Int -> Bool
multiploDe29 x = x `mod` 29 == 0

maiorMultiploDe29 :: Int
maiorMultiploDe29 = last (filter multiploDe29 [1..100000])


{-- 5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. --}
--maiorMultiploDe :: Int -> Int
--maiorMultiploDe x = last (filter  [1..10000])



{-- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12 +22 +32 +42...+𝑛2. --}
somaDeQuadrados :: Int -> Int
somaDeQuadrados n = foldr (+) 0 [ x*x | x <- [1..n]]

{-- 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada. --}
retorna1 :: a -> Int
retorna1 x = 1

comprimento :: [a] -> Int
comprimento x = foldr (+) 0 (map retorna1 x)

{-- 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. --}

main = do
  putStrLn ("Func. 1: entrada: 4; resultado: " ++ show(fatorialln 4))
  putStrLn ("Func. 2: entrada: [-3.5, -2, 0, 3, 4.1]; resultado: " ++ show(quadradoReal [-3.5, -2, 0, 3, 4.1]))
  putStrLn ("Func. 3: entrada: ['', 'a', 'abc']; resultado: " ++ show(comprimentoPalavras ["", "a", "abc"]))
  putStrLn ("Func. 4: entrada: NA; resultado: " ++ show(maiorMultiploDe29))
  --putStrLn ("Func. 5: entrada: 5; resultado: " ++ show(maiorMultiploDe 5))
  putStrLn ("Func. 6: entrada: 7; resultado: " ++ show(somaDeQuadrados 7))
  putStrLn ("Func. 7: entrada: [1,2,3,4]; resultado: " ++ show(comprimento [1,2,3,4]))