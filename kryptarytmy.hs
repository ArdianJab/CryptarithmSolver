-- rownania musza byc zapisane w postaci stringow zlozonych z duzych liter
-- pomiedzy kazdym slowem i znakiem +,*,= jest spacja
-- np. A + B = AC

-- zwraca duze litery znajdujace sie w tablicy stringow, nie robi duplikatow
getLetters :: [String] -> [Char]
getLetters [] = []
getLetters s = getLetters' (concatenate s) ['A' .. 'Z']
    where
        concatenate :: [String] -> String
        concatenate [] = []
        concatenate (x:xs) = x ++ concatenate xs
        getLetters' :: String -> [Char] -> [Char]
        getLetters' s [] = []
        getLetters' s (x:xs)
            | x `elem` s =  x : getLetters' s xs
            | otherwise = getLetters' s xs

-- dostaje tablice liczb i tablice dzialan (+, *) w odpowiedniej kolejnosci pomiedzy nimi
-- wykonuje mnozenia, zwraca liste liczb ktore nalezy dodac
mnozenie :: [Int] -> [Char] -> [Int]
mnozenie x c = mnozenie' x c []
    where
        mnozenie' :: [Int] -> [Char] -> [Int] -> [Int]
        mnozenie' x [] r = r++x
        mnozenie' (x1:x2:xs) (c:cs) r
            | c == '*' = mnozenie' ([x1*x2] ++ xs) cs r
            | otherwise = mnozenie' ([x2] ++ xs) cs (r ++ [x1])

-- generuje wszystkie n-elementowe permutacje liczb [0..9]
permutations :: Int -> [[Int]]
permutations n = permutations' n [0..9]
  where
    permutations' :: Int -> [Int] -> [[Int]]
    permutations' 0 _  = [[]]                  
    permutations' _ [] = []                    
    permutations' k xs = [ x:ys | x <- xs, ys <- permutations' (k-1) (remove x xs) ]
    
    remove _ [] = []
    remove y (z:zs)
        | y == z    = zs
        | otherwise = z : remove y zs

-- znajduje wszystkie rozwiazania kryptarytmu w postaci tablicy tablic par (znak, liczba)
-- jesli go nie znajdzie, to zwraca pusta tablice
findSolution :: [String] -> [Char] -> String -> [[(Char,Int)]]
findSolution s c sol
    | length (getLetters ([sol]++s)) > 10 = []
    | length (f s c sol (getLetters ([sol]++s)) perms) > 0 = (f s c sol (getLetters ([sol]++s)) perms)
    | otherwise = []
    where
        perms = (permutations (length (getLetters ([sol]++s))))

        isFirstZero :: [Char] -> [Int] -> [Char] -> Bool
        isFirstZero _ _ [] = False
        isFirstZero letters numbers (x:xs)
            | isFirstZero' letters numbers x = True
            | otherwise = isFirstZero letters numbers xs
        isFirstZero' :: [Char] -> [Int] -> Char -> Bool
        isFirstZero' [] [] _ = False
        isFirstZero' (l:letters) (n:numbers) x
            | l == x && n == 0 = True
            | otherwise = isFirstZero' letters numbers x
        firstLetters :: [String] -> [Char]
        firstLetters [] = []
        firstLetters (s:ss) = (head s) : (firstLetters ss)
        podmienLitere :: [Char] -> [Int] -> Char -> Int
        podmienLitere (l:letters) (n:numbers) x
            | x == l = n
            | otherwise = podmienLitere letters numbers x
        toNumber :: [Int] -> Int
        toNumber [] = 0
        toNumber (x:xs) = x * (10 ^ length xs) + toNumber xs 
        podmienSlowo :: [Char] -> [Int] -> String -> Int
        podmienSlowo letters numbers str = (toNumber (map (podmienLitere letters numbers) str))
        f :: [String] -> [Char] -> String -> [Char] -> [[Int]] -> [[(Char,Int)]]
        f s c sol letters [] = []
        f s c sol letters perm
            | (not (isFirstZero letters (head perm) (firstLetters ([sol]++s)))) && ( sum ( mnozenie (map (podmienSlowo letters (head perm)) s) c ) ) == podmienSlowo letters (head perm) sol = sparuj letters (head perm) : (f s c sol letters (tail perm)) 
            | otherwise = f s c sol letters (tail perm)
        sparuj :: [Char] -> [Int] -> [(Char, Int)]
        sparuj [] [] = []
        sparuj (l:letters) (n:numbers) = (l, n) : sparuj letters numbers
        
-- bierze rownanie w postaci stringa, przerabia go odpowiednio zeby wrzucic je do findSolution i zwraca wynik
rozwiaz :: String -> [[(Char, Int)]]
rozwiaz str = findSolution (getLeft (words str)) (getChars (words str)) (getRight (words str))
    where
        getLeft :: [String] -> [String]
        getLeft (s:ss)
            | s == "=" = []
            | s == "*" || s == "+" = getLeft ss
            | otherwise = s:(getLeft ss)
        getChars :: [String] -> [Char]
        getChars (s:ss)
            | s == "=" = []
            | s == "*" || s == "+" = (head s):getChars ss
            | otherwise = getChars ss
        getRight :: [String] -> String
        getRight str = head (reverse str) 
        
-- wypisuje wszystkie mozliwe rozwiazania podanego rownania
wypisz :: String -> IO ()
wypisz x = do
    let rozwiazanie = rozwiaz x
    if rozwiazanie == [] then putStrLn "Nie znaleziono rozwiazania."
    else do
        wypiszRozwiazania rozwiazanie
    where
        podmienLitere :: [(Char, Int)] -> Char -> Char
        podmienLitere l '+' = '+'
        podmienLitere l '*' = '*'
        podmienLitere l '=' = '='
        podmienLitere l ' ' = ' '
        podmienLitere (l:ll) x
            | x == fst l = head (show (snd l))
            | otherwise = podmienLitere ll x

        wypiszRozwiazania :: [[(Char, Int)]] -> IO ()
        wypiszRozwiazania [] = return ()
        wypiszRozwiazania (s:sols) = do
            print s
            print (map (podmienLitere s) x)
            wypiszRozwiazania sols

main :: IO ()
main = do
    putStrLn "Z jakiego pliku wczytac dane?: "
    file <- getLine
    rownania <- lines <$> readFile file
    lbl rownania
    where
        lbl [] = putStrLn ""
        lbl (x:xs) = do
            putStrLn "Rownanie i jego rozwiazanie:"
            print x
            wypisz x
            putStrLn ""
            lbl xs