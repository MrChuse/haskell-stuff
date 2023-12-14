-- import qualified GHC.Types
main = do
    putStr "replicate 5 5 "
    print $ replicate 5 5
    putStr "isNothing' Nothing "
    print $ isNothing' Nothing
    putStr "isNothing' $ Just 5 "
    print $ isNothing' $ Just 5
    putStr "fromJust' $ Just 5 "
    print $ fromJust' $ Just 5
    putStr "map' (+1) [1,2,3] "
    print $ map' (+1) [1,2,3]
    putStr "fst' (1, 2) "
    print $ fst' (1, 2)
    putStr "snd' (1, 2) "
    print $ snd' (1, 2)
    putStr "partialSumsl [1, 2, 3] "
    print $ partialSumsl [1, 2, 3]
    putStr "partialSumsr [1, 2, 3] "
    print $ partialSumsr [1, 2, 3]
    putStr "1 `elem'` [1,2,3] "
    print $ 1 `elem'` [1,2,3]
    putStr "4 `elem'` [1,2,3] "
    print $ 4 `elem'` [1,2,3]
    putStr "1 `elem''` [1,2,3] "
    print $ 1 `elem''` [1,2,3]
    putStr "4 `elem''` [1,2,3] "
    print $ 4 `elem''` [1,2,3]
    putStr "1 `notElem'` [1,2,3] "
    print $ 1 `notElem'` [1,2,3]
    putStr "4 `notElem'` [1,2,3] "
    print $ 4 `notElem'` [1,2,3]
    putStr "prodN [[1, 2], [3, 4], [5, 6]] "
    print $ prodN [[1, 2], [3, 4], [5, 6]]
    putStr "prodN [replicate 3 [1, 2]] "
    print $ prodN $ replicate 3 [1, 2]
    putStr "zip' [1, 2, 3] ['a', 'b', 'c'] "
    print $ zip' [1, 2, 3] ['a', 'b', 'c']
    putStr "zipN ([1, 2, 3], [1, 2, 3]) "
    print $ zipN [[1, 2, 3], [1, 2, 3]]



replicate' 0 _ = []
replicate' times thing = thing : replicate' (times-1) thing

isNothing' Nothing = True
isNothing' (Just _) = False

fromJust' Nothing = error "fromJust must be called on a Just"
fromJust' (Just x) = x

-- mapM_' :: (a -> GHC.Types.Any b) -> GHC.Types.Any a -> GHC.Types.Any ()
-- mapM_' func xs = mapM_ func xs

map' f [] = []
map' f (x:xs) =  f x : map' f xs

fst' (a, _) = a
snd' (_, b) = b



foldl' func acc [] = acc
foldl' func acc (x:xs) = foldl' func (func acc x) xs

flip' f a b = f b a

foldr' func acc [] = acc
foldr' func acc (x:xs) = func x (foldr' func acc xs)

partialSumsl = tail . reverse . foldl' (\acc x -> (head acc + x):acc) [0]
partialSumsr = init . foldr' (\x acc -> (head acc + x):acc) [0]

elem' thing = foldr' (\x -> (||) (thing == x)) False
elem'' thing [] = False
elem'' thing (x:xs) = (thing == x) || elem'' thing xs

notElem' thing xs = not $ elem' thing xs

prodN [] = []
prodN xs = foldr (\t acc -> [y:ys | ys <- acc, y <- t]) (map (: []) (last xs)) (init xs)

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

zipN lists
    | minimum (map length lists) == 0 = []
    | otherwise = map head lists:zipN (map tail lists)
