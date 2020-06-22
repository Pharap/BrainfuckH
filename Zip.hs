module Zip where

    data Zip a = Zip [a] [a] deriving (Eq, Show)

    instance Functor Zip where
        --fmap :: (a -> b) -> Zip a -> Zip b
        fmap f (Zip ls rs) = Zip (fmap f ls) (fmap f rs)

    instance Foldable Zip where
        -- foldMap :: Monoid m => (a -> m) -> Zip a -> m
        foldMap f (Zip [] []) = mempty
        foldMap f (Zip [] rs) = mconcat (fmap f rs)
        foldMap f (Zip (x:ls) rs) = foldMap f (Zip ls (x:rs))

    --instance Traversable Zip where
        --traverse :: Applicative f => (a -> f b) -> Zip a -> f (Zip b)
        --traverse f (Zip [] []) = pure (Zip [] [])

    emptyZip :: Zip a
    emptyZip = Zip [] []

    getLeft :: Zip a -> [a]
    getLeft (Zip ls _) = ls

    getRight :: Zip a -> [a]
    getRight (Zip _ rs) = rs

    zipFromLeft :: [a] -> Zip a
    zipFromLeft ls = Zip ls []

    zipFromRight :: [a] -> Zip a
    zipFromRight rs = Zip [] rs

    zipAt :: Int -> [a] -> Zip a
    zipAt index list = Zip (reverse ls) rs where (ls, rs) = splitAt index list

    zipToList :: Zip a -> [a]
    zipToList (Zip ls rs) = (++) (reverse ls) rs

    (=:=) :: (Eq a) => Zip a -> Zip a -> Bool
    (=:=) l r = (==) (zipToList l) (zipToList r)

    infix 4 =:=

    (/:=) :: (Eq a) => Zip a -> Zip a -> Bool
    (/:=) l r = (/=) (zipToList l) (zipToList r)

    infix 4 /:=

    nullLeft :: Zip a -> Bool
    nullLeft (Zip [] _) = True
    nullLeft _ = False

    nullRight :: Zip a -> Bool
    nullRight (Zip _ []) = True
    nullRight _ = False

    headLeft :: Zip a -> a
    headLeft (Zip [] _) = error "Zip has no more left values"
    headLeft (Zip (x:_) _) = x

    headRight :: Zip a -> a
    headRight (Zip _ []) = error "Zip has no more right values"
    headRight (Zip _ (x:_)) = x

    tailLeft :: Zip a -> [a]
    tailLeft (Zip [] _) = error "Zip has no more left values"
    tailLeft (Zip (_:ls) _) = ls

    tailRight :: Zip a -> [a]
    tailRight (Zip _ []) = error "Zip has no more right values"
    tailRight (Zip _ (_:rs)) = rs

    updateLeft :: (a -> a) -> Zip a -> Zip a
    updateLeft f (Zip [] _) = error "Zip has no more left values"
    updateLeft f (Zip (x:ls) rs) = Zip ((f x):ls) rs

    updateRight :: (a -> a) -> Zip a -> Zip a
    updateRight f (Zip _ []) = error "Zip has no more right values"
    updateRight f (Zip ls (x:rs)) = Zip ls ((f x):rs)

    consLeft :: a -> Zip a -> Zip a
    consLeft x (Zip ls rs) = Zip (x:ls) rs

    consRight :: a -> Zip a -> Zip a
    consRight x (Zip ls rs) = Zip ls (x:rs)

    unconsLeft :: Zip a -> Maybe (a, Zip a)
    unconsLeft (Zip (x:ls) rs) = Just (x, (Zip ls rs))
    unconsLeft (Zip [] _) = Nothing

    unconsRight :: Zip a -> Maybe (a, Zip a)
    unconsRight (Zip ls (x:rs)) = Just (x, (Zip ls rs))
    unconsRight (Zip _ []) = Nothing

    rewindLeft :: Zip a -> Zip a
    rewindLeft (Zip ls (x:rs)) = rewindLeft (Zip (x:ls) rs)
    rewindLeft z@(Zip _ []) = z

    rewindRight :: Zip a -> Zip a
    rewindRight (Zip (x:ls) rs) = rewindRight (Zip ls (x:rs))
    rewindRight z@(Zip [] _) = z

    scrollLeft :: Zip a -> Zip a
    scrollLeft (Zip [] _) = error "Zip has no more left values"
    scrollLeft (Zip (x:ls) rs) = (Zip ls (x:rs))

    scrollRight :: Zip a -> Zip a
    scrollRight (Zip _ []) = error "Zip has no more right values"
    scrollRight (Zip ls (x:rs)) = (Zip (x:ls) rs)

    skipLeft :: Int -> Zip a -> Zip a
    skipLeft 0 zip = zip
    skipLeft _ (Zip [] _) = error "Zip has no more left values"
    skipLeft n (Zip (x:ls) rs) = skipLeft (n - 1) (Zip ls (x:rs))

    skipRight :: Int -> Zip a -> Zip a
    skipRight 0 zip = zip
    skipRight _ (Zip _ []) = error "Zip has no more right values"
    skipRight n (Zip ls (x:rs)) = skipRight (n - 1) (Zip (x:ls) rs)

    reverseZip :: Zip a -> Zip a
    reverseZip (Zip [] []) = Zip [] []
    reverseZip (Zip [] rs) = Zip (reverse rs) []
    reverseZip (Zip ls []) = Zip [] (reverse ls)
    reverseZip (Zip ls rs) = Zip (reverse rs) (reverse ls)

    dropLeft :: Int -> Zip a -> Zip a
    dropLeft 0 zip = zip
    dropLeft n (Zip ls rs) = Zip (drop n ls) rs

    dropRight :: Int -> Zip a -> Zip a
    dropRight 0 zip = zip
    dropRight n (Zip ls rs) = Zip ls (drop n rs)

    takeLeft :: Int -> Zip a -> Zip a
    takeLeft 0 _ = Zip [] []
    takeLeft n (Zip ls _) = Zip (take n ls) []

    takeRight :: Int -> Zip a -> Zip a
    takeRight 0 _ = Zip [] []
    takeRight n (Zip _ rs) = Zip [] (take n rs)

    findLeft :: (a -> Bool) -> Zip a -> Maybe (Zip a)
    findLeft _ (Zip [] _) = Nothing
    findLeft p zip@(Zip (x:ls) rs)
        | p x = Just zip
        | otherwise = findLeft p (Zip ls (x:rs))

    findRight :: (a -> Bool) -> Zip a -> Maybe (Zip a)
    findRight _ (Zip _ []) = Nothing
    findRight p zip@(Zip ls (x:rs))
        | p x = Just zip
        | otherwise = findRight p (Zip (x:ls) rs)

    -- scrollExLeft :: a -> Zip a -> Zip a
    -- scrollExLeft x (Zip [] rs) = Zip [] (x:rs)
    -- scrollExLeft _ (Zip (x:ls) rs) = (Zip ls (x:rs))

    -- scrollExRight :: a -> Zip a -> Zip a
    -- scrollExRight x (Zip ls []) = Zip (x:ls) []
    -- scrollExRight _ (Zip ls (x:rs)) = (Zip (x:ls) rs)

    -- updateExLeft :: a -> (a -> a) -> Zip a -> Zip a
    -- updateExLeft x f (Zip [] rs) = Zip [f x] rs
    -- updateExLeft _ f (Zip (x:ls) rs) = Zip ((f x):ls) rs

    -- updateExRight :: a -> (a -> a) -> Zip a -> Zip a
    -- updateExRight x f (Zip ls []) = Zip ls [f x]
    -- updateExRight _ f (Zip ls (x:rs)) = Zip ls ((f x):rs)