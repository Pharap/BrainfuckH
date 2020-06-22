module Zip.State where
    import qualified Zip as Z
    import Control.Monad.Trans.State.Lazy

    type StateZip a = State (Z.Zip a)

    emptyZip :: Z.Zip a
    emptyZip = Z.emptyZip

    zipFromLeft :: [a] -> Z.Zip a
    zipFromLeft = Z.zipFromLeft

    zipFromRight :: [a] -> Z.Zip a
    zipFromRight = Z.zipFromRight

    zipAt :: Int -> [a] -> Z.Zip a
    zipAt = Z.zipAt

    zipToList :: Z.Zip a -> [a]
    zipToList = Z.zipToList

    nullLeft :: StateZip a Bool
    nullLeft = gets Z.nullLeft

    nullRight :: StateZip a Bool
    nullRight = gets Z.nullRight

    headLeft :: StateZip a a
    headLeft = gets Z.headLeft

    headRight :: StateZip a a
    headRight = gets Z.headRight

    tailLeft :: StateZip a [a]
    tailLeft = gets Z.tailLeft

    tailRight :: StateZip a [a]
    tailRight = gets Z.tailRight

    updateLeft :: (a -> a) -> StateZip a ()
    updateLeft f = modify (Z.updateLeft f)

    updateRight :: (a -> a) -> StateZip a ()
    updateRight f = modify (Z.updateRight f)

    consLeft :: a -> StateZip a ()
    consLeft x = modify (Z.consLeft x)

    consRight :: a -> StateZip a ()
    consRight x = modify (Z.consRight x)

    unconsLeft :: StateZip a a
    unconsLeft = gets Z.unconsLeft >>= f
        where f = maybe (fail "") (\(x, zip) -> put zip >> gets (const x))

    unconsRight :: StateZip a a
    unconsRight = gets Z.unconsRight >>= f
        where f = maybe (fail "") (\(x, zip) -> put zip >> gets (const x))

    rewindLeft :: StateZip a ()
    rewindLeft = modify Z.rewindLeft

    rewindRight :: StateZip a ()
    rewindRight = modify Z.rewindRight

    scrollLeft :: StateZip a ()
    scrollLeft = modify Z.scrollLeft

    scrollRight :: StateZip a ()
    scrollRight = modify Z.scrollRight

    skipLeft :: Int -> StateZip a ()
    skipLeft n = modify (Z.skipLeft n)

    skipRight :: Int -> StateZip a ()
    skipRight n = modify (Z.skipRight n)

    reverseZip :: StateZip a ()
    reverseZip = modify Z.reverseZip

    dropLeft :: Int -> StateZip a ()
    dropLeft n = modify (Z.dropLeft n)

    dropRight :: Int -> StateZip a ()
    dropRight n = modify (Z.dropRight n)

    takeLeft :: Int -> StateZip a ()
    takeLeft n = modify (Z.takeLeft n)

    takeRight :: Int -> StateZip a ()
    takeRight n = modify (Z.takeRight n)

    findLeft :: (a -> Bool) -> StateZip a ()
    findLeft p = gets (Z.findLeft p) >>= (maybe (fail "") put)

    findRight :: (a -> Bool) -> StateZip a ()
    findRight p = gets (Z.findRight p) >>= (maybe (fail "") put)

    --scrollExLeft :: a -> StateZip a ()
    --scrollExLeft x = modify (Z.scrollExLeft x)

    --scrollExRight :: a -> StateZip a ()
    --scrollExRight x = modify (Z.scrollExRight x)

    --updateExLeft :: a -> (a -> a) -> StateZip a ()
    --updateExLeft x f = modify (Z.updateExLeft x f)

    --updateExRight :: a -> (a -> a) -> StateZip a ()
    --updateExRight x f = modify (Z.updateExRight x f)
