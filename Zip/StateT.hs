module Zip.StateT where
    import qualified Zip as Z
    import Control.Monad.Trans.State.Lazy

    type ZipStateT a m = StateT (Z.Zip a) m

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

    nullLeft :: Monad m => ZipStateT a m Bool
    nullLeft = gets Z.nullLeft

    nullRight :: Monad m => ZipStateT a m Bool
    nullRight = gets Z.nullRight

    headLeft :: Monad m => ZipStateT a m a
    headLeft = gets Z.headLeft

    headRight :: Monad m => ZipStateT a m a
    headRight = gets Z.headRight

    tailLeft :: Monad m => ZipStateT a m [a]
    tailLeft = gets Z.tailLeft

    tailRight :: Monad m => ZipStateT a m [a]
    tailRight = gets Z.tailRight

    updateLeft :: Monad m => (a -> a) -> ZipStateT a m ()
    updateLeft f = modify (Z.updateLeft f)

    updateRight :: Monad m => (a -> a) -> ZipStateT a m ()
    updateRight f = modify (Z.updateRight f)

    consLeft :: Monad m => a -> ZipStateT a m ()
    consLeft x = modify (Z.consLeft x)

    consRight :: Monad m => a -> ZipStateT a m ()
    consRight x = modify (Z.consRight x)

    unconsLeft :: Monad m => ZipStateT a m a
    unconsLeft = gets Z.unconsLeft >>= f
        where f = maybe (fail "") (\(x, zip) -> put zip >> gets (const x))

    unconsRight :: Monad m => ZipStateT a m a
    unconsRight = gets Z.unconsRight >>= f
        where f = maybe (fail "") (\(x, zip) -> put zip >> gets (const x))

    rewindLeft :: Monad m => ZipStateT a m ()
    rewindLeft = modify Z.rewindLeft

    rewindRight :: Monad m => ZipStateT a m ()
    rewindRight = modify Z.rewindRight

    scrollLeft :: Monad m => ZipStateT a m ()
    scrollLeft = modify Z.scrollLeft

    scrollRight :: Monad m => ZipStateT a m ()
    scrollRight = modify Z.scrollRight

    skipLeft :: Monad m => Int -> ZipStateT a m ()
    skipLeft n = modify (Z.skipLeft n)

    skipRight :: Monad m => Int -> ZipStateT a m ()
    skipRight n = modify (Z.skipRight n)

    reverseZip :: Monad m => ZipStateT a m ()
    reverseZip = modify Z.reverseZip

    dropLeft :: Monad m => Int -> ZipStateT a m ()
    dropLeft n = modify (Z.dropLeft n)

    dropRight :: Monad m => Int -> ZipStateT a m ()
    dropRight n = modify (Z.dropRight n)

    takeLeft :: Monad m => Int -> ZipStateT a m ()
    takeLeft n = modify (Z.takeLeft n)

    takeRight :: Monad m => Int -> ZipStateT a m ()
    takeRight n = modify (Z.takeRight n)

    findLeft :: Monad m => (a -> Bool) -> ZipStateT a m ()
    findLeft p = gets (Z.findLeft p) >>= (maybe (fail "") put)

    findRight :: Monad m => (a -> Bool) -> ZipStateT a m ()
    findRight p = gets (Z.findRight p) >>= (maybe (fail "") put)

    --scrollExLeft :: Monad m => a -> ZipStateT a m ()
    --scrollExLeft x = modify (Z.scrollExLeft x)

    --scrollExRight :: Monad m => a -> ZipStateT a m ()
    --scrollExRight x = modify (Z.scrollExRight x)

    --updateExLeft :: Monad m => a -> (a -> a) -> ZipStateT a m ()
    --updateExLeft x f = modify (Z.updateExLeft x f)

    --updateExRight :: Monad m => a -> (a -> a) -> ZipStateT a m ()
    --updateExRight x f = modify (Z.updateExRight x f)
