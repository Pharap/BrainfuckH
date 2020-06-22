module Brainfuck.StateT
    (
        Tape(..),
        TapeStateT,
        tape,
        emptyTape,
        fillTape,
        right,
        left,
        increment,
        decrement,
        input,
        output,
        getCell,
        setCell,
    )
    where

    import Control.Monad.Trans.State.Lazy
    import Control.Monad.Trans.Class

    import qualified Brainfuck.Tape as BF (increment, decrement, left, right, getCell, setCell)
    import Brainfuck.Tape hiding (increment, decrement, left, right, getCell, setCell)

    type TapeStateT a m = StateT (Tape a) m

    increment :: (Monad m, Num a) => TapeStateT a m ()
    increment = modify BF.increment

    decrement :: (Monad m, Num a) => TapeStateT a m ()
    decrement = modify BF.decrement

    left :: Monad m => TapeStateT a m ()
    left = modify BF.left

    right :: Monad m => TapeStateT a m ()
    right = modify BF.right

    getCell :: Monad m => TapeStateT a m a
    getCell = gets BF.getCell

    setCell :: Monad m => a -> TapeStateT a m ()
    setCell value = modify (BF.setCell value)

    output :: (Show a, Num a) => TapeStateT a IO ()
    output = getCell >>= \value -> lift (print value)

    input :: (Num a) => TapeStateT a IO ()
    input = lift getChar >>= \char -> setCell (fromIntegral $ fromEnum char)