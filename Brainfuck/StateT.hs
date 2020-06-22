module Brainfuck.StateT
    (
        Tape(..),
        TapeState,
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

    type TapeState a m = StateT (Tape a) m

    increment :: (Monad m, Num a) => TapeState a m ()
    increment = modify BF.increment

    decrement :: (Monad m, Num a) => TapeState a m ()
    decrement = modify BF.decrement

    left :: Monad m => TapeState a m ()
    left = modify BF.left

    right :: Monad m => TapeState a m ()
    right = modify BF.right

    getCell :: Monad m => TapeState a m a
    getCell = gets BF.getCell

    setCell :: Monad m => a -> TapeState a m ()
    setCell value = modify (BF.setCell value)

    output :: (Show a, Num a) => TapeState a IO ()
    output = getCell >>= \value -> lift (print value)

    input :: (Num a) => TapeState a IO ()
    input = lift getChar >>= \char -> setCell (fromIntegral $ fromEnum char)