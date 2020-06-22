module Brainfuck.Tape where

    import Zip

    type Tape = Zip

    tape :: [a] -> Tape a
    tape = zipFromRight

    emptyTape :: Tape a
    emptyTape = tape []

    fillTape :: Int -> a -> Tape a
    fillTape count value = tape $ replicate count value

    increment :: Num a => Tape a -> Tape a
    increment = updateRight (+ 1)

    decrement :: Num a => Tape a -> Tape a
    decrement = updateRight (subtract 1)

    left :: Tape a -> Tape a
    left = scrollLeft

    right :: Tape a -> Tape a
    right = scrollRight

    getCell :: Tape a -> a
    getCell = headRight

    setCell :: a -> Tape a -> Tape a
    setCell value = updateRight (const value)