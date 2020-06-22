module Brainfuck.Node where

    import Brainfuck.StateT
    import Data.List (foldl1')

    data Node
        = LeftNode
        | RightNode
        | IncrementNode
        | DecrementNode
        | OutputNode
        | InputNode
        | SequenceNode [Node]
        | LoopNode Node
        deriving Show

    --execute :: (Monad m, Show a, Num a) => Node -> TapeStateT a m ()
    execute :: (Show a, Eq a, Num a) => Node -> TapeStateT a IO ()
    execute LeftNode = left
    execute RightNode = right
    execute IncrementNode = increment
    execute DecrementNode = decrement
    execute OutputNode = output
    execute InputNode = input
    --execute (SequenceNode (x:[])) = execute x
    --execute (SequenceNode (x:xs)) = execute x >> execute (SequenceNode xs)
    execute (SequenceNode []) = error "SequenceNode must not be empty"
    execute (SequenceNode xs) = foldl1' (>>) $ map execute xs
    execute (LoopNode node) = getCell >>= f
        where
            f 0 = return ()
            f _ = execute node >> execute (LoopNode node)