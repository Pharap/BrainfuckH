import Control.Monad.Trans.State.Lazy

import Zip (getRight)
import Brainfuck.Tape
import Brainfuck.StateT
import Brainfuck.Node
import Brainfuck.Parse

main = do
    -- Get a line of input
    line <- getLine
    let
        -- Parse the input into a Node structurs
        node = parseF line
        -- Make a Tape of 10 cells initialised with 0
        tape = fillTape 10 0
    execStateT (execute node) tape >>= (print . getRight)