module Brainfuck.Parse
    (
        Node(..),
        parse,
        parseS,
        parseF,
    )
    where

    import Text.ParserCombinators.ReadP
    import Brainfuck.Node

    parse :: ReadP Node
    parse = parseNode

    parseS :: String -> [(Node, String)]
    parseS = readP_to_S parse

    parseF :: String -> Node
    parseF = fst . Prelude.last . parseS

    parseNode :: ReadP Node
    parseNode = between skipSpaces skipSpaces parseSequence

    nonSequenceParsers :: [ReadP Node]
    nonSequenceParsers = [parseLeft, parseRight, parseIncrement, parseDecrement, parseOutput, parseInput, parseLoop]

    parseNonSequence :: ReadP Node
    parseNonSequence = choice nonSequenceParsers >>= \node -> skipSpaces >> return node

    parseSequence :: ReadP Node
    parseSequence = many1 parseNonSequence >>= \nodes -> return $ SequenceNode nodes

    parseLeft :: ReadP Node
    parseLeft = char '<' >> return LeftNode

    parseRight :: ReadP Node
    parseRight = char '>' >> return RightNode

    parseIncrement :: ReadP Node
    parseIncrement = char '+' >> return IncrementNode

    parseDecrement :: ReadP Node
    parseDecrement = char '-' >> return DecrementNode

    parseOutput :: ReadP Node
    parseOutput = char '.' >> return OutputNode

    parseInput :: ReadP Node
    parseInput = char ',' >> return InputNode

    parseLoop :: ReadP Node
    parseLoop = between (char '[') (char ']') parseNode >>= \node -> return $ LoopNode node