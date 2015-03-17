module Specification where
    import Text.Parsec
    import Text.Parsec.String
    import System.Environment
    import System.Exit
    import Debug.Trace

    import Lawn
    import Mower
    import Direction
    import Instruction

    parseCoordinates :: Parser (Integer, Integer)
    parseCoordinates = do
        i <- many1 digit
        spaces
        j <- many1 digit
        return (read i, read j)
        
    parseDirection :: Parser Direction
    parseDirection = do
        direction <- letter
        return (case direction of
            'N' -> North
            'E' -> East
            'S' -> South
            'W' -> West)

    parseInstruction :: Parser Instruction
    parseInstruction = do
        instruction <- letter
        return (case instruction of
                    'A' -> Advance
                    'G' -> Instruction.Left
                    'D' -> Instruction.Right)

    parseMower :: Parser Mower
    parseMower = do
        (i, j) <- parseCoordinates
        spaces
        direction <- parseDirection
        newline
        return (Mower i j direction)

    parseMowerAndInstructions :: Parser (Mower, [Instruction])
    parseMowerAndInstructions = do
        mower <- parseMower
        instructions <- many parseInstruction
        newline
        return (mower, instructions)
        
    parseMowers :: Parser [(Mower, [Instruction])]
    parseMowers = do
        mowerWithInstructions <- many parseMowerAndInstructions
        return mowerWithInstructions
        
    parseLawn :: Parser (Lawn, [[Instruction]])
    parseLawn = do
        (iMax, jMax) <- parseCoordinates
        newline
        mowersWithInstructions <- parseMowers
        return ((Lawn iMax jMax (map fst mowersWithInstructions)), (map snd mowersWithInstructions))

    parseSpecification :: String -> (Lawn, [[Instruction]])
    parseSpecification specification =
        case parse parseLawn "specification" specification of
            Prelude.Right (lawn@(Lawn a b c), instructions) -> (lawn, instructions)
            Prelude.Left err -> trace (show err) ((Lawn 0 0 []), [])