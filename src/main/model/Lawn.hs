module Lawn where
    import Mower
    import Instruction

    data Lawn = Lawn {iMax::Integer, jMax::Integer, mowers::[Mower]} deriving (Eq, Show)
    
    runMowers (Lawn iMax jMax []) [] = []
    runMowers (Lawn iMax jMax (mower:ms)) (instructions:iss) =
        (executeInstructions mower instructions):(runMowers (Lawn iMax jMax ms) iss)

