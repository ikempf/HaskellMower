module Mower where
    import Direction
    import Instruction

    data Mower = Mower {i::Integer, j::Integer, orientation::Direction} deriving(Eq, Show)
    
    advance mower@(Mower i j orientation) = 
        case orientation of North   -> Mower i (j+1) orientation
                            East    -> Mower (i+1) j orientation
                            South   -> Mower i (j-1) orientation
                            West    -> Mower (i-1) j orientation

    executeInstructions mower [] = mower
    executeInstructions mower@(Mower i j orientation) (instruction:is) =
        case instruction of Instruction.Left    -> executeInstructions (Mower i j (relativeLeft orientation)) is
                            Instruction.Right   -> executeInstructions (Mower i j (relativeRight orientation)) is
                            Advance             -> executeInstructions (advance mower) is