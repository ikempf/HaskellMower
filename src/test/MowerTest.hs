module MowerTest where
    import Data.Char
    import Test.HUnit

    import Direction
    import Mower
    import Instruction
    
    
    sould_advance =
        let Mower i j orientation = executeInstructions mower instructions in
        TestCase $ assertEqual "should_advance" (2, 4, North) (i, j, orientation)
        where
            mower = Mower 2 3 North
            instructions = [Advance]

    should_turn_right =
        let Mower i j orientation = executeInstructions mower instructions in
        TestCase $ assertEqual "sould_execute_instruction_and_set_right_position" (1, 5, West) (i, j, orientation)
        where
            mower = Mower 1 5 North
            instructions = [Instruction.Left]