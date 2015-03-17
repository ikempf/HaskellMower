module SpecificationTest where
    import Data.Char
    import Test.HUnit
    
    import Specification
    import Lawn
    import Direction
    import Mower
    import Instruction
    

    should_run_specifications =
        let (Lawn iMax jMax mowers, instructions) = parseSpecification specification in
        TestCase $ assertEqual "should_run_specifications" (5, 5, [(Mower 1 3 North), (Mower 5 1 East)]) (iMax, jMax, (runMowers (Lawn iMax jMax mowers) instructions))
        where
            specification = "5 5\n1 2 N\nGAGAGAGAA\n3 3 E\nAADAADADDA\n"
