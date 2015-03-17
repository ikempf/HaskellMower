module Main where
    import Test.HUnit

    import MowerTest
    import SpecificationTest

    main =  runTestTT $ TestList [sould_advance, should_turn_right, should_run_specifications]