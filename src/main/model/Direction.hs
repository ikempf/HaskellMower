module Direction where

    data Direction = North | West | South | East deriving(Eq, Show)

    relativeLeft North = West
    relativeLeft West  = South
    relativeLeft South  = East
    relativeLeft East  = North

    relativeRight North = East
    relativeRight East  = South
    relativeRight South  = West
    relativeRight West  = North