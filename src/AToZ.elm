module AToZ exposing (AToZ(..), Case(..))

{-| Basic latin letter a|...|z

@docs AToZ, Case

-}


{-| Basic latin letter without [case](#Case) information
-}
type AToZ
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z


{-| Either lower or upper case
-}
type Case
    = CaseLower
    | CaseUpper
