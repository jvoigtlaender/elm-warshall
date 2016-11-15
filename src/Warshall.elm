module Warshall exposing (warshall)

{-| The Warshall Algorithm, on type `Matrix` from the
[`eeue56/elm-flat-matrix` package](http://package.elm-lang.org/packages/eeue56/elm-flat-matrix/latest).

@docs warshall
-}

import Matrix exposing (Matrix)


connectedIn : Matrix Bool -> Int -> Int -> Bool
connectedIn matrix i j =
    Maybe.withDefault False (Matrix.get i j matrix)


{-| `warshall 4` on matrix

    [ [ False, True,  False, False ]
    , [ False, False, True,  False ]
    , [ False, False, False, True  ]
    , [ False, False, False, False ]
    ]

gives matrix

    [ [ False, True,  True,  True  ]
    , [ False, False, True,  True  ]
    , [ False, False, False, True  ]
    , [ False, False, False, False ]
    ]
-}
warshall : Int -> Matrix Bool -> Matrix Bool
warshall n matrix =
    let
        step k matrix =
            Matrix.indexedMap (\i j t -> t || connectedIn matrix i k && connectedIn matrix k j) matrix
    in
        List.foldl step matrix (List.range 0 (n - 1))
