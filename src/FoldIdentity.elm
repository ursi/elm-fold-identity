module FoldIdentity exposing
    ( maybe, maybe2, maybe3, maybe4, maybe5
    , bool, string, list, array, dict, set
    )

{-|


# Maybes

@docs maybe, maybe2, maybe3, maybe4, maybe5


# Convert Values to `Maybe`

@docs bool, string, list, array, dict, set

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


{-|

    import FoldIdentity as F
    import Html exposing (..)

    idH =
        text ""

    -- ...
    view model =
        div []
            [ someHtml
            , F.maybe idH
                (\value ->
                    -- contitional HTML
                )
                model.maybeThing
            , someMoreHtml
            ]

    -- ...

-}
maybe : i -> (a -> i) -> Maybe a -> i
maybe =
    (>>) Maybe.map
        << (<<)
        << Maybe.withDefault


{-|

    import FoldIdentity as F
    import Html exposing (..)

    idH =
        text ""

    -- ...
    view model =
        div []
            [ someHtml
            , F.maybe2 idH
                (\value _ ->
                    -- conditional HTML
                )
                model.maybeThing
                (F.bool model.boolean)
            , someMoreHtml
            ]

    -- ...

-}
maybe2 : i -> (a -> b -> i) -> Maybe a -> Maybe b -> i
maybe2 =
    (>>) Maybe.map2
        << (<<)
        << (<<)
        << Maybe.withDefault


{-| -}
maybe3 : i -> (a -> b -> c -> i) -> Maybe a -> Maybe b -> Maybe c -> i
maybe3 =
    (>>) Maybe.map3
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


{-| -}
maybe4 : i -> (a -> b -> c -> d -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> i
maybe4 =
    (>>) Maybe.map4
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


{-| -}
maybe5 : i -> (a -> b -> c -> d -> e -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> i
maybe5 =
    (>>) Maybe.map5
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


{-|

    bool b =
        if b then
            Just b

        else
            Nothing

-}
bool : Bool -> Maybe Bool
bool b =
    if b then
        Just b

    else
        Nothing


{-|

    string s =
        if String.isEmpty s then
            Nothing

        else
            Just s

-}
string : String -> Maybe String
string s =
    if String.isEmpty s then
        Nothing

    else
        Just s


{-|

    list l =
        if List.isEmpty l then
            Nothing

        else
            Just l

-}
list : List a -> Maybe (List a)
list l =
    if List.isEmpty l then
        Nothing

    else
        Just l


{-|

    array a =
        if Array.isEmpty a then
            Nothing

        else
            Just a

-}
array : Array a -> Maybe (Array a)
array a =
    if Array.isEmpty a then
        Nothing

    else
        Just a


{-|

    dict d =
        if Dict.isEmpty d then
            Nothing

        else
            Just d

-}
dict : Dict k v -> Maybe (Dict k v)
dict d =
    if Dict.isEmpty d then
        Nothing

    else
        Just d


{-|

    set s =
        if Set.isEmpty s then
            Nothing

        else
            Just s

-}
set : Set a -> Maybe (Set a)
set s =
    if Set.isEmpty s then
        Nothing

    else
        Just s
