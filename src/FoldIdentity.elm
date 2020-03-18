module FoldIdentity exposing
    ( map, map2, map3, map4, map5
    , bool, string, list, array, dict
    )

{-|

@docs map, map2, map3, map4, map5
@docs bool, string, list, array, dict, set

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


map : i -> (a -> i) -> Maybe a -> i
map =
    (>>) Maybe.map
        << (<<)
        << Maybe.withDefault


map2 : i -> (a -> b -> i) -> Maybe a -> Maybe b -> i
map2 =
    (>>) Maybe.map2
        << (<<)
        << (<<)
        << Maybe.withDefault


map3 : i -> (a -> b -> c -> i) -> Maybe a -> Maybe b -> Maybe c -> i
map3 =
    (>>) Maybe.map3
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


map4 : i -> (a -> b -> c -> d -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> i
map4 =
    (>>) Maybe.map4
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


map5 : i -> (a -> b -> c -> d -> e -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> i
map5 =
    (>>) Maybe.map5
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << (<<)
        << Maybe.withDefault


bool : Bool -> Maybe Bool
bool bool_ =
    if bool_ then
        Just bool_

    else
        Nothing


string : String -> Maybe String
string str =
    if String.isEmpty str then
        Nothing

    else
        Just str


list : List a -> Maybe (List a)
list list_ =
    if List.isEmpty list_ then
        Nothing

    else
        Just list_


array : Array a -> Maybe (Array a)
array array_ =
    if Array.isEmpty array_ then
        Nothing

    else
        Just array_


dict : Dict k v -> Maybe (Dict k v)
dict dict_ =
    if Dict.isEmpty dict_ then
        Nothing

    else
        Just dict_


set : Set a -> Maybe (Set a)
set set_ =
    if Set.isEmpty set_ then
        Nothing

    else
        Just set_
