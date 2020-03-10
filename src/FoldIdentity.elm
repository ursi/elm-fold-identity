module FoldIdentity exposing
    ( Submodule
    , array
    , bool
    , css
    , dict
    , html
    , htmlAttributes
    , htmlStyled
    , htmlStyledAttributes
    , list
    , map
    , map2
    , map3
    , map4
    , map5
    , string
    , submodule
    )

import Array exposing (Array)
import Css
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes
import Set exposing (Set)


type alias Submodule i a b c d e =
    { identity : i
    , map : (a -> i) -> Maybe a -> i
    , map2 : (a -> b -> i) -> Maybe a -> Maybe b -> i
    , map3 : (a -> b -> c -> i) -> Maybe a -> Maybe b -> Maybe c -> i
    , map4 : (a -> b -> c -> d -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> i
    , map5 : (a -> b -> c -> d -> e -> i) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> i
    }


submodule : i -> Submodule i a b c d e
submodule identity =
    { identity = identity
    , map = map identity
    , map2 = map2 identity
    , map3 = map3 identity
    , map4 = map4 identity
    , map5 = map5 identity
    }


map : i -> (a -> i) -> Maybe a -> i
map identity toValue maybeValue =
    case maybeValue of
        Just a ->
            toValue a

        Nothing ->
            identity


map2 : a -> (b -> c -> a) -> Maybe b -> Maybe c -> a
map2 identity toValue mb mc =
    case mb of
        Just b ->
            case mc of
                Just c ->
                    toValue b c

                Nothing ->
                    identity

        Nothing ->
            identity


map3 : a -> (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
map3 identity toValue mb mc md =
    case mb of
        Just b ->
            case mc of
                Just c ->
                    case md of
                        Just d ->
                            toValue b c d

                        Nothing ->
                            identity

                Nothing ->
                    identity

        Nothing ->
            identity


map4 : a -> (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
map4 identity toValue mb mc md me =
    case mb of
        Just b ->
            case mc of
                Just c ->
                    case md of
                        Just d ->
                            case me of
                                Just e ->
                                    toValue b c d e

                                Nothing ->
                                    identity

                        Nothing ->
                            identity

                Nothing ->
                    identity

        Nothing ->
            identity


map5 : a -> (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
map5 identity toValue mb mc md me mf =
    case mb of
        Just b ->
            case mc of
                Just c ->
                    case md of
                        Just d ->
                            case me of
                                Just e ->
                                    case mf of
                                        Just f ->
                                            toValue b c d e f

                                        Nothing ->
                                            identity

                                Nothing ->
                                    identity

                        Nothing ->
                            identity

                Nothing ->
                    identity

        Nothing ->
            identity


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


css : Submodule Css.Style b c d e f
css =
    submodule <| Css.batch []


html : Submodule (Html.Html msg) b c d e f
html =
    submodule <| Html.text ""


htmlAttributes : Submodule (Html.Attribute msg) b c d e f
htmlAttributes =
    submodule <| Html.Attributes.classList []


htmlStyled : Submodule (Html.Styled.Html msg) b c d e f
htmlStyled =
    submodule <| Html.Styled.text ""


htmlStyledAttributes : Submodule (Html.Styled.Attribute msg) b c d e f
htmlStyledAttributes =
    submodule <| Html.Styled.Attributes.classList []
