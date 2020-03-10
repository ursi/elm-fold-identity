module FoldIdentity exposing
    ( Operand(..)
    , map, map2, map3, map4, map5, andThen
    , bool, maybe, string, list, array, dict
    , resolve
    , css, html, htmlAttributes, htmlStyled, htmlStyledAttributes
    )

{-|

@docs Operand
@docs map, map2, map3, map4, map5, andThen
@docs bool, maybe, string, list, array, dict, set
@docs resolve
@docs css, html, htmlAttributes, htmlStyled, htmlStyledAttributes

-}

import Array exposing (Array)
import Css
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes
import Set exposing (Set)


type Operand a
    = Identity
    | NotIdentity a


resolve : i -> Operand i -> i
resolve ident operand =
    case operand of
        NotIdentity value ->
            value

        Identity ->
            ident


map : (a -> b) -> Operand a -> Operand b
map f oa =
    case oa of
        NotIdentity a ->
            NotIdentity <| f a

        Identity ->
            Identity


map2 : (a -> b -> c) -> Operand a -> Operand b -> Operand c
map2 f oa ob =
    case oa of
        NotIdentity a ->
            case ob of
                NotIdentity b ->
                    NotIdentity <| f a b

                Identity ->
                    Identity

        Identity ->
            Identity


map3 : (a -> b -> c -> d) -> Operand a -> Operand b -> Operand c -> Operand d
map3 f oa ob oc =
    case oa of
        NotIdentity a ->
            case ob of
                NotIdentity b ->
                    case oc of
                        NotIdentity c ->
                            NotIdentity <| f a b c

                        Identity ->
                            Identity

                Identity ->
                    Identity

        Identity ->
            Identity


map4 : (a -> b -> c -> d -> e) -> Operand a -> Operand b -> Operand c -> Operand d -> Operand e
map4 f oa ob oc od =
    case oa of
        NotIdentity a ->
            case ob of
                NotIdentity b ->
                    case oc of
                        NotIdentity c ->
                            case od of
                                NotIdentity d ->
                                    NotIdentity <| f a b c d

                                Identity ->
                                    Identity

                        Identity ->
                            Identity

                Identity ->
                    Identity

        Identity ->
            Identity


map5 : (a -> b -> c -> d -> e -> f) -> Operand a -> Operand b -> Operand c -> Operand d -> Operand e -> Operand f
map5 f oa ob oc od of_ =
    case oa of
        NotIdentity a ->
            case ob of
                NotIdentity b ->
                    case oc of
                        NotIdentity c ->
                            case od of
                                NotIdentity d ->
                                    case of_ of
                                        NotIdentity f_ ->
                                            NotIdentity <| f a b c d f_

                                        Identity ->
                                            Identity

                                Identity ->
                                    Identity

                        Identity ->
                            Identity

                Identity ->
                    Identity

        Identity ->
            Identity


andThen : (a -> Operand b) -> Operand a -> Operand b
andThen f oa =
    case oa of
        NotIdentity a ->
            f a

        Identity ->
            Identity


bool : Bool -> Operand Bool
bool bool_ =
    if bool_ then
        NotIdentity bool_

    else
        Identity


maybe : Maybe a -> Operand a
maybe ma =
    case ma of
        Just a ->
            NotIdentity a

        Nothing ->
            Identity


string : String -> Operand String
string str =
    if String.isEmpty str then
        Identity

    else
        NotIdentity str


list : List a -> Operand (List a)
list list_ =
    if List.isEmpty list_ then
        Identity

    else
        NotIdentity list_


array : Array a -> Operand (Array a)
array array_ =
    if Array.isEmpty array_ then
        Identity

    else
        NotIdentity array_


dict : Dict k v -> Operand (Dict k v)
dict dict_ =
    if Dict.isEmpty dict_ then
        Identity

    else
        NotIdentity dict_


set : Set a -> Operand (Set a)
set set_ =
    if Set.isEmpty set_ then
        Identity

    else
        NotIdentity set_


css : Css.Style
css =
    Css.batch []


html : Html.Html msg
html =
    Html.text ""


htmlAttributes : Html.Attribute msg
htmlAttributes =
    Html.Attributes.classList []


htmlStyled : Html.Styled.Html msg
htmlStyled =
    Html.Styled.text ""


htmlStyledAttributes : Html.Styled.Attribute msg
htmlStyledAttributes =
    Html.Styled.Attributes.classList []
