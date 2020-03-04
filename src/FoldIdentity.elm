module FoldIdentity exposing
    ( Submodule
    , css
    , html
    , htmlAttributes
    , htmlStyled
    , htmlStyledAttributes
    , ifMap
    , ifMap2
    , ifMap3
    , ifMap4
    , ifMap5
    , if_
    , map
    , map2
    , map3
    , map4
    , map5
    , submodule
    )

import Css
import Html
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes


submodule : a -> Submodule a b c d e f
submodule identity =
    { identity = identity
    , if_ = if_ identity
    , map = map identity
    , map2 = map2 identity
    , map3 = map3 identity
    , map4 = map4 identity
    , map5 = map5 identity
    , ifMap = ifMap identity
    , ifMap2 = ifMap2 identity
    , ifMap3 = ifMap3 identity
    , ifMap4 = ifMap4 identity
    , ifMap5 = ifMap5 identity
    }


type alias Submodule a b c d e f =
    { identity : a
    , if_ : (Bool -> a) -> Bool -> a
    , map : (b -> a) -> Maybe b -> a
    , map2 : (b -> c -> a) -> Maybe b -> Maybe c -> a
    , map3 : (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
    , map4 : (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
    , map5 : (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
    , ifMap : (b -> a) -> Bool -> Maybe b -> a
    , ifMap2 : (b -> c -> a) -> Bool -> Maybe b -> Maybe c -> a
    , ifMap3 : (b -> c -> d -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> a
    , ifMap4 : (b -> c -> d -> e -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
    , ifMap5 : (b -> c -> d -> e -> f -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
    }


if_ : a -> (Bool -> a) -> Bool -> a
if_ identity toValue condition =
    if condition then
        toValue condition

    else
        identity


map : a -> (b -> a) -> Maybe b -> a
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


ifMap : a -> (b -> a) -> Bool -> Maybe b -> a
ifMap identity toValue condition maybeValue =
    if condition then
        case maybeValue of
            Just a ->
                toValue a

            Nothing ->
                identity

    else
        identity


ifMap2 : a -> (b -> c -> a) -> Bool -> Maybe b -> Maybe c -> a
ifMap2 identity toValue condition mb mc =
    if condition then
        case mb of
            Just b ->
                case mc of
                    Just c ->
                        toValue b c

                    Nothing ->
                        identity

            Nothing ->
                identity

    else
        identity


ifMap3 : a -> (b -> c -> d -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> a
ifMap3 identity toValue condition mb mc md =
    if condition then
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

    else
        identity


ifMap4 : a -> (b -> c -> d -> e -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
ifMap4 identity toValue condition mb mc md me =
    if condition then
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

    else
        identity


ifMap5 : a -> (b -> c -> d -> e -> f -> a) -> Bool -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
ifMap5 identity toValue condition mb mc md me mf =
    if condition then
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

    else
        identity


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
