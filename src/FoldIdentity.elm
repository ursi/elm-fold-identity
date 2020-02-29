module FoldIdentity exposing
    ( Submodule
    , css
    , html
    , htmlAttributes
    , htmlStyled
    , htmlStyledAttributes
    , ifMaybeMap
    , ifMaybeMap2
    , ifMaybeMap3
    , ifMaybeMap4
    , ifMaybeMap5
    , if_
    , maybeMap
    , maybeMap2
    , maybeMap3
    , maybeMap4
    , maybeMap5
    , submodule
    )

import Css
import Html
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes


submodule :
    a
    -> Submodule a b c d e f
submodule identity =
    { identity = identity
    , if_ = if_ identity
    , maybeMap = maybeMap identity
    , maybeMap2 = maybeMap2 identity
    , maybeMap3 = maybeMap3 identity
    , maybeMap4 = maybeMap4 identity
    , maybeMap5 = maybeMap5 identity
    , ifMaybeMap = ifMaybeMap identity
    , ifMaybeMap2 = ifMaybeMap2 identity
    , ifMaybeMap3 = ifMaybeMap3 identity
    , ifMaybeMap4 = ifMaybeMap4 identity
    , ifMaybeMap5 = ifMaybeMap5 identity
    }


type alias Submodule a b c d e f =
    { identity : a
    , if_ : Bool -> (Bool -> a) -> a
    , maybeMap : (b -> a) -> Maybe b -> a
    , maybeMap2 : (b -> c -> a) -> Maybe b -> Maybe c -> a
    , maybeMap3 : (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
    , maybeMap4 : (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
    , maybeMap5 : (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
    , ifMaybeMap : Bool -> (b -> a) -> Maybe b -> a
    , ifMaybeMap2 : Bool -> (b -> c -> a) -> Maybe b -> Maybe c -> a
    , ifMaybeMap3 : Bool -> (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
    , ifMaybeMap4 : Bool -> (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
    , ifMaybeMap5 : Bool -> (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
    }


if_ : a -> Bool -> (Bool -> a) -> a
if_ identity condition toValue =
    if condition then
        toValue condition

    else
        identity


maybeMap : a -> (b -> a) -> Maybe b -> a
maybeMap identity toValue maybeValue =
    case maybeValue of
        Just a ->
            toValue a

        Nothing ->
            identity


maybeMap2 : a -> (b -> c -> a) -> Maybe b -> Maybe c -> a
maybeMap2 identity toValue mb mc =
    case mb of
        Just b ->
            case mc of
                Just c ->
                    toValue b c

                Nothing ->
                    identity

        Nothing ->
            identity


maybeMap3 : a -> (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
maybeMap3 identity toValue mb mc md =
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


maybeMap4 : a -> (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
maybeMap4 identity toValue mb mc md me =
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


maybeMap5 : a -> (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
maybeMap5 identity toValue mb mc md me mf =
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


ifMaybeMap : a -> Bool -> (b -> a) -> Maybe b -> a
ifMaybeMap identity bool toValue maybeValue =
    if bool then
        case maybeValue of
            Just a ->
                toValue a

            Nothing ->
                identity

    else
        identity


ifMaybeMap2 : a -> Bool -> (b -> c -> a) -> Maybe b -> Maybe c -> a
ifMaybeMap2 identity bool toValue mb mc =
    if bool then
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


ifMaybeMap3 : a -> Bool -> (b -> c -> d -> a) -> Maybe b -> Maybe c -> Maybe d -> a
ifMaybeMap3 identity bool toValue mb mc md =
    if bool then
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


ifMaybeMap4 : a -> Bool -> (b -> c -> d -> e -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> a
ifMaybeMap4 identity bool toValue mb mc md me =
    if bool then
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


ifMaybeMap5 : a -> Bool -> (b -> c -> d -> e -> f -> a) -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> a
ifMaybeMap5 identity bool toValue mb mc md me mf =
    if bool then
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
