port module PhotoGroove exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (id, class, classList, src, name, max, type_, title)
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random
import Http exposing (Response)


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title: String
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type alias Model =
    { photos : List Photo
    , status : String
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Msg
    = SelectByUrl String
      | SetHue Int
      | SetRipple Int
      | SetNoise Int
      | SupriseMe
      | SetSize ThumbnailSize
      | SelectByIndex Int
      | SetStatus String
      | LoadPhotos (Result Http.Error (List Photo))


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus (Response String)
    | BadPayload String (Response String)


type ThumbnailSize
    = Small
    | Medium
    | Large


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


initialModel : Model
initialModel =
    { photos = []
    , status = ""
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            Just photo.url
        Nothing ->
           Nothing


selectPhoto : { operation : String, data : String }
selectPhoto = { operation = "SELECT_PHOTO", data = "1.jpeg" }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
          , title (thumbnail.title ++ "  [" ++ toString thumbnail.size ++ " KB]")
          , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
          , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
      Small ->
          "small"

      Medium ->
          "med"

      Large ->
          "large"



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetStatus status ->
          ( { model | status = status }, Cmd.none )

        SetHue hue ->
          applyFilters { model | hue = hue }

        SetRipple ripple ->
          applyFilters { model | ripple = ripple }

        SetNoise noise ->
          applyFilters { model | noise = noise }

        LoadPhotos (Ok photos) ->
          applyFilters
            { model
              | photos = photos
              , selectedUrl = Maybe.map .url (List.head photos)
            }

        LoadPhotos (Err _ ) ->
            ( { model
                | loadingError = Just "Error! (Try turning it off and on again?)"
              }
            , Cmd.none
            )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

        SelectByUrl selectedUrl ->
            applyFilters { model | selectedUrl = Just selectedUrl }

        SupriseMe ->
           let
               randomPhotoPicker =
                 Random.int 0 (List.length model.photos - 1)
           in
              ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
  case model.selectedUrl of
    Just selectedUrl ->
      let
          filters =
            [ { name = "Hue", amount = toFloat model.hue / 11 }
            , { name = "Ripple", amount = toFloat model.ripple / 11 }
            , { name = "Noise", amount = toFloat model.noise / 11 }
            ]

          url =
            urlPrefix ++ "large/" ++ selectedUrl
      in
         ( model, setFilters { url = url, filters = filters } )

    Nothing ->
      ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SupriseMe ]
            [ text "Suprise Me!" ]
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]

viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""
        Just url ->
            canvas [ id "main-canvas", class "large" ] []

viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


main : Program Float Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = \_ -> statusChanges SetStatus
        }


init : Float -> ( Model, Cmd Msg )
init flags =
  let
      status =
        "Initializing Pasta v" ++ toString flags
  in
     ( { initialModel | status = status }, initialCmd )


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"

