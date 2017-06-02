module PhotoGroove exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

initialModel =
    { photos =
        [ { url = "1.jpeg" }
      , { url = "2.jpeg" }
      , { url = "3.jpeg" }
      ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix =
  "http://elm-in-action.com/"


viewThumbnail selectedUrl thumbnail =
    img [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick { operation = "SELECT_PHOTO", data = thumbnail.url }
        ]
        []


update msg model =
  if msg.operation == "SELECT_PHOTO" then
    { model | selectedUrl = msg.data }
  else
      model


view model =
  div [ class "content" ]
      [ h1 [] [ text "Photo Groove" ]
      , div [ id "thumbnails" ]
          (List.map (viewThumbnail model.selectedUrl) model.photos)
      , img
          [ class "large"
          , src (urlPrefix ++ "large/" ++ model.selectedUrl)
          ]
          []
      ]


main =
  Html.beginnerProgram
      { model = initialModel
      , view = view
      , update = update
      }
