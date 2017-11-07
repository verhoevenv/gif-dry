port module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

import Debug

main =
  Html.program
    { init = init "kitten"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- CircularBuffer



type alias BufferIndex = Int

type alias CircularFIFOBuffer a =  
  { contents : Array a
  , maxSize : Int
  , begin : BufferIndex
  , end : BufferIndex
  }

emptyBuffer : Int -> CircularFIFOBuffer a
emptyBuffer maxSize = CircularFIFOBuffer empty maxSize 0 0

increase_index : CircularFIFOBuffer a -> BufferIndex -> BufferIndex
increase_index buff idx =
  if idx == (buff.maxSize - 1) then 0 else idx + 1

add : a -> CircularFIFOBuffer a -> CircularFIFOBuffer a
add elem buff =
  if (length buff.contents) < buff.maxSize then
    {buff | contents = push elem buff.contents
          , end = increase_index buff buff.end
    }
  else
    {buff | contents = set buff.end elem buff.contents
          , end = increase_index buff buff.end
    }

step : CircularFIFOBuffer a -> CircularFIFOBuffer a
step buff =
  if (length buff.contents) == 0 then buff
  else if (buff.end == buff.begin) then buff
  else {buff | begin = increase_index buff buff.begin}


toStableList : (a -> BufferIndex -> Bool -> b) -> CircularFIFOBuffer a -> List b
toStableList mappingFun buff =
  List.map (\(idx, elem) -> mappingFun elem idx (idx == buff.begin)) <| toIndexedList buff.contents

-- MODEL


type alias Gif = 
  { url : String
  }

-- The circular buffer is to prevent a problem with the naive list implementation:
-- if we keep the list of video's as list, and we map that to a list of <video> dom nodes,
-- the virtual dom will add new video-tags instead of reusing the existing tags.
-- This results in obvious delay when switching to a new video.
-- Solution: keep the same list of video tags, rotating their source and only showing one as non-hidden
-- The next problem there is starting the video when it becomes unhidden, which is solved with some Javascript interop
type alias Model =
  { topic : String
  , gifUrlBuffer : CircularFIFOBuffer Gif
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic (emptyBuffer 4)
  , Cmd.batch [getRandomGif topic, getRandomGif topic, getRandomGif topic]
  )



-- UPDATE


type Msg
  = ShowNextGif
  | NewGifUrl (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowNextGif ->
      let 
        newBuff = step model.gifUrlBuffer
        _ = Debug.log "newbuff" newBuff
      in
        ({model | gifUrlBuffer = newBuff }, Cmd.batch [playVideo ("video-" ++ toString newBuff.begin), getRandomGif model.topic])

    NewGifUrl (Ok newUrl) ->
      ({model | gifUrlBuffer = add (Gif newUrl) model.gifUrlBuffer }, Cmd.none)

    NewGifUrl (Err _) ->
      (model, Cmd.none)



-- VIEW
onEnded msg =
  on "ended" (Decode.succeed msg)

view : Model -> Html Msg
view model =
  div []
    (
      [ h2 [] [text model.topic]
      , button [ onClick ShowNextGif ] [ text "More Please!" ]
      , br [] []
      ]
      ++ toStableList (\gif idx isBegin -> video [id ("video-" ++ toString idx), src gif.url, preload "auto", hidden (not isBegin), onEnded ShowNextGif] []) model.gifUrlBuffer
    )



-- SUBSCRIPTIONS
getVideoIfAvailable : List String -> Html Msg
getVideoIfAvailable buffer = 
  case buffer of
    someVideo :: others -> video [src someVideo, autoplay True] []
    [] -> p [] [text "Loading..."]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- PORT

port playVideo : String -> Cmd msg

-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGifUrl (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_mp4_url"] Decode.string
