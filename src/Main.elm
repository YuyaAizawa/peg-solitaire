module Main exposing (main)

import Browser
import Html exposing (Html)
import Json.Decode as Jd
import ListWrapper.Dict as Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events exposing (onMouseUp, onMouseDown)

type alias Attr msg = Svg.Attribute msg



main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }



-- MODEL --

type alias Model =
  { board : Dict Coords State
  , hold : Maybe Coords
  , mouse : Position
  }

type alias Coords =
  { x : Int
  , y : Int
  }

type State
  = Hole
  | Peg

type alias Position =
  { x : Float
  , y : Float
  }

init : Model
init =
  { board = initBoard
  , hold = Nothing
  , mouse = Position 0 0
  }

initBoard : Dict Coords State
initBoard =
  let
    insertAction ( x, y, c ) =
      case c of
          '·' -> Dict.insert (Coords x y) Peg
          'o' -> Dict.insert (Coords x y) Hole
          _   -> identity
  in
    [ "  ···  "
    , "  ···  "
    , "·······"
    , "···o···"
    , "·······"
    , "  ···  "
    , "  ···  "
    ]
      |> List.indexedMap(\y -> String.toList >> List.indexedMap (\x c -> ( x, y, c )))
      |> List.concat
      |> List.foldl insertAction Dict.empty



-- UPDATE --

type Msg
  = MouseMoved Position
  | Hold Coords
  | Drop Coords
  | Cancel

update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseMoved pos ->
      { model | mouse = pos }

    Hold coords ->
      { model | hold = Just coords }

    Drop to ->
      case ( model.hold, model.board |> Dict.get to) of
        ( Just from, Just Hole ) ->
          case getJumpOvered to from of
            Just jumpOvered ->
              { model | board =
                model.board
                  |> Dict.insert from Hole
                  |> Dict.insert jumpOvered Hole
                  |> Dict.insert to Peg
              }
            _ -> model
        _ -> model

    Cancel ->
      { model | hold = Nothing }



getJumpOvered : Coords -> Coords -> Maybe Coords
getJumpOvered to from =
  let
    dx = to.x - from.x
    dy = to.y - from.y

    toBeRemoved =
      Coords (from.x + dx//2) (from.y + dy//2)
  in
    case ( abs dx, abs dy ) of
      ( 2, 0 ) -> Just toBeRemoved
      ( 0, 2 ) -> Just toBeRemoved
      _        -> Nothing



-- VIEW --

viewBoxWidth = "560"
viewBoxHeight = "560"

gridSize = 80
gridMargin = 2

pegRadius = "20"

view : Model -> Html Msg
view { board, hold, mouse } =
  let
    svgHold =
      case hold of
        Nothing -> []
        _       -> [ svgPegHolding mouse ]

    svgBoard =
      board
        |> Dict.toList
        |> List.concatMap
          (\( coords, pegState ) ->
            case pegState of
              Hole ->
                [ svgHole coords ]
              Peg ->
                if hold /= Just coords
                then [ svgHole coords, svgPeg coords ]
                else [ svgHole coords ]
          )
  in
    Svg.svg
      [ onMouseMove (\x y -> MouseMoved <| Position x y)
      , onMouseUp Cancel
      , Attr.width viewBoxWidth
      , Attr.height viewBoxHeight
      , Attr.viewBox <| "0 0 "++viewBoxWidth++" "++viewBoxHeight
      , Attr.id "elm-area"
      ]
      (svgBoard ++ svgHold)

svgHole : Coords -> Svg Msg
svgHole ({ x, y } as coords) =
  Svg.rect
    [ onMouseUp <| Drop coords
    , Attr.x <| String.fromInt <| x * gridSize + gridMargin
    , Attr.y <| String.fromInt <| y * gridSize + gridMargin
    , Attr.width  <| String.fromInt <| gridSize - 2 * gridMargin
    , Attr.height <| String.fromInt <| gridSize - 2 * gridMargin
    , Attr.class "hole"
    ]
    []

svgPeg : Coords -> Svg Msg
svgPeg ({ x, y } as coords) =
  Svg.circle
    [ onMouseDown <| Hold coords
    , Attr.cx <| String.fromFloat <| (toFloat x + 0.5) * gridSize
    , Attr.cy <| String.fromFloat <| (toFloat y + 0.5) * gridSize
    , Attr.r pegRadius
    , Attr.class "peg"
    ]
    []

svgPegHolding : Position -> Svg msg
svgPegHolding { x, y } =
  Svg.circle
    [ Attr.pointerEvents "none" -- to bubble through mouse events
    , Attr.cx <| String.fromFloat x
    , Attr.cy <| String.fromFloat y
    , Attr.r pegRadius
    , Attr.class "peg-hold"
    ]
    []

onMouseMove : (Float -> Float -> msg) -> Attr msg
onMouseMove msg =
  Svg.Events.on "mousemove"
    (Jd.map2 msg
      (Jd.field "offsetX" Jd.float)
      (Jd.field "offsetY" Jd.float)
    )
