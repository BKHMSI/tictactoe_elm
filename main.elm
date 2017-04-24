
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- main : Program Never
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }

-- MODEL

type alias Model = 
        { board: List (List Cell)
         , turn: Bool
         , winner: Int
         , draw: Bool
        }

type alias Cell = 
        { x: Int
        , y: Int
        , player: Int
        }

initModel : Model
initModel = 
    { board = 
        [ [ Cell 0 0 1, Cell 1 0 0, Cell 2 0 1, Cell 3 0 0, Cell 4 0 1, Cell 5 0 0, Cell 6 0 1, Cell 7 0 0 ]
        , [ Cell 0 1 0, Cell 1 1 1, Cell 2 1 0, Cell 3 1 1, Cell 4 1 0, Cell 5 1 1, Cell 6 1 0, Cell 7 1 1 ]
        , [ Cell 0 2 1, Cell 1 2 0, Cell 2 2 1, Cell 3 2 0, Cell 4 2 1, Cell 5 2 0, Cell 6 2 1, Cell 7 2 0 ]
        , [ Cell 0 3 0, Cell 1 3 0, Cell 2 3 0, Cell 3 3 0, Cell 4 3 0, Cell 5 3 0, Cell 6 3 0, Cell 7 3 0 ]
        , [ Cell 0 4 0, Cell 1 4 0, Cell 2 4 0, Cell 3 4 0, Cell 4 4 0, Cell 5 4 0, Cell 6 4 0, Cell 7 4 0 ]
        , [ Cell 0 5 0, Cell 1 5 2, Cell 2 5 0, Cell 3 5 2, Cell 4 5 0, Cell 5 5 2, Cell 6 5 0, Cell 7 5 2 ]
        , [ Cell 0 6 2, Cell 1 6 0, Cell 2 6 2, Cell 3 6 0, Cell 4 6 2, Cell 5 6 0, Cell 6 6 2, Cell 7 6 0 ]
        , [ Cell 0 7 0, Cell 1 7 2, Cell 2 7 0, Cell 3 7 2, Cell 4 7 0, Cell 5 7 2, Cell 6 7 0, Cell 7 7 2 ]
        ]
    , turn = True
    , winner = 0
    , draw = False
    }

-- UPDATE

type Msg
    = Reset
    | Move Int Int

update: Msg -> Model -> Model
update msg model = 
    case msg of 
        Reset ->
            initModel
        
        Move x y ->
            let
                new_board = 
                    if model.winner /= 0 then 
                        updateCell model x y 
                    else 
                        model.board
                
                new_turn = not model.turn
            in 
                { model | board = new_board, turn = new_turn, winner = 0, draw = False}


updateCell : Model -> Int -> Int -> List (List Cell)
updateCell model x y =
    (List.map
        (\row ->
            List.map
                (\cell ->
                    if cell.x == x && cell.y == y && cell.player == 0 then
                        { cell | player = 1 }
                    else
                        cell
                )
                row
        )
        model.board
    )

getPlayer : Int -> String 
getPlayer player =  
    if player == 0 then " " else if player == 1 then "X" else "O"

-- VIEW 

view : Model -> Html Msg 
view model = 
    div []
        [ h1 [ ] [ text "Checkers" ]
        , showWinner model
        , makeBoard model
        , clearButton
        ]

showWinner : Model -> Html Msg
showWinner model =
    h1 []
        [ if model.winner /= 0 then
            text ("Winner = " ++ (getPlayer model.winner))
          else if model.draw then
            text "DRAW!"
          else
            text (( if model.turn then "Player 1" else "Player 2") ++ "'s Turn")
        ]

clearButton : Html Msg
clearButton =
    button [ type_ "button", onClick Reset ] [ text "Restart" ]

makeBoard : Model -> Html Msg
makeBoard model =
    div [ class "board" ]
        [ ul []
            (List.map
                (\boardRow ->
                    makeBoardCells boardRow
                )
                model.board
            )
        ]

makeBoardCells : List Cell -> Html Msg
makeBoardCells boardRow =
    li []
        (List.map
            (\cell ->
                div
                    [ class "button", onClick (Move cell.x cell.y) ]
                    [ text
                        (getPlayer cell.player)
                    ]
            )
            boardRow
        )