
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

main : Program Never Model Msg
main =
    Html.program
        { init = init_model 
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- MODEL

type alias Model = 
        { board: List (List Char)
         , turn: Bool
         , winner: Bool
         , draw: Bool
        }

type alias Cell = 
        { x: Int
        , y: Int
        , player: Char
        }

init_model : (Model, Cmd msg)
init_model = 
    ({ board = 
        [ [ ' ', ' ', ' ' ]
        , [ ' ', ' ', ' ' ]
        , [ ' ', ' ', ' ' ]
        ]
    , turn = True
    , winner = False
    , draw = False
    }, Cmd.none)

-- UPDATE

type Msg
    = Reset
    | Move Int Int

update: Msg -> Model -> ( Model, Cmd msg )
update msg model = 
    case msg of 
        Reset ->
            init_model
        
        Move x y ->
            if model.winner then
                model ! [] 
            else 
                let
                    new_board = 
                            update_cell model x y

                    new_winner =
                        check_winner new_board
                    
                    is_draw = 
                        check_draw new_board 
                    
                    new_turn = 
                            if model.board == new_board then model.turn else not model.turn
                in 
                    { model | board = new_board, turn = new_turn, winner = new_winner, draw = is_draw} ! [] 

update_cell : Model -> Int -> Int -> List (List Char)
update_cell model xx yy =
    (List.indexedMap
        (\y row ->
            List.indexedMap
                (\x cell ->
                    if x == xx && y == yy && cell == ' ' then
                        get_player model.turn
                    else
                        cell
                )
                row
        )
        model.board
    )

check_winner : List (List Char) -> Bool
check_winner board =
    (check_row board || check_col board || check_diag board)

is_equal : List Char -> Bool
is_equal list =
    case List.head list of
        Just ' ' ->
            False

        Just value ->
            List.foldr
                (\element status ->
                    if status then
                        element == value
                    else
                        False
                )
                True
                list

        Nothing ->
            False

take_el : Int -> List a -> List a
take_el index list =
    List.take 1 <| List.drop index list

check_diag : List (List Char) -> Bool
check_diag board =
    let
        d_1 =
            List.concat <|
                List.indexedMap
                    (\index row ->
                        take_el index row
                    )
                <|
                    board

        d_2 =
            List.concat <|
                List.indexedMap
                    (\index row ->
                        take_el (2 - index) row
                    )
                <|
                    board
    in
        (is_equal d_1 || is_equal d_2)
    
check_col : List (List Char) -> Bool
check_col board =
    List.length (List.filter (\status -> status) <| List.map is_equal board) > 0

check_row : List (List Char) -> Bool
check_row board =
    let
        rows =
            List.indexedMap
                (\index _ ->
                    List.concat <|
                        List.map
                            (\row ->
                                take_el index row
                            )
                            board
                )
            <|
                List.range 0 2
    in
        List.length (List.filter (\status -> status) <| List.map is_equal rows) > 0

check_draw : List (List Char) -> Bool
check_draw board =
    not <| List.member ' ' (List.foldr List.append [] board)

get_player : Bool -> Char 
get_player player =  
    if player then 'X' else 'O'

get_player_str : Bool -> String 
get_player_str player =  
     if player then "Player 1" else "Player 2"
     

-- VIEW 

view : Model -> Html Msg 
view model = 
    div [style[("text-align",("center"))]]
        [ h1 [ style[("margin-bottom","0")] ] [ text "X / O using ELM" ]
        , display_info
        , display_winner model
        , div[style[
            ("display","flex"),
            ("justify-content","center"),
            ("text-align","center"),
            ("margin","0 auto")
        ]][make_board model]
        , reset_btn
        ]

display_winner : Model -> Html Msg
display_winner model =
    h1 []
        [ if model.winner then
            text ("Winner: " ++ (get_player_str (not model.turn)))
          else if model.draw then
            text "DRAW!"
          else
            text ((get_player_str model.turn) ++ "'s Turn")
        ]

display_info: Html Msg
display_info =
    h4 []
        [ 
            text ("Project for CSCE 3104: Concepts of Programming Languages")
        ]

reset_btn : Html Msg
reset_btn =
    button [ type_ "button", onClick Reset ] [ text "Restart" ]

make_board : Model -> Html Msg
make_board model =
    div [ style [] ]
        [ ul [style[("padding","0")]]
            (List.indexedMap
                (\y board_row ->
                    make_board_cells y board_row
                )
                model.board
            )
        ]

make_board_cells : Int -> List Char -> Html Msg
make_board_cells y board_row =
    li [style[("display","flex"), ("list-style","none")]]
        (List.indexedMap
            (\x cell ->
                div
                    [ style [
                        ("width","100px"),
                        ("height","100px"),
                        ("border-style","solid"),
                        ("background-color","#26e4b6"),
                        ("color","black"),
                        ("font-size","72px")
                    ], onClick (Move x y) ]
                    [ text 
                        (String.fromChar cell)
                    ]
            )
            board_row
        )