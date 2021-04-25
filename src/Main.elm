module Main exposing (main)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (Parser, (|.), (|=), symbol, float, succeed, Trailing(..))
import Browser 
type alias Term =
                { coeff : Float 
                , exp   : Float
                }

term_parser : Parser Term 
term_parser = 
    succeed Term 
    |= float 
    |. symbol "x"
    |. symbol "^"
    |= float 

term_string : Term -> String 
term_string term =  String.fromFloat term.coeff ++ "x^("++ String.fromFloat term.exp  ++")"

type alias Polynomial = List Term 

poly_parser : Parser Polynomial 
poly_parser = 
    Parser.sequence 
    { start = ""
    , separator = "+"
    , end = ""
    , spaces = Parser.spaces
    , item = term_parser 
    , trailing = Forbidden
    }

type alias Model = { poly_str : String, polynomial: Polynomial }

type Msg
    =  ParsePolynomial String

init : Model 
init = { polynomial = [] , poly_str = "" }

update : Msg -> Model -> Model 
update msg model = 
        case msg of
            ParsePolynomial poly_str  -> 
                let 
                    new_model = {model | poly_str = poly_str } 
                    is_parsed = Parser.run poly_parser poly_str
                    _ = Debug.log "parsed poly" is_parsed
                in 
                    case is_parsed of 
                        Ok poly -> 
                            {new_model | polynomial = poly }
                        Err _ -> 
                           new_model  

                    
main = 
    Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

view : Model -> Html Msg 
view model = 
    div 
        [] 
        [ input [ placeholder "Type polynomial", value model.poly_str, onInput ParsePolynomial ] []
        ]

