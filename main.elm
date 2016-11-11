module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String

import Http
import Task exposing (Task)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Base64

main : Program Never
main = 
    Html.program 
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
    
{- 
    MODEL
    * Model type 
    * Initialize model with empty values
    * Initialize with a random quote
-}

type alias TogglEntry = 
  { id: Int
    , pid: Maybe Int
    , tid: Maybe Int
    , uid: Maybe Int
    , description: String
    , start: String
    , end: String
    , updated: String
    , dur: Int
    , user: String
    , useStop: Bool
    , client: Maybe String
    , project: Maybe String
    , task: Maybe String
    , billable: Maybe Float
    , isBillable: Bool
    , cur: Maybe String
    , tags: List String
  }

type alias Model =
    { username : String
    , logged : Bool
    , data : List TogglEntry
    , errorMsg : String
    }
    
init : (Model, Cmd Msg)
init =
    ( Model "" False [] "", Cmd.none )
    
{-
    UPDATE
    * API routes
    * GET and POST
    * Encode request body 
    * Decode responses
    * Messages
    * Update case 
-}

-- API request URLs
api : String
api = "https://toggl.com/reports/api/v2/details"
     

randomQuoteUrl : String
randomQuoteUrl =    
    Http.url api [ 
  ("user_agent", "both")  -- required
  , ("workspace_id", "1587771")  -- required
  , ("grouping", "projects")
  , ("subgrouping", "time_entries") 
  , ("order_field", "duration") 
  , ("order_desc", "off") 
  , ("rounding", "on") 
  , ("since", "2016-08-22") 
  , ("until", "2016-08-28") 
  , ("date_format","DD%2FMM%2FYYYY")
  ]
    
registerUrl : String
registerUrl =
    Http.url api [ 
  ("user_agent", "both")  -- required
  , ("workspace_id", "1587771")  -- required
  , ("grouping", "projects")
  , ("subgrouping", "time_entries") 
  , ("order_field", "duration") 
  , ("order_desc", "off") 
  , ("rounding", "on") 
  , ("since", "2016-08-22") 
  , ("until", "2016-08-28") 
  , ("date_format","DD%2FMM%2FYYYY")
  ] 
 
-- POST register / GetData request

getTokenB64: String -> String -> String
getTokenB64 username password = 
              let 
                encode =  Base64.encode(username ++":" ++ password)        
              in case encode of
                Ok success -> success
                Err error -> ""
    
getData : Model -> String -> Task Http.Error (List TogglEntry)
getData model apiUrl =
    { verb = "GET"
    , headers = [ 
      ("Content-Type", "application/json")
      , ("Authorization", "Basic " ++ getTokenB64 model.username "api_token") 
      ]
    , url = apiUrl
    , body = Http.empty --body = Http.string <| Encode.encode 0 <| userEncoder model
    }
    |> Http.send Http.defaultSettings
    |> Http.fromJson dataDecoder    
    
getDataCmd : Model -> String -> Cmd Msg    
getDataCmd model apiUrl = 
    Task.perform AuthError GetDataSuccess <| getData model apiUrl
    
-- Decode POST response to get token
  
decodeEntry : Decoder TogglEntry
decodeEntry =  
  Decode.succeed TogglEntry
        |: ("id" := int) 
        |: ("pid" := maybe int)
        |: ("tid" := maybe int)
        |: ("uid" := maybe int)
        |: ("description" := string)
        |: ("start" := string)
        |: ("end" := string)
        |: ("updated" := string)  
        |: ("dur" := int)
        |: ("user" := string)
        |: ("use_stop" := bool)
        |: ("client" := maybe string)
        |: ("project" := maybe string)
        |: ("task"  := maybe string)
        |: ("billable" := maybe float) 
        |: ("is_billable" := bool)    
        |: ("cur" := maybe string)
        |: ("tags" := Decode.list string)
        

       
dataDecoder : Decoder (List TogglEntry)
dataDecoder =
    Decode.at ["data"] (Decode.list decodeEntry)     
    
-- Messages

type Msg 
    = GetData
    | HttpError Http.Error
    | AuthError Http.Error
    | SetUsername String
    | GetDataSuccess (List TogglEntry) 
 
-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        
        HttpError _ ->
            ( model, Cmd.none )  

        AuthError error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )  

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        GetData ->
            ( model, getDataCmd model registerUrl )

        GetDataSuccess newData ->
            ( { model | logged = True, errorMsg = "", data = newData } |> Debug.log "logged in", Cmd.none )  
                       
{-
    VIEW
    * Hide sections of view depending on authenticaton state of model
    * Get a quote
    * Register
-}



view : Model -> Html Msg
view model =
    let 
        
        listItem data = li [] [text data.description]
        -- If the user is logged in, show a greeting; if logged out, show the GetData/register form
        authBoxView =
            let
                -- If there is an error on authentication, show the error alert
                showError : String
                showError = 
                    if String.isEmpty model.errorMsg then "hidden" else ""  

                -- Greet a logged in user by username
                greeting : String
                greeting =
                    "Hello, " ++ model.username ++ "!"

            in        
                if model.logged then
                    div [id "greeting" ][
                        h3 [ class "text-center" ] [ text greeting ]
                        , p [ class "text-center" ] [ text "You have super-secret access to protected quotes." ]  
                        , div [ class "container" ] [
                            h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
                            , p [ class "text-center" ] [
                                button [ class "btn btn-success", onClick GetData ] [ text "Grab a quote!" ]
                            ]
                            -- Blockquote with data
                            , div [] [ 
                                p [] (List.map (listItem) model.data)
                            ]
                            
                        ]
                    ] 
                else
                    div [ id "form" ] [
                        h2 [ class "text-center" ] [ text "Log In" ]
                        , p [ class "help-block" ] [ text "If you already have an account, please Log In." ]
                        , div [ class showError ] [
                            div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                        , div [ class "form-group row" ] [
                            div [ class "col-md-offset-2 col-md-8" ] [
                                label [ for "username" ] [ text "Token: String"]
                                , input [ id "username", type' "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                            ]    
                        ]
                        , div [ class "text-center" ] [
                            button [ class "btn btn-link", onClick GetData ] [ text "Log in" ]
                        ] 
                    ]
                           
    in
        div [ class "jumbotron text-left" ] [
            -- GetData/Register form or user greeting
            authBoxView
        ]