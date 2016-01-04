import Circle exposing (Model, init, update, view)

import StartApp
import Task
import Html exposing (Html)
import Effects exposing (Never)

app : StartApp.App Model
app =
    StartApp.start
        { init = init 0 100
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks



view address model =
     