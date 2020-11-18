module PageUpdater exposing
    ( PageUpdater
    , addCmd
    , addGlobalAction
    , fromPair
    , init
    , map
    , setGlobalActions
    )

import GlobalActions as GA exposing (GlobalAction)


type alias PageUpdater model msg =
    { globalActions : List (GlobalAction msg)
    , update : ( model, Cmd msg )
    }


init : model -> PageUpdater model msg
init model =
    fromPair ( model, Cmd.none )


fromPair : ( model, Cmd msg ) -> PageUpdater model msg
fromPair pair =
    { globalActions = []
    , update = pair
    }


addCmd : Cmd msg -> PageUpdater model msg -> PageUpdater model msg
addCmd cmd pageUpdater =
    let
        ( model, oldCmd ) =
            pageUpdater.update
    in
        { pageUpdater | update = ( model, Cmd.batch [ oldCmd, cmd ] ) }


setGlobalActions : List (GlobalAction msg) -> PageUpdater model msg -> PageUpdater model msg
setGlobalActions globalActions pageUpdater =
    { pageUpdater | globalActions = globalActions }


addGlobalAction : GlobalAction msg -> PageUpdater model msg -> PageUpdater model msg
addGlobalAction globalAction pageUpdater =
    { pageUpdater | globalActions = globalAction :: pageUpdater.globalActions }


map : (subModel -> model) -> (subMsg -> msg) -> PageUpdater subModel subMsg -> PageUpdater model msg
map mapModel mapMsg subUpdater =
    let
        ( subModel, subCmd ) =
            subUpdater.update
    in
        { globalActions = List.map (GA.map mapMsg) subUpdater.globalActions
        , update = ( mapModel subModel, Cmd.map mapMsg subCmd )
        }
