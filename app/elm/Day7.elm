module Day7 exposing (dependencies, part1, part2)

import Data.Day7 exposing (inputStr)
import Dict exposing (Dict)
import Set exposing (Set)



-- Section: Input Parsing


type alias StepId =
    Int


dependencies : Dict StepId (Set StepId)
dependencies =
    let
        emptyDeps =
            List.range 65 90
                |> List.map
                    (\id ->
                        ( id
                        , Set.empty
                        )
                    )
                |> Dict.fromList
    in
    inputStr
        |> String.lines
        |> List.map parsePair
        |> List.foldl
            (\( stepId, dep ) deps ->
                Dict.update stepId (addDep dep) deps
            )
            emptyDeps


addDep : StepId -> Maybe (Set StepId) -> Maybe (Set StepId)
addDep id deps =
    case deps of
        Nothing ->
            Just (Set.singleton id)

        Just depSet ->
            Just (Set.insert id depSet)


parsePair : String -> ( StepId, StepId )
parsePair depStr =
    let
        middle =
            String.slice 5 -11 depStr
                |> String.toList

        dependency =
            List.head middle
                |> Maybe.map Char.toCode
                |> Maybe.withDefault 0

        char =
            List.reverse middle
                |> List.head
                |> Maybe.map Char.toCode
                |> Maybe.withDefault 0
    in
    ( char, dependency )



-- Section: Part One


part1 : Dict StepId (Set StepId) -> String
part1 deps =
    -- Calling clearDep with a dummy step finds all initially ready steps
    doSteps [] (clearDep 0 ( [], deps ))
        |> List.map Char.fromCode
        |> String.fromList


doSteps : List StepId -> ( List StepId, Dict StepId (Set StepId) ) -> List StepId
doSteps proccessLog ( ready, remainingDeps ) =
    case ready of
        todo :: otherReady ->
            doSteps
                (todo :: proccessLog)
                (clearDep todo ( otherReady, remainingDeps ))

        [] ->
            List.reverse proccessLog


clearDep :
    StepId
    -> ( List StepId, Dict StepId (Set StepId) )
    -> ( List StepId, Dict StepId (Set StepId) )
clearDep finishedDep pendingSteps =
    Dict.foldl
        (\step stepDeps ( ready, deps ) ->
            let
                newStepDeps =
                    Set.remove finishedDep stepDeps
            in
            if Set.isEmpty newStepDeps then
                ( step :: ready, Dict.remove step deps )

            else
                ( ready, Dict.insert step newStepDeps deps )
        )
        pendingSteps
        (Tuple.second pendingSteps)
        |> Tuple.mapFirst List.sort



-- Section: Part Two


type alias WorkerState =
    { step : StepId
    , remainingTime : Int
    }


type alias ProjectState =
    { time : Int
    , completedSteps : List StepId
    , availableSteps : List StepId
    , unavailableSteps : Dict StepId (Set StepId)
    , workers : List WorkerState
    }


part2 : Dict StepId (Set StepId) -> Int
part2 deps =
    let
        ( availableSteps, unavailableSteps ) =
            clearDep 0 ( [], deps )

        startState =
            { time = -1
            , completedSteps = []
            , availableSteps = availableSteps
            , unavailableSteps = unavailableSteps
            , workers = []
            }

        endState =
            doProject startState
    in
    endState.time


doProject : ProjectState -> ProjectState
doProject projectState =
    let
        updatedProject =
            advance projectState
    in
    if List.length updatedProject.completedSteps == 26 then
        updatedProject

    else
        doProject updatedProject


advance : ProjectState -> ProjectState
advance project =
    let
        ( justCompleted, stillWorking ) =
            project.workers
                |> List.map advanceWorker
                |> List.partition (.remainingTime >> (==) 0)
                |> Tuple.mapFirst (List.map .step)

        ( preAvailableSteps, unavailableSteps ) =
            clearCompleted
                justCompleted
                project.availableSteps
                project.unavailableSteps

        availableWorkers =
            5 - List.length stillWorking

        workerState =
            stillWorking
                ++ (List.take availableWorkers preAvailableSteps
                        |> List.map startWork
                   )

        availableSteps =
            List.drop availableWorkers preAvailableSteps
    in
    { time = project.time + 1
    , completedSteps = project.completedSteps ++ List.sort justCompleted
    , availableSteps = availableSteps
    , unavailableSteps = unavailableSteps
    , workers = workerState
    }


startWork : StepId -> WorkerState
startWork stepId =
    { step = stepId
    , remainingTime = stepId - 4
    }


clearCompleted :
    List StepId
    -> List StepId
    -> Dict StepId (Set StepId)
    -> ( List StepId, Dict StepId (Set StepId) )
clearCompleted completedList availableSteps unavailableSteps =
    let
        completed =
            Set.fromList completedList
    in
    Dict.foldl
        (\step stepDeps ( ready, deps ) ->
            let
                newStepDeps =
                    Set.diff stepDeps completed
            in
            if Set.isEmpty newStepDeps then
                ( step :: ready, Dict.remove step deps )

            else
                ( ready, Dict.insert step newStepDeps deps )
        )
        ( availableSteps, unavailableSteps )
        unavailableSteps
        |> Tuple.mapFirst List.sort


advanceWorker : WorkerState -> WorkerState
advanceWorker worker =
    { worker | remainingTime = worker.remainingTime - 1 }
