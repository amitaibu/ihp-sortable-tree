module Web.Controller.Tasks where

import Web.Controller.Prelude
import Web.View.Tasks.Index

instance Controller TasksController where
    action TasksAction = do
        tasks <- query @Task |> fetch
        let newTask = newRecord @Task
        render IndexView { .. }

    action UpdateTaskAction { taskId } = do
        task <- fetch taskId
        task
            |> buildTask
            |> ifValid \case
                Left task -> do
                    indexView <- getIndexView
                    render indexView
                Right task -> do
                    task <- task |> updateRecord
                    redirectTo TasksAction

    action CreateTaskAction = do
        let task = newRecord @Task
        task
            |> buildTask
            |> ifValid \case
                Left task -> do
                    indexView <- getIndexView
                    render indexView
                Right task -> do
                    task <- task |> updateRecord
                    redirectTo TasksAction

    action DeleteTaskAction { taskId } = do
        task <- fetch taskId
        deleteRecord task
        setSuccessMessage "Task deleted"
        redirectTo TasksAction

buildTask task = task
    |> fill @'["body"]

getIndexView :: (?modelContext :: ModelContext) => IO IndexView
getIndexView = do
    tasks <- query @Task |> fetch
    let newTask = newRecord @Task
    pure IndexView { .. }