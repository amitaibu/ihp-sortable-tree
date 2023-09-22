module Web.Controller.Tasks where

import Web.Controller.Prelude
import Web.View.Tasks.Index
import Web.View.Tasks.Form

-- Decode data coming from frontend.
import qualified Data.Aeson (Value, toJSON, (.=))

-- Allow encoding HTML so we can handover to the frontend as JSON.
import qualified Text.Blaze.Html.Renderer.Text as Renderer
import Text.Blaze.Html (Html)
import qualified Data.Text.Lazy as TL

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

getIndexView :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IO IndexView
getIndexView = do
    tasks <- query @Task |> fetch
    let newTask = newRecord @Task
    let treeJson = tasksToTreeJson tasks
    pure IndexView { .. }

tasksToTreeJson :: (?context :: ControllerContext) => [Task] -> Value
tasksToTreeJson tasks = toJSON pseudoRoot
    where
        newTask = newRecord @Task

        -- We use a pseudo root to have a single tree, instead of a forsest of trees.
        -- This makes it easier to work with the tree in the frontend.
        pseudoRoot = object
            [ "data" .= object
                [ "uuid" .= newTask.id
                , "body" .= ("Tasks" :: Text)
                ]
            , "nodes" .= map toItemTree rootItems
            ]

        -- Indicate the task has no parent.
        isRoot item = isNothing item.taskId
        rootItems = filter isRoot tasks |> sortOn (.weight)

        toItemTree :: Task -> Value
        toItemTree task = object
            [ "data" .= object
                [ "uuid" .= task.id
                , "body" .= Web.View.Tasks.Form.renderForm task children
                ]
            , "nodes" .= map toItemTree children
            ]
            where children = childrenOf task tasks

        childrenOf :: Task -> [Task] -> [Task]
        childrenOf parent tasks = filter
            (\child ->
                let (Id parentId) = parent.id
                in child.taskId == Just parentId
            ) tasks |> sortOn (.weight)

instance ToJSON Html where
    toJSON html = String (TL.toStrict $ Renderer.renderHtml html)