module Web.Controller.Tasks where

import Web.Controller.Prelude
import Web.View.Tasks.Index
import Web.View.Tasks.Form

-- Decode data coming from frontend.
import Data.Aeson.Types (Parser)
import qualified Data.Aeson (Value, toJSON, (.=))
import Network.Wai (Request, getRequestBodyChunk)

-- Allow encoding HTML so we can handover to the frontend as JSON.
import qualified Text.Blaze.Html.Renderer.Text as Renderer
import Text.Blaze.Html (Html)
import qualified Data.Text.Lazy as TL

-- For creating the Tree structure.
import Data.Tree (Tree(..))
import qualified Data.Tree as Tree
import Data.Tree.Zipper



instance Controller TasksController where

    action UpdateSortTasksAction = do
            decodedTree <- decodeRequestTree request
            case decodedTree of
                Right uuidTree -> do
                    tasks <- query @Task |> fetch

                    -- Go over the tree and update the weights and the reference to the parent,
                    -- if there is one.
                    -- If the level is 0, then we should not reference any parent.
                    -- They way we should do it, is by going over the tree with a zipper
                    -- so we can know the level and weight of each element, and the
                    updateFromZipper tasks zipper Nothing 0
                    redirectTo TasksAction
                    where
                        actualTree = unUUIDTree uuidTree
                        zipper = fromTree actualTree
                Left err ->
                    error $ show err

    action TasksAction = do
        indexView <- getIndexView
        render indexView

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
        tasksCount <- query @Task
            -- Find root tasks (tasks without a parent).
            |> filterWhere (#taskId, Nothing)
            |> fetchCount

        let task = newRecord @Task |> set #weight tasksCount

        task
            |> buildTask
            |> ifValid \case
                Left task -> do
                    indexView <- getIndexView
                    -- Pass the erroring task to the view, so we can render the errors.
                    render $ indexView {newTask = task}
                Right task -> do
                    task <- task |> createRecord
                    redirectTo TasksAction

    action DeleteTaskAction { taskId } = do
        task <- fetch taskId
        deleteRecord task
        redirectTo TasksAction

buildTask task = task
    |> fill @'["body"]
    |> validateField #body nonEmpty

getIndexView :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IO IndexView
getIndexView = do
    tasks <- query @Task |> fetch
    let newTask = newRecord @Task
    let treeJson = tasksToTreeJson tasks
    pure IndexView { .. }

tasksToTreeJson :: (?context :: ControllerContext) => [Task] -> Value
tasksToTreeJson tasks = toJSON [pseudoRoot]
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


-- Create a newtype to be able to decode the tree from the frontend.
newtype UUIDTree = UUIDTree { unUUIDTree :: Tree UUID }

instance FromJSON UUIDTree where
    parseJSON = withObject "tree" $ \v -> do
        treesArray <- v .: "tree"
        case treesArray of
            [treeObj] -> parseTree treeObj
            _ -> fail "Unexpected number of trees, there should only a single one"

parseTree :: Object -> Parser UUIDTree
parseTree obj = do
    elementObj <- obj .: "element"
    dataObj <- elementObj .: "_data"
    root <- dataObj .: "uuid"
    forestUUIDTrees <- obj .: "subnodes" >>= mapM parseTree
    let forest = map (\(UUIDTree t) -> t) forestUUIDTrees
    return $ UUIDTree (Node root forest)


decodeRequestTree :: (?context::ControllerContext) => Request -> IO (Either String UUIDTree)
decodeRequestTree req = do
    requestBody <- request |> getRequestBodyChunk
    return $ eitherDecode (cs requestBody)


updateFromZipper :: (?modelContext :: ModelContext) => [Task] -> TreePos Full UUID -> Maybe UUID -> Int -> IO ()
updateFromZipper tasks zipper parentId weight = do
    -- The currently focused task's UUID.
    let uuid = label zipper
    updateItem tasks uuid parentId weight

    -- Update children
    let childrenZipper = firstChild zipper
    case childrenZipper of
        -- start numbering children from 1
        Just childZipper -> go childZipper 1
        Nothing -> return ()
    where
        go childZipper childWeight = do
            updateFromZipper tasks childZipper (Just $ label zipper) childWeight
            let nextChildrenZipper = next childZipper
            case nextChildrenZipper of
                Just nextChildZipper -> go nextChildZipper (childWeight + 1)
                Nothing  -> return ()


updateItem :: (?modelContext :: ModelContext) =>[Task] -> UUID -> Maybe UUID -> Int -> IO ()
updateItem tasks uuid mParentId weight =
    let matchingTasks = filter (\item -> item.id == Id uuid) tasks
    in case matchingTasks of
        (task:_) -> do
            task
                |> set #weight weight
                |> set #taskId parentId
                |> updateRecord
            pure ()
            where
                newTask = newRecord @Task
                (Id newTaskUuid) = newTask.id
                parentId = case mParentId of
                    Just parentId -> if newTaskUuid == parentId
                        -- Don't reference the pseudo root.
                        then Nothing
                        else Just parentId
                    Nothing -> Nothing

        _ -> pure ()