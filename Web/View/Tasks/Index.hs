module Web.View.Tasks.Index where
import Web.View.Prelude
import Web.View.Tasks.Form (renderForm)
import Data.Aeson (encode, Value)

data IndexView = IndexView
    { tasks :: [Task]
    , newTask :: Task
    , treeJson :: Data.Aeson.Value
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div data-tree={encode treeJson} id="treeContainer"></div>
        <div class="flex flex-col gap-4">
            <h2>Add New Task</h2>
            <div>{renderForm newTask []}</div>
        </div>
    |]

renderTask :: Task -> Html
renderTask task = [hsx|
    <tr>
        <td>{task}</td>
        <td><a href={DeleteTaskAction task.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]