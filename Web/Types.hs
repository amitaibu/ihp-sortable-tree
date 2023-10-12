module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data TasksController
    = TasksAction
    -- We use `Update` prefix, so it will get a POST request.
    | UpdateSortTasksAction
    | CreateTaskAction
    | UpdateTaskAction { taskId :: !(Id Task) }
    | DeleteTaskAction { taskId :: !(Id Task) }
    deriving (Eq, Show, Data)
