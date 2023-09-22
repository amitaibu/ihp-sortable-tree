module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Tasks

instance FrontController WebApplication where
    controllers =
        [ startPage TasksAction
        -- Generator Marker
        , parseRoute @TasksController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
