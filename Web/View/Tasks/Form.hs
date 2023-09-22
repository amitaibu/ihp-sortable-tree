module Web.View.Tasks.Form where

import Web.View.Prelude
import Text.Blaze.Html4.FrameSet.Attributes (href)

renderForm :: Task -> [Task] ->  Html
renderForm task children = formFor task [hsx|
        {(hiddenField #taskId)}
        <div class="flex flex-col gap-4">
            {(textareaField #body) {fieldLabel = ""}}

            <div class="flex flex-row justify-between items-end">
                {submitButton {label = "Save"}}
                {deleteLink}
            </div>
        </div>
    |]
        where deleteLink
                -- Show delete link only if there are no children.
                | isEmpty children = [hsx|<a href={DeleteTaskAction task.id} class="js-delete text-sm hover:underline">Delete</a>|]
                | otherwise = ""


