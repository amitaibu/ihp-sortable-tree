module Web.View.Tasks.Form where

import Web.View.Prelude

renderForm :: Task -> [Task] ->  Html
renderForm task children = formFor task [hsx|
        {(hiddenField #taskId)}
        <div class="flex flex-col gap-4">
            {(textareaField #body) {fieldLabel = ""}}
            {submitButton {label = "Save"}}
        </div>
    |]

