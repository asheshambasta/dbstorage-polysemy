module Database.Lib.Redact
  ( redactPasswordEquals
  )
where

import           Data.Text.ICU.Replace          ( replaceAll )

redactPasswordEquals :: Text -> Text
redactPasswordEquals =
  replaceAll "password[\\s]*=[\\s]*.[^\\s]*" "password=REDACTED"

