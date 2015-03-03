module Jenkins.Render
 ( Render(..)
 , joinTxt
 ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

delimiter :: T.Text
delimiter = "|"

joinTxt :: [T.Text] -> T.Text
joinTxt = T.intercalate delimiter

class Show a => Render a where
  renderTTY :: a -> T.Text
  renderTTY = render

  render :: a -> T.Text
  render = T.pack . show

  prettyPrint :: Show a => a -> IO ()
  prettyPrint = T.putStrLn . render
