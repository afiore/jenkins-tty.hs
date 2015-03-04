module Jenkins.Render
 ( Render(..)
 , joinTxt
 , padL
 , padR
 ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import Data.Monoid

class Show a => Render a where
  renderTTY :: a -> T.Text
  renderTTY = render

  render :: a -> T.Text
  render = T.pack . show

  prettyPrint :: Show a => a -> IO ()
  prettyPrint v = do
    isTTY  <- hIsTerminalDevice stdout
    let txt = if isTTY then renderTTY v else render v
    T.hPutStrLn stdout txt

delimiter :: T.Text
delimiter = "|"

joinTxt :: [T.Text] -> T.Text
joinTxt = T.intercalate delimiter

data PadDir = PadLeft | PadRight

padL :: Int -> T.Text -> T.Text
padL = pad PadLeft ' '

padR :: Int -> T.Text -> T.Text
padR = pad PadRight ' '

pad :: PadDir -> Char -> Int -> T.Text -> T.Text
pad dir c maxL t =
   if l < maxL then pad' else T.take maxL t
  where
    l       = T.length t
    c'      = T.singleton c
    padding = T.replicate (maxL - l) c'
    pad'    = case dir of
                PadLeft  -> padding <> t
                PadRight -> t <> padding
