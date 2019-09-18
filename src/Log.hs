module Log where

import Data.Time.Clock
import System.IO

reset :: IO()
reset = do
  writeFile "./app.log" ""

log :: String -> IO()
log x = do
  now <- getCurrentTime
  appendFile "./app.log" $ "[" ++ show now ++ "] " ++ x ++ "\n"
  