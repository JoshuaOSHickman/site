module Main where

import Control.Monad (guard, msum, void)
import Control.Monad.Trans  (liftIO)
import Data.List.Split (splitOn)
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, ServerPart, withHost)

main :: IO ()
main = do
     putStrLn "Starting Server..."
     simpleHTTP nullConf site

site :: ServerPart String
site = msum [subsite "fish" $ ok "Hello, Fish!",
             subsite "box" $ ok "Hello, Box!",
             ok "Sup, World!"]

subsite :: String -> ServerPart a -> ServerPart a
subsite subdomain handler = withHost conditionally
  where conditionally host = let parts = splitOn "." host
                                 visitableDomain = length parts == 3
                                 thisDomain = head parts == subdomain
                                 shouldServe = visitableDomain && thisDomain
                             in guard shouldServe >> handler
