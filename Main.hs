module Main where

import Control.Monad (guard, msum, void)
import Control.Monad.Trans (liftIO)
import Data.List.Split (splitOn)
import Happstack.Server (nullConf, simpleHTTP, toResponse,
                         ok, ServerPart, withHost, serveDirectory,
                         Browsing(DisableBrowsing), Response)

main :: IO ()
main = do
     putStrLn "Starting Server..."
     simpleHTTP nullConf site

site :: ServerPart Response
site = msum [subsite "timer" timer,
             subsite "box" $ ok $ toResponse "Hello, Box!",
             ok $ toResponse "Sup, World!"]

timer :: ServerPart Response
timer = serveDirectory DisableBrowsing ["index.html"] "./pulse/www"

subsite :: String -> ServerPart a -> ServerPart a
subsite subdomain handler = withHost conditionally
  where conditionally host = let parts = splitOn "." host
                                 visitableDomain = length parts == 3
                                 thisDomain = head parts == subdomain
                                 shouldServe = visitableDomain && thisDomain
                             in guard shouldServe >> handler
