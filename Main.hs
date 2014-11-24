module Main where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, ServerPart)

main :: IO ()
main = do
     putStrLn "Starting Server..."
     simpleHTTP nullConf $ site
     
site :: ServerPart String
site = ok "Sup, World!"
