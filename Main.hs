{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as L
import Web.Scotty

asyncPrint :: MVar String -> IO ()
asyncPrint x = forever $ getLine >>= swapMVar x >> return ()

server :: MVar String -> IO ()
server x = do
  scotty 3000 $
    get "/" $ do
      val <- liftIO $ readMVar x
      let message = mconcat ["<h1>Scotty, ", (L.pack val), " me up!</h1>"]
      (html message)

main :: IO ()
main = newMVar "Boy" >>= \x -> concurrently_ (asyncPrint x) (server x)
