module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Map as M
import System.FilePath
import System.Directory

type Action = StateT (M.Map String FilePath) IO

runAction :: Action a -> M.Map String FilePath -> IO a
runAction = evalStateT

diff :: FilePath -> FilePath -> IO ()
diff path path' = do
    x <- B.readFile path
    y <- B.readFile path'
    if x == y then do
        putStrLn "Equally named files"
        putStrLn $ "    " ++ path
        putStrLn $ "    " ++ path'
        putStrLn "-> are equal"
        putStrLn ""
    else do
        putStrLn "Equally named files"
        putStrLn $ "    " ++ path
        putStrLn $ "    " ++ path'
        putStrLn "-> are DIFFERENT"
        putStrLn ""

process :: FilePath -> Action ()
process path = do
    dir <- liftIO $ doesDirectoryExist path
    if dir then do
        liftIO (listDirectory path) >>= mapM_ (process . (path</>))
    else do
        let filename = takeFileName path

        map <- get
        case M.lookup filename map of
            Nothing -> put $ M.insert filename path map
            Just path' -> liftIO $ diff path path'

main :: IO ()
main = runAction (process ".") M.empty
