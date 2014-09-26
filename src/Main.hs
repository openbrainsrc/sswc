import System.Environment
import Text.SSWC
import qualified Data.Text.IO as T

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] =   putStrLn "usage: sswc filename"
dispatch (fnm:_) = do
  edoc <- loadDocument fnm
  case edoc of 
    Right doc -> do let ts = getTemplates doc
                        newDoc = runTemplates ts doc
                    print ts
                    T.putStrLn $ renderDocument newDoc
    Left err -> putStrLn $ "error: "++err
