import           Control.Monad
import           Data.List
import           Data.Text                          hiding (intercalate, map)
import           System.Environment
import           System.Hclip
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Prim

body :: Parser String
body = many1 $ noneOf "&\\"

row :: Parser [String]
row = sepBy body (char '&')

matrix :: Parser [[String]]
matrix = sepBy row (try (string "\\\\"))

full_matrix :: Parser [[String]]
full_matrix = do
  string "\\begin{bmatrix}"
  mat <- matrix
  string "\\end{bmatrix}"
  return mat

doParse :: String -> Either ParseError [[String]]
doParse input = parse full_matrix "matrix" input

trim :: String -> String
trim = unpack . strip . pack

cleanUp :: [[String]] -> [[String]]
cleanUp (x : xs) = map trim x : cleanUp xs
cleanUp []       = []

wolfram :: [[String]] -> String
wolfram x = "{" ++ wolfram' ++ "}"
    where
        wolfram' = intercalate ",\n " (map row x)
        row y = "{" ++ row' y ++ "}"
        row' y = intercalate ", " y

transformLeft :: (a -> b) -> Either a c -> Either b c
transformLeft fn e = either lhs rhs e
    where
        lhs a = Left $ fn a
        rhs c = Right c

main :: IO ()
main = do
     string <- getClipboard
     putStrLn "Got input:"
     putStrLn $ string ++ "\n"
     let result = do
                result <- transformLeft show $ doParse string
                return $ wolfram $ cleanUp result

     case result of
          Left e -> do
               putStrLn "Failure! Reason:"
               putStrLn e
          Right s -> do
                void $ setClipboard $ s
                putStrLn "Success! Copied result to clipboard:"
                putStrLn s
