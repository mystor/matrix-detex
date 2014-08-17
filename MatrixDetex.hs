import           Data.List
import           Data.Text                     hiding (intercalate, map)
import           System.Hclip
import           Text.ParserCombinators.Parsec

-- | Strip, with Strings instead of Text for arguments
trim :: String -> String
trim = unpack . strip . pack

-- | A single cell of a matrix
body :: Parser String
body = many1 $ noneOf "&\\"

-- | A single row of the matrix
row :: Parser [String]
row = sepBy body (char '&')

-- | A matrix parser (excluding wrappers)
matrix :: Parser [[String]]
matrix = sepBy row (try (string "\\\\"))

-- | A wrapped matrix parser
wrappedMatrix :: Parser [[String]]
wrappedMatrix = do
    skipMany space -- Skip any leading whitespace
    optional (try $ string "\\begin{bmatrix}")
    mat <- matrix
    optional (try $ string "\\end{bmatrix}")
    return mat

-- | Trim every element of the matrix
cleanUp :: [[String]] -> [[String]]
cleanUp (x : xs) = map trim x : cleanUp xs
cleanUp []       = []

-- | Generate a wolfram array from an array of arrays of strings
wolfram :: [[String]] -> String
wolfram x = "{" ++ wolfram' ++ "}"
    where
        wolfram' = intercalate ",\n " (map row x)
        row y = "{" ++ row' y ++ "}"
        row' y = intercalate ", " y

main :: IO ()
main = do
     string <- getClipboard

     putStrLn $ "Got input: \n" ++ string ++ "\n"

     let result = do
                result <- parse wrappedMatrix "matrix" string
                return $ wolfram $ cleanUp result

     case result of
          Left e -> do
               putStrLn "Failed to parse input:"
               putStrLn $ show e
          Right s -> do
               setClipboard s
               putStrLn "Success! Copied result to clipboard:"
               putStrLn s
