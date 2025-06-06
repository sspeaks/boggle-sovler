{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Aeson               (ToJSON, encode)
import qualified Data.Array               as A
import           Data.Char                (toLower)
import           Data.List                (nub, sortBy)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Ord                 (Down (..), comparing)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           GHC.Generics             (Generic)
import           Network.HTTP.Types       (hContentType, status200, status400,
                                           status500)
import           Network.Wai              (Application, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (getExecutablePath)
import           System.FilePath          (takeDirectory, (</>))
import           Text.Printf              (printf)

-- Define the Trie data structure
data Trie = Trie
  { endOfWord :: Bool,
    children  :: Map Char Trie
  }
  deriving (Show)

type GameBoard = A.Array (Int, Int) Char

gameBoard :: T.Text -> GameBoard
gameBoard exampleBoard = A.array ((0, 0), (squareRoot - 1, squareRoot - 1)) $ zip [(i, j) | i <- [0 .. squareRoot - 1], j <- [0 .. squareRoot - 1]] (map toLower stringBoard)
  where
    stringBoard :: String
    stringBoard = T.unpack exampleBoard
    squareRoot :: Int
    squareRoot = (round :: Double -> Int) . sqrt . fromIntegral . T.length $ exampleBoard

permuteBoard :: GameBoard -> Trie -> [T.Text]
permuteBoard b t = concatMap (permuteFrom b t S.empty T.empty) startingPositions
  where
    permuteFrom :: GameBoard -> Trie -> S.Set (Int, Int) -> T.Text -> (Int, Int) -> [T.Text]
    permuteFrom board trie visited prefix (i, j) =
      case Map.lookup c (children trie) of
        Just child ->
          let newPrefix = T.snoc prefix c
              newVisited = S.insert (i, j) visited
              results = concatMap (permuteFrom board child newVisited newPrefix) neighbours
           in if endOfWord child
                then newPrefix : results
                else results
        Nothing -> []
      where
        c = board A.! (i, j)
        neighbours = filter (`S.notMember` visited) . filter inRange . filter (\(v1, v2) -> not (v1 == i && v2 == j)) $ [(i + x, j + y) | x <- [-1 .. 1], y <- [-1 .. 1]]
        inRange :: (Int, Int) -> Bool
        inRange (i', j') =
          let ((mini, minj), (maxi, maxj)) = bounds b
           in i' >= mini && i' <= maxi && j' >= minj && j' <= maxj
        bounds :: GameBoard -> ((Int, Int), (Int, Int))
        bounds = A.bounds
    startingPositions = A.indices b

-- Create an empty Trie
emptyTrie :: Trie
emptyTrie = Trie False Map.empty

-- Insert a word into the Trie
insert :: T.Text -> Trie -> Trie
insert word trie
  | T.null word = trie {endOfWord = True}
  | otherwise = case Map.lookup c (children trie) of
      Just child -> trie {children = Map.insert c (insert (T.tail word) child) (children trie)}
      Nothing -> trie {children = Map.insert c (insert (T.tail word) emptyTrie) (children trie)}
  where
    c = T.head word

-- Insert multiple words into the Trie
insertWords :: [T.Text] -> Trie -> Trie
insertWords wds trie = foldr insert trie wds

-- Check if a word is in the Trie
search :: T.Text -> Trie -> Bool
search word trie = maybe False endOfWord (T.foldl' searchChar (Just trie) word)
  where
    searchChar (Just (Trie _ childs)) c = Map.lookup c childs
    searchChar Nothing _                = Nothing

main :: IO ()
main = do
  exePath <- getExecutablePath
  let dictPath = takeDirectory exePath </> "updated_dictionary_keys.txt"
  dict <- TIO.readFile dictPath
  let wds = T.lines dict
  let trie = insertWords wds emptyTrie
  putStrLn "Running boggle server at localhost:8081"
  run 8081 $ application trie

data ResponseStruct = Resp
  { value :: Maybe [T.Text],
    error :: Maybe T.Text
  }
  deriving (Generic, ToJSON)

isSquare :: Int -> Bool
isSquare n =
  let intArg = fromIntegral n :: Double
      roundedSqrt = (round . sqrt $ intArg) :: Int
      s = fromIntegral roundedSqrt :: Double
   in s == (intArg / s)

-- A warp application that takes a 16 character string consisting of only a-z. Throws an error if the string is not 16 characters long or characters aren't correct.
--
application :: Trie -> Application
application trie req res = do
  let pathInfoList = pathInfo req
  case pathInfoList of
    [] -> do
      liftIO $ printf "Empty board passed.\n"
      res $
        responseLBS
          status500
          [(hContentType, "application/json")]
          (encode $ Resp Nothing (Just "Empty board passed. Must be a square number (e.g. 16 chars) of characters and a-z"))

    (board:_)
      | T.length board > 100 -> do
      liftIO $ printf "Board too long: %s\n" (T.unpack board)
      res $
        responseLBS
          status400
          [(hContentType, "application/json")]
          (encode $ Resp Nothing (Just "Board too long. Must be 100 characters (i.e. 10x10) or fewer."))

      | (not . isSquare) (T.length board) || not (T.all (`elem` ['a' .. 'z']) board) -> do
      liftIO $ printf "Invalid board: %s\n" (T.unpack board)
      res $
        responseLBS
          status400
          [(hContentType, "application/json")]
          (encode $ Resp Nothing (Just "Invalid board. Must be a square number (e.g. 16 chars) of characters and a-z"))

    (board:_) -> do
          liftIO $ printf "Received board: %s\n" (T.unpack board)
          let solved = permuteBoard (gameBoard board) trie
          let uniqueSolved = nub solved
          let sortedSolved = sortBy (comparing (Down . T.length) <> comparing T.toLower) uniqueSolved
          let jsonData = Resp (Just sortedSolved) Nothing
          res $
            responseLBS
              status200
              [(hContentType, "application/json")]
              (encode jsonData)
