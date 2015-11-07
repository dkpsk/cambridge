module Main where
import System.Environment(getArgs)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Control.Monad.Random
import System.Random.Shuffle(shuffleM)

main :: IO ()
main = getArgs >>= f >>= p where
  f :: [String] -> IO[T.Text]
  f ws = sequence $ map cambridge ws
  p :: [T.Text] -> IO()
  p ws = mapM_ (\x -> TIO.putStr x >> putChar ' ') ws

-- ケブンリッジ化.
cambridge :: (MonadRandom m) => String -> m T.Text
cambridge word = cambridge' word $ length word

cambridge' :: (MonadRandom m) => String -> Int -> m T.Text
cambridge' word l | l > 3 = shuffle word
                  | otherwise = return $ T.pack word

-- 単語の最初の文字と最後の文字以外をとって返す.
intermediateOf :: String -> String
intermediateOf text = let l = length text in drop 1 $ take (l-1) text

-- 最初と最後以外の文字をシャッフルした文字列を返す.
shuffle :: MonadRandom m => String -> m T.Text
shuffle word = do
  body <-  shuffleM $ intermediateOf word
  return $ (h `T.cons` (T.pack body)) `T.snoc` l
  where w = T.pack word
        h = T.head w
        l = T.last w
