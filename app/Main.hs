module Main where

import System.Console.Terminal.Size
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent


timeSleep :: Int
timeSleep = 1000000


{-# NOINLINE imgSize #-}
imgSize :: Int
imgSize = fromIntegral (unsafePerformIO $ fmap (\(Just x) -> height x) size) :: Int


output :: Int -> [[Char]]
output n
  | n `mod` 3 == 0 = charOutput '0'
  | n `mod` 3 == 1 = charOutput '1'
  | n `mod` 3 == 2 = charOutput '2'
  where
    height = imgSize `div` 2
    width = 2 * height 
    charOutput :: Char -> [[Char]]
    charOutput c = replicate height $ replicate width c


render :: Int -> IO ()
render n = do
  putStr "\x1b[H"
  mapM_ putStrLn (output n)
  threadDelay timeSleep
  render (n + 1)


main :: IO ()
main = do
  putStr "\x1b[H"
  render 0 
