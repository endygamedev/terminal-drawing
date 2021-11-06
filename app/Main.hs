module Main where

import System.Console.Terminal.Size
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import System.Console.Pretty


type ColorString = [(Color, [Char])]


timeSleep :: Int
timeSleep = 400000


charClear :: String
charClear = "\x1b[H"


{-# NOINLINE imgSize #-}
imgSize :: Int
imgSize = fromIntegral (unsafePerformIO $ fmap (\(Just x) -> height x) size) :: Int


output :: Int -> ColorString 
output n
  | n `mod` 6 == 0 = charOutput Red '0'
  | n `mod` 6 == 1 = charOutput Yellow '1'
  | n `mod` 6 == 2 = charOutput Green '2'
  | n `mod` 6 == 3 = charOutput Cyan '3'
  | n `mod` 6 == 4 = charOutput Blue '4'
  | otherwise = charOutput Magenta '5'
  where
    height = imgSize `div` 2
    width = 2 * height 
    charOutput :: Color -> Char -> [(Color, [Char])]
    charOutput color char = replicate height (color, replicate width char)


render :: Int -> IO ()
render n = do
  putStr charClear
  mapM_ (\(c,s) -> putStrLn (color c s)) (output n)
  threadDelay timeSleep
  render (n + 1)


main :: IO ()
main = do
  putStr charClear 
  render 0
