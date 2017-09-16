module AnsiiOutput where

import System.Console.ANSI

import System.IO

import Control.Concurrent
import Control.Monad
import DataTypes

outputAsciiImagei :: AsciiImage -> Int -> IO()
outputAsciiImagei image count
   | count>=(length image) = putStrLn ""
   | otherwise = do
        putStrLn (image!!count)
        outputAsciiImagei image (count+1)

outputAsciiImage :: AsciiImage -> IO()
outputAsciiImage image = do
    outputAsciiImagei image 0
