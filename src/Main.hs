
-- HsTeTAc, a haskell implementation triplet-triplet energy-transfer activate complex coordinate algorithm
-- Angel Alvarez, Felipe Zapata 
-- @ 2013 The Resmol Group


module Main where

import System.Environment
import System.IO
import Gaussian

-- For a complete description of this method please refer to:
-- Luis Manuel Frutos and Obis Castaño "A new algorithm for predicting triplet-triplet energy-transfer activated
-- complex coordinate in terms of accurate potential-energy surfaces" THE JOURNAL OF CHEMICAL PHYSICS 123, 15 Sep 2005


main :: IO ()
main = do
    args <- getArgs
    case args of
         [file] -> do
             result <- parseGaussianCheckpoint file
             case result of
                  Left err  -> print err
                  Right xs  -> mapM_ (putStrLn.show) xs
         _ -> do
             progName <- getProgName
             hPutStrLn stderr $ "Usage: " ++ " filename"

