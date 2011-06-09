module Main where
--
--
import System.Environment
import InvestWebParser
--
--
main = do
   (uri : args) <- getArgs
   getFirstFile uri
--
--
