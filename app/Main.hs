-- Main.hs for the Regex module (executable) to run on an interesting example.
-- See @src/DSL.hs@ for more examples.

module Main where

import RDSL
import Regex (matchWithTrace)

main :: IO ()
main = do
    let reg          = csv ','
        input        = "a,b,c\ne,f,g\nh,i,jfsdfds\n fd, d,,s"
        (matched,tr) = matchWithTrace reg input

    putStrLn $ if matched then "Input accepted." else "Input rejected!"
