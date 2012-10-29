module Destruction where

-- IOAction Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/

-- Going to jump straight to the Kleisli arrow version so no code yet!

-- Define a couple of functions that interact with the outside world
waitForInput :: String -> IO String
waitForInput s = do { putStrLn s ; getLine }

outputReverse :: String -> IO ()
outputReverse s = putStrLn (reverse s)

-- Define a main using these functions
-- Using composition we want something like:
-- main = ( outputReverse . waitForInput "Enter some test:")
main = do
           s <- waitForInput "Enter some test:"
           outputReverse s
