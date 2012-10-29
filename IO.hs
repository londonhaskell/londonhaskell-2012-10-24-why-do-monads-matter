module Destruction where

-- IOAction Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/

-- Going to jump straight to the Kleisli arrow version so no code yet!

-- Define a couple of functions that interact with the outside world
waitForInput :: String -> IOAction String
waitForInput s = Output s (Wait (\t -> Return t))

outputReverse :: String -> IOAction ()
outputReverse s = Output (reverse s) (Return ())

-- Define a main using these functions
-- Using composition we want something like:
-- main = ( outputReverse . waitForInput "Enter some test:")
main = runIO ((outputReverse `composeIO` waitForInput) "Enter some test:")

-- Each IOAction describes one type of action and the action
-- to do next
data IOAction a = Output String (IOAction a) -- Write the string and then the next action
                | Wait (String -> IOAction a) -- Get a string and pass it to the function that returns the next action
                | Return a

runIO :: IOAction a -> IO a
runIO (Output s n) = do { putStrLn s ; runIO n }
runIO (Wait f) = do { s <- getLine ; runIO (f s) }
runIO (Return x) = do { return x }

-- Kleisli composition
composeIO :: (b -> IOAction c) -> (a -> IOAction b) -> (a -> IOAction c)
composeIO f g x = case (g x) of
                   Output s n -> Output s (composeIO f (\_ -> n) ())
                   Wait h -> Wait ( \s -> composeIO f h s )
                   Return r -> f r

-- Kleisli identity
idIO :: a -> IOAction a
idIO x = Return x
