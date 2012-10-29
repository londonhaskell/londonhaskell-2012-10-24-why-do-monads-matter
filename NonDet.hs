module Uncertainty where

-- P (PowerSet) Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/

-- TODO
-- Blackjack problem taken from Haskell
-- Hoodlums meetup on 17-Oct-2012.
-- http://www.meetup.com/

-- Non-Determinism comes from rule in Blackjack
-- that allows an Ace to be valued either as 1 
-- or 11.

data Card = Ace
          | Number Integer
          | Picture

valueCard :: Card -> P Integer
valueCard Ace = [1,11] -- Aces can be either 1 or 11
valueCard (Number x) = [x]
valueCard Picture = [10]

-- Hands are just lists of cards
valueHand :: [Card] -> P Integer
valueHand [] = [0]
valueHand (c:cs) = (valueCard c) `addP` (valueHand cs)

-- Print the value of some test hands
main = do
    print (valueHand []) -- 0
    print (valueHand [Ace, Number 2, Number 3]) -- 6 or 16
    print (valueHand [Ace, Ace, Number 2]) -- 4, 14 or 24
    print (valueHand [Ace, Ace, Ace]) -- 3, 13, 23 or 33
    print (valueHand [Picture, Picture]) -- 20
    print (valueHand [Ace, Picture]) -- 11 or 21
    print (valueHand [Ace, Picture, Picture]) -- 21 or 31
    print (valueHand [Ace, Picture, Picture, Picture]) -- 31 or 41

-- Type synonym for Power-Sets
-- A non-deterministic choice will be represented by a list
type P a = [a]

-- Kleisli composition
-- Apply f to every possible result from applying g to x and merge the results
composeP :: (b -> P c) -> (a -> P b) -> (a -> P c)
composeP f g x = h ( g x ) -- Redefine so not using list comprehension
              where
                     h [] = []
                     h (y:ys) = (f y) ++ (h ys)

-- Kleisli identity
idP :: a -> P a
idP x = [x] -- A choice of one

-- Add two non-deterministic integers and return every possible combination
addP :: P Integer -> P Integer -> P Integer
addP xs ys = (f xs `composeP` f ys) 0
    where f n i = map (+i) n

-- Define join using composeP
joinP :: P (P a) -> P a
joinP = composeP id id
