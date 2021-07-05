module Question3 where

-- Name:Bora Oner
-- 150170301


data Ternary a = Empty | Single a | Multiple [a]
                 deriving Show

instance Functor Ternary where
    fmap f Empty = Empty
    fmap f (Single x) = Single(f x)
    fmap f (Multiple(xss)) = case xss of
       [] -> Empty
       [x]-> Single(f x)
       _  -> Multiple(map f xss)
