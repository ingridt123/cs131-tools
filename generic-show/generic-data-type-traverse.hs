{-# LANGUAGE DeriveDataTypeable #-}

-- this file inspired by this article:
-- https://chrisdone.com/posts/data-typeable/#fnref1


import Data.Typeable
import Data.Data ( Data(gmapQ, toConstr), showConstr ) 
import Data.Generics.Aliases ( extQ )
-- note: had to run on command line
-- > stack install syb 
-- to get Data.Generics.Aliases to work 
-- syb == "Scrap Your Boilerplate"

data Exp = Num      Int 
         | Plus     Exp Exp 
         | Times    Exp Exp 
         deriving (Show, Data, Typeable)

exp_1 = Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))

-- extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
-- Extend a generic query by a type-specific case

gshows :: Data a => a -> ShowS
-- gshows = render `extQ` (shows :: String -> ShowS) where
gshows = render where
  render t                            -- render :: (a -> ShowS)
    | isTuple = showChar '('
              . drop 1                -- removes first element from list
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t   -- doesn't show full list?
          -- listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t   -- doesn't show full list?
          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))    -- equivalent to not (any (not . flip elem "[]") (constructor ""))?
          isList = constructor "" == "(:)"

gshow :: Data a => a -> String 
gshow dataExpr = gshows dataExpr ""

data Stuff
    = None  
    | This Int 
    | That Bool
    | That2 (Stuff, Stuff)
    | Those [Stuff] 
    deriving (Show, Eq, Data, Typeable)

data Foo = Foo Char Int deriving (Data,Typeable)

x = This 5
y = That False
z = None
w = Those [x,y,z]
v = (x,y)

x_string = gshow x      -- "(This (5))"
y_string = gshow y      -- "(That (False))"
z_string = gshow z      -- "(None)"
w_string = gshow w      -- "(Those [(This(5)), (That (False)), (None)])" ??
v_string = gshow v      -- "(That2 ((This (5)), (That (False))))" ??
ex_string = gshow ([Just (2 ::Int) ],'c',Foo 'a' 5) 

main :: IO ()
main = do 
    putStrLn x_string 
    putStrLn y_string
    putStrLn z_string
    putStrLn w_string
    putStrLn ex_string


-- this doesn't work, type error
constructorString :: Data a => a -> String 
constructorString = showConstr . toConstr

-- But, I suspect the gshow function works 
-- because of the extQ operator or because buried within it
-- are the isNull, isTuple, isList, and gmapQ, which 
-- enforce that the passed in values are types than
-- can either be dealt with, or the failure is captured 

-- Alternative: https://stackoverflow.com/questions/34967447/dynamically-retrieve-values-of-a-data-type
-- | Generic show: an alternative to \"deriving Show\"
-- gshow :: Data a => a -> String
-- gshow x = gshows x ""

-- -- | Generic shows
-- gshows :: Data a => a -> ShowS

-- -- This is a prefix-show using surrounding "(" and ")",
-- -- where we recurse into subterms with gmapQ.
-- gshows = ( \t ->
--                 showChar '('
--               . (showString . showConstr . toConstr $ t)
--               . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)
--               . showChar ')'
--          ) `extQ` (shows :: String -> ShowS)