{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Data 
import Data.Generics.Aliases
-- note: had to run on command line
-- > stack install syb 
-- to get Data.Generics.Aliases to work 
-- syb == "Scrap Your Boilerplate"

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple = showChar '('
              . drop 1
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
          listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))
          isList = constructor "" == "(:)"

gshow :: Data a => a -> String 
gshow dataExpr = gshows dataExpr ""

data Stuff
    = None  
    | This Int 
    | That Bool
    | Those [Stuff] 
    deriving (Show, Eq, Data, Typeable)

data Foo = Foo Char Int deriving (Data,Typeable)

x = This 5
y = That False 
z = None
w = Those [x,y,z]

x_string = gshow x 
y_string = gshow y 
z_string = gshow z 
w_string = gshow w
ex_string = gshow ([Just (2 ::Int) ],'c',Foo 'a' 5) 

main :: IO ()
main = do 
    putStrLn x_string 
    putStrLn y_string
    putStrLn z_string
    putStrLn w_string
    putStrLn ex_string


-- this doesn't work, type error
-- constructorString :: Data a => String 
-- constructorString = showConstr . toConstr

-- But, I suspect the gshow function works 
-- because of the extQ operator or because buried within it
-- are the isNull, isTuple, isList, and gmapQ, which 
-- enforce that the passed in values are types than
-- can either be dealt with, or the failure is captured 
