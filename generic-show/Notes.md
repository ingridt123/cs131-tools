# Unwrapping Generic Show (Data.Generics.Text)

### gshows :: Data a => a -> ShowS
(See additional comments in `gshow.hs`)

Adds subterms to output
```(foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t) :: ShowS```
* `gmapQ ((showChar ' ' .) . gshows) $ t :: [ShowS]`
    * `gmapQ` iterates over the subterms of `t`
    * Applies `((showChar ' ' .) . gshows) :: String -> String` to each subterm
        * Recursively calls `gshows`
        * `(showChar ' ' .)` prepends a space to the output of `gshows`
        * `showChar ' '` approximately equivalent to `(:) ' '`
    * Outputs a list of `String -> String = ShowS` of subterms for string concatenation
        * E.g. `gmapQ ((showChar ' ' .) . gshows) $ [1,2,3] -> [(++) " (1)", (++) " ((:) (2) ((:) (3) ([])))"]`

* `foldr (.) id :: Foldable t => t (b -> b) -> b -> b`
    * `foldr (.) id [x1, x2, ..., xn] == x1 . (x2 . ... (xn . id) ...)`
    * Reduces list by applying function composition to the elements in the list (initial value is `id`)

* Reduces output list from `gmapQ` using `foldr`, resulting in `String -> String = ShowS` that is used for string concatenation
    * E.g.
      ```
      foldr (.) id [(++) " (1)", (++) " ((:) (2) ((:) (3) ([])))"] 
      = ((++) " (1)" . (++) " ((:) (2) ((:) (3) ([])))" . id))
      
      # Let us simplify this using x,y
      x = (++) " (1)"                       :: ShowS
      y = (++) " ((:) (2) ((:) (3) ([])))"  :: ShowS

      = foldr (.) [x,y]
      = (x . (y . id))   :: ShowS

      # Say we apply "" to the above function (in the actual gshows function, combined with parentheses and constructor name between applying "" in the gshow function)
      (x . (y . id)) ""  :: String
      = (x . (y ""))
      = (x " ((:) (2) ((:) (3) ([])))")
      = " (1) ((:) (2) ((:) (3) ([])))"
      ```