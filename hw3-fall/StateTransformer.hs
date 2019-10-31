module StateTransformer
  where

{- ST monad definition -}
type State = Int     -- in general, you could make State anything; we'll make it Int for simplicity
-- State Transformer Functor/Applicative/Monad
newtype ST a = S (State -> (a, State))
  -- function from initial state to result and post-state

instance Show a => Show (ST a) where
 {- Haskell can't show lamdas so just print ST contents by
    evaluating the state transformer on a concrete State: we use 0. This works
    well when the state transitions are simple like state+1 as
    in the tree relabelling function -}
  show x = let str = show (app x 0)
               in "S (" ++ "0 -> " ++ str ++ ")" 

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s
                in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
                     let (f,s') = app stf s
                         (x,s'') = app stx s'
                           in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
                  let (x,s') = app st s
                  in app (f x) s')
