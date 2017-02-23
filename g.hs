{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}

data Term a where
  Lit    :: Int -> Term Int
  Bool   :: Bool -> Term Bool
  Succ   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a
  Pair   :: a ~ b => Term a -> Term b -> Term (a, b)

deriving instance Show (Term a)

eval :: Term a -> a
eval (Lit i)      = i
eval (Bool b)     = b
eval (Succ t)     = 1 + eval t
eval (IsZero t)   = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)
