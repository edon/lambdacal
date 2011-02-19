module Lambda (
  Var, 
  Term(..), 
  Result(..),
  Error(..), 
  equivalent
              ) where 

import Control.Applicative
import Data.Maybe

type Var  = String
data Term = Var Var
          | Abs Var Term
          | App Term Term
--            deriving (Show)
                     
type Result = Either Var Term 
data Error  = Undeclared Var | CantReduce 
              deriving (Show)

instance Show Term where
  show (Var v) = v
  show (Abs v t) = "\\" ++ v ++ "." ++ (show t)
  show (App (Var v1) (Var v2)) = v1 ++ v2
  show (App (Var v) t) = v ++ "(" ++ (show t) ++ ")"
  show (App t (Var v)) = "(" ++ (show t) ++ ")" ++ v
  show (App t1 t2) = "(" ++ (show t1) ++ ")"
                   ++ "(" ++ (show t2) ++ ")"

betaReduce                    :: Term -> Maybe Term
betaReduce (App (Abs v t') t) = replaceIn (v,t) t'
    where 
      replaceIn (v, t) (Var v')
          = if   v == v'
            then Just t
            else Just $ Var v'
      replaceIn (v, t) (Abs v' t')
          = if   v == v'
            then Nothing
            else Abs v' <$> replaceIn (v, t) t'
      replaceIn (v, t) (App t1 t2)
          = t' >>= betaReduce
            where t' = App 
                       <$> (replaceIn (v, t) t1) 
                       <*> (replaceIn (v, t) t2)
betaReduce (App t1 t2)        = betaReduce t1 >>= \t1' -> betaReduce $ App t1' t2
betaReduce x                  = Just x

alphaEquivalent :: Term -> Term -> Bool
alphaEquivalent = f []
    where 
      f :: [(String, String)] -> Term -> Term -> Bool
      f xs (Var v1) (Var v2) 
          | v1 == v2   = True
          | otherwise = case (==) <$> Just v2 <*> lookup v1 xs of
                          Just r -> r
                          Nothing -> False
      f xs (App t1 t1') (App t2 t2') = f xs t1 t2 && (f xs t1' t2')
      f xs (Abs v1 t1) (Abs v2 t2) 
          = f ((v1, v2):xs) t1 t2
      f _ _ _ = False

decurry           :: Term -> Term
decurry (Abs v t) = foldl (\f c -> f . Abs [c]) id v $ decurry t
decurry (App t1 t2) = App (decurry t1) (decurry t2)
decurry x         = x

undeclaredVar :: [(Var, Term)] -> Term -> Maybe Var
undeclaredVar = f []
    where f fs xs (Var v) = if v `elem` fs then Nothing
                            else case lookup v xs of
                                   Just _ -> Nothing
                                   Nothing -> Just v
          f fs xs (Abs v t) = f (v:fs) xs t
          f fs xs (App t1 t2) = case f fs xs t1 of
                                  Just v -> Just v
                                  Nothing -> f fs xs t2

expand :: [(Var, Term)] -> Term -> Either Var Term
expand xs t = case undeclaredVar xs t of
                Just v -> Left v
                Nothing -> Right $ f [] xs t
    where 
      f fs xs (Var v) = if not $ v `elem` fs then fromJust $ lookup v xs
                        else (Var v)
      f fs xs (Abs v t) = Abs v $ f (v:fs) xs t
      f fs xs (App t1 t2) = App (f fs xs t1) (f fs xs t2)

equivalent :: [(Var, Term)] -> Term -> Either Error Result
equivalent xs t = case expand xs t' of
                    Left v -> Left $ Undeclared v
                    Right term -> case betaReduce term of 
                                   Nothing -> Left CantReduce
                                   Just x -> Right $ f x
    where 
      f t = case equivs xs t of 
              [] -> Right t
              vs -> Left vs
      equivs [] t = []
      equivs (x:xs) t = if t `alphaEquivalent` (snd x) then (fst x)
                           else equivs xs t
      t' = decurry t
