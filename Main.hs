module Main where
import Parser
import Lambda

import Control.Applicative
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Maybe
import System.IO
    
repl   :: Maybe Handle -> StateT [(Var, Term)] IO ()
repl h = do
  maybe ((liftIO $ putStr "> ") >> (liftIO $ hFlush stdout)) (const $ return ()) h
  xs <- get 
  s <- filter (not . (== ' ')) <$> (liftIO $ maybe getLine hGetLine h)

  if length s > 0
    then either (\e -> liftIO $ putStrLn $ "error: " ++ e)  
             (\t -> either (evalDecl xs) (evalTerm xs) t) 
             $ parseLambda s
    else return ()
  
  where 
    evalDecl xs (v, t)
        = let e = either (\v' -> put ((v, fromJust $ lookup v' xs):xs))
                         (\t' -> put ((v, t'):xs))
          in either evlErr e $ equivalent xs t
           
    evalTerm xs t
         = let e = either (\v' -> liftIO $ putStrLn v')
                          (\t' -> liftIO $ putStrLn $ show t')
           in either evlErr e $ equivalent xs t
    
    evlErr (Undeclared v') 
      = liftIO $ putStrLn $ "error: Undeclared variable " ++ v'
    evlErr CantReduce
      = liftIO $ putStrLn "error: Can't reduce expression"

main :: IO ()
main = do
  let fname = "terms.lambda"
  n <- nrLines fname
  h <- openFile fname ReadMode
  
  evalStateT ((sequence_ $ replicate n (repl $ Just h)) 
              >> (sequence_ $ repeat $ repl Nothing)) []
    
    where 
      nrLines x = (length . lines) <$> (openFile x ReadMode >>= hGetContents)
