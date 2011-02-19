module Parser (parseLambda) where
import Lambda
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec

parseLambda   :: String -> Either String (Either (Var, Term) Term)
parseLambda s = if ':' `elem` s 
                then 
                    case parse declaration "" s of
                      Left err -> Left $ show err
                      Right (v, t) -> Right $ Left (v, t)
                else
                    case parse lambda "" s of
                      Left err -> Left $ show err
                      Right t -> Right $ Right t
                      
declaration :: Parser (Var, Term)
declaration = do
  v <- many1 upper
  string ":="
  t <- lambda
  return $ (v, t)

lambda :: Parser Term
lambda = bracketedTerm <|> term

term :: Parser Term
term = try appChain
               <|> abstraction
               <|> var

bracketedTerm :: Parser Term
bracketedTerm = do
  char '('
  t <- term
  char ')'
  return t

var :: Parser Term
var = (Var <$> many1 upper) <|> (Var . pure <$> letter)

abstraction :: Parser Term
abstraction = do
  char '\\'
  vars <- many1 lower
  char '.'
  os <- bracketedTerm <|> abstraction <|> var
  return $ Abs vars os

appChain :: Parser Term
appChain = do 
  let otherTs = abstraction <|> var <|> bracketedTerm
  t1 <- otherTs
  t2 <- otherTs
  ts <- many otherTs
  return $ foldl App t1 (t2:ts)
