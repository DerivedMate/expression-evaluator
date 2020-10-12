module Parser where
import Data.Char (digitToInt)
import Exp

data Sign  = Neg
           | Pos
  deriving (Show, Eq)
data Token = Digit Int
           | Exp
           | TVar Char
           | Sign Sign
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = aux input []
  where 
    aux :: String -> [Token] -> [Token]
    aux []     out = reverse out
    aux (c:cs) out
      | c `elem` ['0'..'9'] = aux cs (Digit (digitToInt c) : out)
      | c == '^'            = aux cs (Exp                  : out)
      | c == '+'            = aux cs (Sign Pos             : out)
      | c == '-'            = aux cs (Sign Neg             : out)
      | c `elem` ['a'..'z'] = aux cs (TVar c               : out)
      | otherwise           = aux cs out
    
parse :: [Token] -> Exs
parse ts = aux ts (E 1 []) []
  where 
    aux :: [Token] -> Expression -> Exs -> Exs
    aux [] e exs = reverse $ map reverseChain $ e : exs
    aux (t:ts) ex@(E quant cs) exs
      | isSign t && null cs  = aux ts (E (sgnOfSign t) [Id]) exs
      | isSign t             = aux ts (E (sgnOfSign t) []) (ex:exs)
      | isDigit t && null cs = aux ts (E (quant * intOfDigit t) [Id]) exs 
      | isDigit t            = aux ts (E quant (expVar (head cs) (intOfDigit t) : tail cs)) exs
      | isVar t              = aux ts (E quant ((Var (varOfToken t) Nothing) : cs)) exs     
      | isExp t              = aux ts ex exs  
      | otherwise            = aux ts ex exs       

express :: String -> Exs
express = addExpressions . map cleanChain . parse . tokenize

-- helpers
reverseChain :: Expression -> Expression
reverseChain (E q c) = E q (reverse c)

cleanChain :: Expression -> Expression
cleanChain (E q c) = E q (filter (not . (==Id)) c)

expVar :: Var -> Int -> Var
expVar (Var v Nothing) d  = Var v (Just d)
expVar (Var v (Just e)) d = Var v (Just (e*10 + d))

isDigit :: Token -> Bool
isDigit (Digit _) = True
isDigit _         = False

isVar :: Token -> Bool
isVar (TVar _)    = True
isVar _           = False

isSign :: Token -> Bool
isSign (Sign _)   = True
isSign _          = False

isExp :: Token -> Bool
isExp Exp         = True
isExp _           = False

sgnOfSign :: Token -> Int
sgnOfSign (Sign Pos) = 1
sgnOfSign (Sign Neg) = -1

intOfDigit :: Token -> Int
intOfDigit (Digit d) = d

varOfToken :: Token -> Char
varOfToken (TVar v) = v
-- intOfDigit _         = 1