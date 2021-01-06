--Credit : Thinking Functionally with Haskell Ch 12 - Parsing
--TODO : Parse without using Monads
import Data.Char

main :: IO()
main  = interact solve

newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s -> [(y, s'')
                          | (x, s') <- apply p s,
                            (y, s'') <- apply (q x) s'])

instance Functor Parser where
  fmap g p = Parser (\s -> case apply p s of
                             [] -> []
                             [(x, s')] -> [(g x, s')])

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  p <*> q = Parser (\s -> case apply p s of
                            [] -> []
                            [(x, s')] -> apply (fmap x q) s')

data Expr = Con Int | Bin Op Expr Expr deriving (Show)
data Op = Plus | Minus | Mul | Div deriving (Show)

solve :: String -> String
solve = show . sum . map (eval . (parse expr) ) . lines

eval :: Expr -> Int
eval (Con x) = x
eval (Bin Plus e1 e2) = eval e1 + eval e2
eval (Bin Mul e1 e2) = eval e1 * eval e2
eval (Bin Minus e1 e2) = eval e1 - eval e2

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p


expr :: Parser Expr
expr = token (term >>= rest)

rest e1 = do {p <- addop;
              e2 <- term;
              rest (Bin p e1 e2)}
          <|> return e1

term = token (factor >>= more)

more e1 = do {p <- mulop;
              e2 <- factor;
              more (Bin p e1 e2)}
          <|> return e1

factor = token (constant <|> paren expr)
  
constant = do {n <- nat; return (Con n)}

addop = (symbol "*" >> return Mul) <|>
        (symbol "-" >> return Minus)

mulop = (symbol "/" >> return Div) <|>
        (symbol "+" >> return Plus)


paren :: Parser a -> Parser a
paren p = do {symbol "(";
              x <- p;
              symbol ")";
              return x}

nat = do {ds <- some digit;
          return (foldl1 shiftl ds)}
      where shiftl m n = 10*m+n

many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p;return (x:xs)}
         <|> none

none = return []

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol :: String -> Parser()
symbol xs = space >> string xs

token :: Parser a -> Parser a
token p = space >> p

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
        where f s = let ps = apply p s in
                    if null ps then apply q s
                    else ps

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)}
            where cvt d = fromEnum d - fromEnum '0'

char :: Char -> Parser ()
char x = do {c <- sat (==x); return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc;
           if p c then return c
           else failParse}

failParse = Parser (\s -> [])

getc :: Parser Char
getc = Parser f
       where f [] = []
             f (c:cs) = [(c, cs)]
