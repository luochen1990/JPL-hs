module JPL.Core.Parser (
    parseExpr, parsed
) where

import Text.Megaparsec hiding (sepBy, sepBy1)
import Text.Megaparsec.Char hiding (space, space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import Control.Monad.Combinators.Expr
import qualified Data.Map as M
import Data.Char
import Data.Void
import Data.Functor
import Control.Applicative ((<**>))
import Control.Monad.Identity
import Control.Monad.State
import JPL.Core.Definitions
import JPL.Core.Functions
-- import Text.Megaparsec.Debug
dbg = const id

type Parser = ParsecT Void String Identity

parseExpr :: String -> Either String Expr
parseExpr s = either (Left . errorBundlePretty) Right (runParser (pExpr <* space <* eof) "input" s)

parsed :: String -> Expr
parsed s = either error id (parseExpr s)

{-
syntax examples:

term1 := x? y? x + y
term2 := x? y? add x y
term3 := x? y? x @add y
term4 := x? y? let r (x @add y); r @add 1
term5 := x? x @(null? 0 | "one"? 1 | _? 2)
-}

pExpr :: Parser Expr
pExpr = pAlt

pAlt :: Parser Expr
pAlt = space *> (pLam `chainr1` (Alt <$ tok '|' <* space))
    <?> "pAlt"

pLam :: Parser Expr
pLam = space *> (pSeq `chainr1` (Lam <$ tok '?' <* space)) --NOTE: expand pPattern to pLam here
    <?> "pLam"

pSeq :: Parser Expr
pSeq = space *> (pSuffixApp `chainr1` (App <$ tok ';' <* space))
    <?> "pSeq"

pSuffixApp :: Parser Expr
pSuffixApp = space *> (pBin `chainl1` (flip App <$ tok '@' <* space))
    <?> "pSuffixApp"

pBin :: Parser Expr
pBin = pApp
    <?> "pBin"

--"+-*/%^&></\:=."
--
--binOps = M.fromList [
--    ("+", "add")
--    ("-", "sub")
--    ("*", "mul")
--    ("/", "div")
--    ("//", "exactDiv")
--    ("%", "mod")
--    ]

pApp :: Parser Expr
pApp = foldl1 App <$> pAtom `sepBy1'` (spaceChar *> space)
    <?> "pApp"

pAtom :: Parser Expr
pAtom = space *> (
        Null <$ toks "null"
    <|> Number <$> pLitNumber
    <|> Text <$> pLitText
    <|> Boolean <$> pLitBoolean
    <|> Var <$> pIdent
    <|> Dict <$> (tok '{' *> (((,) <$> (space *> (pIdent <|> pLitText) <* tok ':') <*> pExpr) `sepBy` tok ',') <* tok '}')
    <|> List <$> (tok '[' *> pExpr `sepBy` tok ',' <* tok ']')
    <|> tok '(' *> pExpr <* tok ')'
    ) <?> "pAtom"

pIdent :: Parser Ident
pIdent = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    <?> "pIdent"

pLitNumber :: Parser Double
pLitNumber = L.signed (pure ()) (toRealFloat <$> L.scientific)
    <?> "pLitNumber"

pLitText :: Parser String
pLitText = char '"' *> many (pSpecialChar <|> anySingleBut '"') <* char '"'
    <?> "pLitText"

pSpecialChar :: Parser Char
pSpecialChar = char '\\' *> (char '"' <|> char '\\') --TODO: more special chars
    <?> "pSpecialChar"

pLitBoolean :: Parser Bool
pLitBoolean = toks "true" $> True <|> toks "false" $> False
    <?> "pLitBoolean"

-- ** parser utils

spaceChar :: Parser Char
spaceChar = (char ' ' <|> tab) <?> "spaceChar"

space :: Parser ()
space = void (many spaceChar <?> "spaces")

tok :: Char -> Parser Char
tok c = try (space *> char c)

toks :: String -> Parser String
toks s | isAlphaNum (last s) = try (space *> string s <* notFollowedBy alphaNumChar)
toks s = try (space *> string s)

sepBy1' :: Parser a -> Parser b -> Parser [a]
sepBy1' p op = liftA2 (:) p (many (try (op *> p)))

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p op = liftA2 (:) p (many (op *> p)) <|> pure []

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <**> (foldr (flip (.)) id <$> many (flip <$> op <*> p))

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
    p1 <- p
    mop1 <- optional op
    case mop1 of
        Just op1 -> op1 p1 <$> chainr1 p op
        Nothing -> pure p1
