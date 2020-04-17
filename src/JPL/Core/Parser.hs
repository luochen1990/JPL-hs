module JPL.Core.Parser (
    parseExpr
) where

import Text.Megaparsec hiding (sepBy, sepBy1)
import Text.Megaparsec.Char hiding (space, space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import qualified Data.Map as M
import Data.Char
import Data.Void
import Data.Functor
import Control.Monad.Identity
import Control.Monad.State
import JPL.Core.Definitions
import JPL.Core.Functions
-- import Text.Megaparsec.Debug
dbg = const id

type Parser = ParsecT Void String Identity

parseExpr :: String -> Either String Expr
parseExpr s = either (Left . errorBundlePretty) Right (runParser (pExpr <* space <* eof) "input" s)

pExpr :: Parser Expr
pExpr = pLam

pLam :: Parser Expr
pLam = space *> (
        Lam <$> try (pIdent <* tok '?' <* space) <*> pLam
    <|> pCase
    ) <?> "pLam"

pCase :: Parser Expr
pCase = Case <$> (toks "case" *> pApp <* toks "of") <*> some ((,) <$> (pPattern <* tok ':') <*> pApp)
    <|> pSeq
    <?> "pCase"

pSeq :: Parser Expr
pSeq = space *> (
        Let <$> try (pIdent <* toks ":=") <*> (pApp <* tok ';') <*> pSeq
    <|> Assume <$> (toks "assume " *> pApp <* tok ';') <*> pSeq
    <|> Assert <$> (toks "assert " *> pApp <* tok ';') <*> pSeq
    <|> pApp
    ) <?> "pSeq"

pApp :: Parser Expr
pApp = foldl1 App <$> pAtom `sepBy1` (spaceChar *> space)
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
pIdent = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    <?> "pIdent"

pLitNumber :: Parser Double
pLitNumber = try ((char '-' $> negate <|> pure id) <*> L.float)
    <|> L.decimal
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

pPattern :: Parser Pattern
pPattern = undefined

-- ** parser utils

spaceChar :: Parser Char
spaceChar = (char ' ' <|> tab) <?> "space/tab"

space :: Parser ()
space = () <$ many spaceChar <?> "spaces/tabs"

tok :: Char -> Parser Char
tok c = try (space *> char c)

toks :: String -> Parser String
toks s | isAlphaNum (last s) = try (space *> string s <* notFollowedBy alphaNumChar)
toks s = try (space *> string s)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p op = liftM2 (:) p (many (try (op *> p)))

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p op = liftM2 (:) p (many (try (op *> p))) <|> pure []

