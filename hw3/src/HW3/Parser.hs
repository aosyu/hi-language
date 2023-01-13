{-# LANGUAGE PartialTypeSignatures #-}
module HW3.Parser (parse) where

import Control.Monad.Combinators.Expr
import Control.Applicative (liftA2)
import Data.Void
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import HW3.Base

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pExpr <* eof) ""

pExpr :: Parser HiExpr
pExpr = makeExprParser (sc *> pHiExpr <* sc) operatorTable

-- | The main parser
-- pHiExpr -> ((pHiExpr) | pHiExpr)[(pExpr,pExpr,...) | .string.string...]
pHiExpr :: Parser HiExpr
pHiExpr = choice
  [ brackets pExpr
  , pExprValue
  ] >>= pApplication

pExprValue :: Parser HiExpr
pExprValue = choice
  [ pFunctionName
  , pValue
  , pList
  , pDict
  ]

-- | parses a functional application like function(arg1,arg2,...)
-- supports multiple applications: function(arg1,...)(arg11,...)(...)
pApplication :: HiExpr -> Parser HiExpr
pApplication expr = do
  nextSb <- optional (lBr <|> symbol ".")
  case nextSb of
    Nothing -> return expr
    Just "(" -> pSequence comma <* rBr >>= return . HiExprApply expr >>= pApplication
    Just _ -> pCharSequence >>= (\res -> return $ HiExprApply expr [res]) >>= pApplication

-- | parses Dict fields
pCharSequence :: Parser HiExpr
pCharSequence = HiExprValue . HiValueString . (T.pack . List.intercalate "-") <$>
                                                (((:) <$> satisfy Char.isAlpha <*> many (satisfy Char.isAlphaNum)) `sepBy1` char '-')

-- | parser for HiExprValue
pValue :: Parser HiExpr
pValue = HiExprValue <$> choice
  [ pNumber
  , pBool
  , pText
  , pNull
  , pCwd
  , pNow
  , pBytes
  ]

-- | parses function name
pFunctionName :: Parser HiExpr
pFunctionName = HiExprValue <$> HiValueFunction <$> do
  op <- lexeme $ choice $ ((\name -> try $ chunk name <* notFollowedBy (symbol "-")) <$> hiFunNames)
  return $ hiFunByName op

-- | parses text in quotes
pText :: Parser HiValue
pText = HiValueString . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pList :: Parser HiExpr
pList = do
  elements <- between (symbol "[") (symbol "]") $ pSequence comma
  let f = HiExprValue $ HiValueFunction HiFunList
  return $ HiExprApply f elements

pDict :: Parser HiExpr
pDict = do
  elements <-between (symbol "{") (symbol "}") $ (liftA2 (,) (lexeme $ pExpr <* symbol ":") pExpr `sepBy` comma)
  return $ HiExprDict elements

-- | cwd keyword
pCwd :: Parser HiValue
pCwd = (HiValueAction $ HiActionCwd) <$ (chunk "cwd")

-- | now keyword
pNow :: Parser HiValue
pNow = (HiValueAction $ HiActionNow) <$ (chunk "now")

pNull :: Parser HiValue
pNull = HiValueNull <$ (chunk "null")

pBool :: Parser HiValue
pBool = HiValueBool <$> (False <$ chunk "false" <|> True <$ chunk "true")

pNumber :: Parser HiValue
pNumber = HiValueNumber <$> toRational <$> (lexeme $ L.signed sc L.scientific)

pBytes :: Parser HiValue
pBytes = do
  elements <- between (symbol "[#") (symbol "#]") $ many pByte
  return $ HiValueBytes $ BS.pack elements

-- | Utils
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

lBr, rBr, comma :: Parser String
lBr = symbol "("
rBr = symbol ")"
comma = symbol ","

brackets :: Parser a -> Parser a
brackets = between lBr rBr

sc :: Parser ()
sc = L.space space1 empty empty

pByte :: Parser Word8
pByte = lexeme $ do
  val <- lookAhead (takeWhile1P Nothing Char.isHexDigit)
  if length val == 2
  then L.hexadecimal
  else fail "Byte consists of two hex numbers"

-- | parser for sequence of elements separated by sep
pSequence :: Parser String -> Parser [HiExpr]
pSequence sep = (:) <$> pExpr <*> many (sep *> pExpr) <|> mempty

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
    [
      Postfix $ foldr1 (.) <$> some (HiExprRun <$ symbol "!")
    ],
    [
      binaryLeft (chunk "*") HiFunMul,
      binaryLeft ((lexeme . try) (chunk "/" <* notFollowedBy (symbol "="))) HiFunDiv
    ],
    [
      binaryLeft (chunk "+") HiFunAdd,
      binaryLeft (chunk "-") HiFunSub
    ],
    [
      binaryNon (chunk ">=") HiFunNotLessThan,
      binaryNon (chunk "<=") HiFunNotGreaterThan,
      binaryNon (chunk "/=") HiFunNotEquals,
      binaryNon (chunk "==") HiFunEquals,
      binaryNon (chunk "<")  HiFunLessThan,
      binaryNon (chunk ">")  HiFunGreaterThan
    ],
    [
      binaryRight (chunk "&&") HiFunAnd
    ],
    [
      binaryRight (chunk "||") HiFunOr
    ]
  ]

binaryLeft :: Parser String -> HiFun -> Operator Parser HiExpr
binaryLeft = binary InfixL

binaryRight :: Parser String -> HiFun -> Operator Parser HiExpr
binaryRight = binary InfixR

binaryNon :: Parser String -> HiFun -> Operator Parser HiExpr
binaryNon = binary InfixN

binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -- associativity
  -> Parser String -- operation parser
  -> HiFun
  -> Operator Parser HiExpr
binary associativity parser hiFun = associativity $ (\a b -> HiExprApply (HiExprValue $ HiValueFunction hiFun) [a, b]) <$ parser