--          Copyright Douglas W. Potter 2014.
-- Distributed under the Boost Software License, Version 1.0.
--    (See accompanying file LICENSE_1_0.txt or copy at
--          http://www.boost.org/LICENSE_1_0.txt)
module ParseLinearSystem
  (parseLinearInequalityLines, parseLinearInequality,
   parseTerms, parseDegreeOneMonomial, parseSense, parseRational)
       where
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim hiding (try)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as PT
import Control.Applicative ((<$>), (*>), (<*>))
import Data.List ()
import Data.Ratio ((%))
import Data.Word (Word)
import qualified LinearSystem as LS

--TODO rewrite more in applicative style
--TODO restrict what is exported, ? how to do this and support testing some internals?
lexer = PT.makeTokenParser emptyDef
integer = PT.integer lexer
natural = PT.natural lexer

-- TODO: instead of ';' use '\n' as the separator.
-- Note even if usages below of spaces changed to something that only
-- skips ' ', some other parser is skipping '\n'.
parseLinearInequalityLines :: GenParser Char st [LS.LinearInequality]
parseLinearInequalityLines = endBy1 parseLinearInequality eol
  where
    eol = char ';'

parseLinearInequality :: GenParser Char st LS.LinearInequality
parseLinearInequality =
  LS.LinearInequality <$> parseTerms <*> parseSense <*> parseRational

parseSense :: GenParser Char st LS.Sense
parseSense = spaces *> (badValues *> goodValues <?> "<=, >= or =")
 where 
   badValues =  foldr1 (>>) (map (notFollowedBy . string)  ["==", "=>", "=<"])
   goodValues = foldr1 (<|>)
     [(string "<=") *> (Prim.parserReturn LS.LesserOrEqual) 
     ,(string ">=") *> (Prim.parserReturn LS.GreaterOrEqual)
     ,(string  "=") *> (Prim.parserReturn LS.Equal)]

parseTerms ::  GenParser Char st [LS.DegreeOneMonomial]
parseTerms = (try p) <?> 
  "an expression of DegreeOneMonomials with unique variable indices"
  where
    p = do
      spaces
      x <- parseDegreeOneMonomial
      xs <- pRemaining
-- While this way of checking for repeating varaible indicies gives quadratic
-- complexity in the number of variables (when reading), it does show 
-- the location of the duplication in the input without much coding work.
-- The complexity could be reduced by scaning the entire list but then 
-- locating the error in the input will be harded or maintaining some 
-- sorted data structure.
      let i = LS.variableIndex x
      if i `elem` usedIndices xs
        then fail $ "variable index " ++ (show i) ++ " repeated"
        else return $ x : xs
    pRemaining = (try (lookAhead $ char '-') *> p)
      <|> (char '+' *> p)
      <|> (return [])
    usedIndices = map LS.variableIndex

parseDegreeOneMonomial :: GenParser Char st LS.DegreeOneMonomial
parseDegreeOneMonomial = do
  r <- parseRational
  string LS.coefficientIndexSep
  spaces
  index <- parseWord
  return $ LS.DegreeOneMonomial {
    LS.coefficient = r,
    LS.variableIndex = index}

parseRational :: GenParser Char st Rational
parseRational = spaces *> ((%) <$> integer <*> parseDenominator)
  where
    parseDenominator = (notFollowedBy (string LS.fracSep) *> return 1)
      <|> ((string LS.fracSep) >> spaces >> parseNonZeroInteger)

parseNonZeroInteger :: GenParser Char st Integer
parseNonZeroInteger = try p <?> "a nonzero integer"
 where p = do
         x <- integer
         case x of
           0 -> fail ""
           _ -> return x

parseWord :: GenParser Char st Word
parseWord = do
  n <- natural
  let w = fromInteger n :: Word
  if toInteger w == n then
    return w
    else fail $ "index parsed as " ++ (show n) ++
         "unrepresentable as index type Word (unsigned Int)."
