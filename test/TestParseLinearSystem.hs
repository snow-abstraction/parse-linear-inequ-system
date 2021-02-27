--          Copyright Douglas W. Potter 2014.
-- Distributed under the Boost Software License, Version 1.0.
--    (See accompanying file LICENSE_1_0.txt or copy at
--          http://www.boost.org/LICENSE_1_0.txt)
-- TG = test group
{-# LANGUAGE  FlexibleContexts #-}
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit ((@?), (@=?), testCase)
import Text.ParserCombinators.Parsec (parse)
import Data.Ratio ((%))

import LinearSystem (Sense(..), DegreeOneMonomial(..), LinearInequality(..),
  makeDegreeOneMonomial, fracSep, coefficientIndexSep)
import ArbitraryLinearSystem ()
-- the parsing functions tested
import ParseLinearSystem (parseLinearInequalityLines, parseLinearInequality,
  parseTerms, parseDegreeOneMonomial, parseSense, parseRational)

-- TODO should test that good error msgs are produce in the more tricky cases

tg = testGroup
tc = testCase
ciSep = coefficientIndexSep

parseMsgNoExplain s e = "'" ++ s ++ "' parsed as expected as "++ show e

testedByMsg = " tested by checking that "

parseMsgExplain m s e =
  m ++ testedByMsg ++ parseMsgNoExplain s e

parsesTCBase m p s e =  tc (m s e) $ Just e @=? (maybeParse p s)

parsesTC p s e = parsesTCBase parseMsgNoExplain p s e

descParsescTC m p s e = parsesTCBase (parseMsgExplain m) p s e

failMsgNoExplain s = "'"++ s++ "' fails to parse"

failMsgExplain m s =  m ++ testedByMsg ++ (failMsgNoExplain s)

failParseTCBase m p s = tc (m s) $ (isParseError p s) @? assertMsg where
    assertMsg = ""

failParseTC p s = failParseTCBase failMsgNoExplain p s

descFailParseTC m p s = failParseTCBase (failMsgExplain m) p s 

parsesLikeTC p s l = tc msg $ e @=? (maybeParse p s) where
  e = maybeParse p l
  msg = "'"++ s++ "' parses like "++ "'"++ l++ "'"++
        " which parses to "++ show e

maybeParse p s = case parse p "" s of
  Right x -> Just x
  Left _ -> Nothing

isParseError p s = case parse p "" s of
  Right _ -> False
  Left _  -> True

-- TODO: should this main be here when running tests via cabal
main = defaultMain tests

tests = testGroup "Tests"
        [qcProperties, unitTests]

qcProperties = tg "QuickCheck Properties"
  [testProperty
   "for any DegreeOneMonomial m, parsing (show m) equals m" $ 
   \m -> Just m == (maybeParse parseDegreeOneMonomial 
                    (show m))]

unitTests = tg "Unit tests"
  [parseSenseTG,
   parseRationalTG,
   parseDegreeOneMonomialTG,
   parseTermsTG,
   parseLinearInequalityTG,
   parseLinearInequalityLinesTG]
  
parseSenseTG = tg "parseSense tests"
 [parsesTC parseSense "<=" LesserOrEqual 
 ,parsesTC parseSense ">=" GreaterOrEqual   
 ,parsesTC parseSense "=" Equal   
 ,parsesTC parseSense " =" Equal   
 ,failParseTC parseSense "=<"
 ,failParseTC parseSense "=>"  
 ,failParseTC parseSense "=="  
 ,failParseTC parseSense ""  
 ,failParseTC parseSense " "  
 ,failParseTC parseSense "< ="  
 ,failParseTC parseSense "asdf"  
 ,failParseTC parseSense " asdf"  ]

parseRationalTG = tg "parseRational tests"
 [parsesTC parseRational ("1235 "++fracSep++" 55") (1235 % 55) 
 ,descParsescTC "Neg. sign on nominator should parse"
  parseRational ("-1235 "++fracSep++" 55") (-1235 % 55) 
 ,descParsescTC "Neg. sign on denominator should parse" 
  parseRational ("1235 "++fracSep++" -55") (-1235 % 55) 
 ,descParsescTC "Neg. sign on nominator and denominator should parse"
  parseRational ("-1235 "++fracSep++" -55") (1235 % 55) 
-- This behavior is preferable to failing since otherwise we have problem 
-- of looking ahead until fracSep, a reserved token or symbol, eol or eof is found. 
 ,descParsescTC "Unexpected - should end parsing without failure" 
  parseRational ("1235- "++fracSep++" 55") (1235 % 1) 
 ,descParsescTC "Extra spaces at beginning and end should parse" 
  parseRational (" 2 "++fracSep++" 3 ") (2 % 3) 
 ,descParsescTC "Expression without spaces shoud parse" 
  parseRational ("7"++fracSep++"55") (7 % 55) 
 ,descParsescTC "Integer format (not fraction) should parse" 
  parseRational "4" (4 % 1) 
 ,descParsescTC "Neg. sign in integer format should parse" 
  parseRational "-7 " (-7 % 1) 
 ,descParsescTC "Extra space at beginning of integer format should parse" 
  parseRational " 11" (11 % 1) 
 ,descParsescTC "Extra space before - in integer format should parse" 
  parseRational " -77" (-77 % 1) 
 ,descParsescTC "Extra space around - in integer format should parse" 
  parseRational " - 1234 " (-1234 % 1) 
 
 ,parsesLikeTC parseRational ("1235 "++fracSep++" 55-")
  ("1235 "++fracSep++" 55") 
 ,parsesLikeTC parseRational ("1870 "++fracSep++" -935")
  ("-64 "++fracSep++" 32")  
 ,parsesLikeTC parseRational "5" ("5"++fracSep++"1") 
 ,parsesLikeTC parseRational "5 5" "5"    
 ,parsesLikeTC parseRational ("1235- "++fracSep++" 55") "1235" 
 ,parsesLikeTC parseRational "1 % 2" "1"  
 ,parsesLikeTC parseRational "2 1"  "2" 
  
 ,failParseTC parseRational ("1"++fracSep) 
 ,failParseTC parseRational ("1"++fracSep++fracSep++"2") 
 ,failParseTC parseRational ("5 "++fracSep++" 0") 
 ,failParseTC parseRational ("1235 "++fracSep++"--55")   
 ,failParseTC parseRational ""  
 ,failParseTC parseRational "asfd"  
 ,failParseTC parseRational " "  
 ,descFailParseTC "Double negation should not parse"
  parseRational ("--1235 "++fracSep++" 55")  
 ,failParseTC parseRational (fracSep++"5")]

parseDegreeOneMonomialTG = tg "parseDegreeOneMonomial tests"
  [parsesTC parseDegreeOneMonomial ("1123 "++fracSep++" 123"++ciSep++"0")
   (DegreeOneMonomial (1123 % 123) 0)
  ,parsesTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++ciSep++" 5 ")
   (DegreeOneMonomial (-1123 % 123) 5) 
  ,parsesTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++ciSep++" 5 ")
   (DegreeOneMonomial (-1123 % 123) 5) 
  ,parsesTC parseDegreeOneMonomial
   ("7"++fracSep++"9"++ciSep++"2"++fracSep)
   (DegreeOneMonomial (7 % 9) 2) 
  ,parsesTC parseDegreeOneMonomial
   (" - 1123"++fracSep++"-123"++ciSep++"5  ")
   (DegreeOneMonomial (1123 % 123) 5) 
  ,parsesTC parseDegreeOneMonomial (" - 7"++ciSep++"12312  ")
   (DegreeOneMonomial (-7 % 1) 12312) 
  ,failParseTC parseDegreeOneMonomial (" 1123 "++fracSep++" -123 "++" 5 ")
  ,failParseTC parseDegreeOneMonomial
   (" 1123- "++fracSep++" 123 "++ciSep++" 5 ")
  ,failParseTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" 123 "++ciSep++" -5 ")
  ,descFailParseTC "Repeated coefficient-index seperator should not parse"
   parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++ciSep++ciSep++" 5")
  ,failParseTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++"."++" 5 ")
  ,failParseTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++"."++" 5 ")
  ,failParseTC parseDegreeOneMonomial
   (" 1123 "++fracSep++" -123 "++""++" 5 ")]

parseTermsTG = tg "parseTerms tests"
  [parsesTC parseTerms good1 [expected1] 
  ,parsesTC parseTerms (good1++"+"++good2) [expected1, expected2] 
  ,parsesTC parseTerms (good1++"+"++good2++"-"++good3)
   [expected1, expected2, expected3negated] 
  ,descParsescTC "'+-' should parse like '-'" 
   parseTerms (good1++"-"++good3++"+"++good2)
   [expected1, expected3negated, expected2] 
  ,parsesLikeTC parseTerms (good1++"-"++good3++"+"++good2)
   (good1++"-"++good3++good2)
  ,descFailParseTC "Terms beginning with a bad expression should not parse"
   parseTerms bad1 
  ,failParseTC parseTerms (bad1++"+"++good2) 
  ,descFailParseTC "Terms ending with a bad expression should not parse"
   parseTerms (good1++"+"++bad1) 
  ,descFailParseTC
   "Terms with a bad expression in the middle should not parse"
   parseTerms (good1++"+"++bad1++"-"++good3) 
  ,failParseTC parseTerms (good1++"-"++good2) 
  ,failParseTC parseTerms (fst repeatedIndex++" + "++snd repeatedIndex)]
  where
    good1 = ("91 / 3"++ciSep++"0")
    expected1 = (DegreeOneMonomial (91 % 3) 0)
    good2 = ("-2 / 7"++ciSep++"751")
    expected2 = (DegreeOneMonomial (-2 % 7) 751)
    good3 = ("5 / -3333"++ciSep++"981")
    expected3negated = (DegreeOneMonomial (5 % 3333) 981)
    bad1 =  ("91 / 3"++ciSep++"-1")
    repeatedIndex = (("91 / 3"++ciSep++"0"),
                     ("1 / 7"++ciSep++"0"))
    
parseLinearInequalityTG = tg "parseLinearInequality"
--Intentially not using fracSep for readability
  [parsesTC parseLinearInequality
   (dom1str++"+"++dom2str++" <= 343/2")
   LinearInequality { lhs = [dom1, dom2],
                      sense = LesserOrEqual,
                      rhs = 343 % 2}
   ,parsesTC parseLinearInequality
   (dom3str++"-"++dom4str++" <= 23432894723894723982342332239999")
   LinearInequality { lhs = [dom3, dom4neg],
                      sense = LesserOrEqual,
                      rhs = 23432894723894723982342332239999}]
  where
    --dom = degree one monomial
    dom1 = makeDegreeOneMonomial 1 1 0
    dom1str = "1"++fracSep++"1"++ciSep++"0"
    dom2 = makeDegreeOneMonomial (-5) 7 1    
    dom2str = "-5"++fracSep++"7"++ciSep ++"1"
    dom3 = makeDegreeOneMonomial (-5) 1 2    
    dom3str = "-5"++ciSep ++"2"
    dom4 = makeDegreeOneMonomial 23423432432 1 3    
    dom4neg = DegreeOneMonomial {coefficient = -(coefficient dom4),
                                 variableIndex = variableIndex dom4}
    dom4str = "23423432432"++ciSep ++"3"    


parseLinearInequalityLinesTG = tg "parseLinearInequalityLines"
  [descParsescTC "Separated and ended by by ; should parse as two LinearInequalities"
   parseLinearInequalityLines twoInequalitiesInput twoInequalitiesExpected,
   descFailParseTC "Separated and ended by by \\n should fail to parse."
   parseLinearInequalityLines [if x == ';' then '\n' else x | x <- twoInequalitiesInput],
   descFailParseTC "No inequalities should fail to parse."
   parseLinearInequalityLines "asd"
  ]
  where
    twoInequalitiesInput = "10/9 x_1<= 1/2 ;-7 x_2 = 5;"
    twoInequalitiesExpected = [
      LinearInequality { lhs = [makeDegreeOneMonomial 10 9 1],
                         sense = LesserOrEqual,
                         rhs = 1 % 2},
      LinearInequality { lhs = [makeDegreeOneMonomial (-7) 1 2],
                         sense = Equal,
                         rhs = 5 % 1}]
