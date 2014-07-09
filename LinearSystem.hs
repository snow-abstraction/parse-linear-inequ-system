--          Copyright Douglas W. Potter 2014.
-- Distributed under the Boost Software License, Version 1.0.
--    (See accompanying file LICENSE_1_0.txt or copy at
--          http://www.boost.org/LICENSE_1_0.txt)
module LinearSystem
  (LinearInequality(..),
   Sense(..), 
   DegreeOneMonomial(..),
   System,
   makeDegreeOneMonomial,
   fracSep,
   coefficientIndexSep)
  where
import Data.Ratio
  ((%),
   numerator,
   denominator)
import Data.Word
  (Word)

type System = [LinearInequality]

fracSep :: String
fracSep = "/"
coefficientIndexSep :: String
coefficientIndexSep = "x_"
              
data Sense = LesserOrEqual | Equal | GreaterOrEqual deriving (Show, Eq)

data LinearInequality = LinearInequality {
  lhs :: [DegreeOneMonomial],
  sense :: Sense,
  rhs :: Rational }
  deriving (Eq, Show)

-- could paramaterized the type of the index, i.e.
-- to allow arbitrarilyy many variables
data DegreeOneMonomial = DegreeOneMonomial {  
  coefficient :: Rational,  
  variableIndex :: Word }
  deriving (Eq)
                        
instance Show DegreeOneMonomial where
  show x =  (show $ numerator $ coefficient x) ++ fracSep ++
            (show $ denominator $ coefficient x) ++ coefficientIndexSep ++
            (show $ variableIndex x)

makeDegreeOneMonomial :: Integer -> Integer -> Word -> DegreeOneMonomial
makeDegreeOneMonomial n d index = DegreeOneMonomial (n % d) index
