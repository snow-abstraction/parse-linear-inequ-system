--          Copyright Douglas W. Potter 2014.
-- Distributed under the Boost Software License, Version 1.0.
--    (See accompanying file LICENSE_1_0.txt or copy at
--          http://www.boost.org/LICENSE_1_0.txt)
module ArbitraryLinearSystem
       where
import LinearSystem
import Test.QuickCheck
import Data.Word (Word)

instance Arbitrary DegreeOneMonomial where
  arbitrary = do
    c <- arbitrary :: Gen Rational
    i <- arbitrary :: Gen Word
    return $ DegreeOneMonomial {
      coefficient = c,
      variableIndex = i}
