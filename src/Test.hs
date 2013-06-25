{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Test.Data.SetMultiMap



main = htfMain $ htf_thisModulesTests : htf_importedTests
