------------------------------------------------------------------------------
--- Some tests for AbstractCurry libraries.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "curry-check testAbstractCurry"
--- 
--- @author Michael Hanus
--- @version September 2017
------------------------------------------------------------------------------

import AbstractCurry.Files
import AbstractCurry.Pretty
import AbstractCurry.Types
import Directory
import Test.Prop

--- Test for equality of an AbstractCurry program with the same program
--- after pretty printing and reading this AbstractCurry program:
readAndTestEqualFcy :: String -> IO Bool
readAndTestEqualFcy mod = do
  prog1 <- readAbstractCurryStrict mod
  let modcurry = mod ++ ".curry"
      modbak   = mod ++ ".BAK"
  renameFile modcurry modbak
  copyFile modbak modcurry
  let modpp = mod ++ ".PP"
  readCurry mod >>= writeFile modpp . showCProg
  removeFile modcurry
  renameFile modpp modcurry
  prog2 <- readAbstractCurryStrict mod
  removeFile modcurry
  renameFile modbak modcurry
  let abstractequal = prog1 == prog2
  unless abstractequal $ do
    putStrLn $ unlines
      [ "Differences in programs occurred:"
      , "Original AbstractCurry program:", show prog1
      , "Pretty printed AbstractCurry program:", show prog2 ]
  return abstractequal

-- Strictly read a AbstractCurry program in order to avoid race conditions
-- due to copying/moving source files:
readAbstractCurryStrict :: String -> IO CurryProg
readAbstractCurryStrict mod = do
  prog <- readCurry mod
  id $!! prog `seq` return prog

testAbstractCurryPretty_rev =
  (readAndTestEqualFcy "Rev") `returns` True
                  
testAbstractCurryPretty_TestAbstractCurry =
  (readAndTestEqualFcy "Nat") `returns` True
