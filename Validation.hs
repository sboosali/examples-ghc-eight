{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| The Validation Applicative

Run with:

@
stack build examples-ghc-eight:exe:example-validation && stack exec -- example-validation
@

'Either' is a Monad, and short-circuits on the first failure.

'Validation' is an Applicative but not a Monad, and thus can collect all failures, not just the first. 

'readPoint_Either_bad' can only provide the first error, while 'readPoint_Validation_bad_both' provides all errors (i.e. both the first and second). 

-}
module Validation where

import Text.Read (readEither)

import Data.Either.Validation

readValidation :: Read a => String -> Validation [String] a
readValidation s = case (readEither s) of -- eitherToValidation
  Left e  -> Failure [e]
  Right a -> Success a

readPoint_Either_good     :: Either String (Int,Int)
readPoint_Either_good     = (,) <$> readEither "1"  <*> readEither "2"

readPoint_Either_bad      :: Either String (Int,Int)
readPoint_Either_bad      = (,) <$> readEither "one" <*> readEither "two"

readPoint_Validation_good      :: Validation [String] (Int,Int)
readPoint_Validation_good       = (,) <$> readValidation "1"   <*> readValidation "2"

readPoint_Validation_bad_both  :: Validation [String] (Int,Int)
readPoint_Validation_bad_both   = (,) <$> readValidation "one" <*> readValidation "two"

readPoint_Validation_bad_left  :: Validation [String] (Int,Int)
readPoint_Validation_bad_left   = (,) <$> readValidation "one" <*> readValidation "2"

readPoint_Validation_bad_right :: Validation [String] (Int,Int)
readPoint_Validation_bad_right  = (,) <$> readValidation "1"   <*> readValidation "two"

main = do
  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"  
  putStrLn "EITHER"
  
  putStrLn ""
  putStrLn $ "[readPoint_Either_good] ="
  print $ readPoint_Either_good
  
  putStrLn ""
  putStrLn $ "[readPoint_Either_bad] ="
  print $ readPoint_Either_bad
  
  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"  
  putStrLn "VALIDATION"
  
  putStrLn ""
  putStrLn $ "[readPoint_Validation_good] ="
  print $ readPoint_Validation_good
  
  putStrLn ""
  putStrLn $ "[readPoint_Validation_bad_both] ="
  print $ readPoint_Validation_bad_both 

  putStrLn ""
  putStrLn $ "[readPoint_Validation_bad_left] ="
  print $ readPoint_Validation_bad_left 

  putStrLn ""
  putStrLn $ "[readPoint_Validation_bad_right] ="
  print $ readPoint_Validation_bad_right 

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"  
