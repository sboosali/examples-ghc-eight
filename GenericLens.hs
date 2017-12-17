{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-} 

{-| http://kcsongor.github.io/generic-lens/

>>> :set -XDuplicateRecordFields
>>> data Person = Person { name :: String, age :: Natural }
>>> data Dog    = Dog    { name :: String, isPitbull :: Bool }
>>> sameField @"name" (Person "Alex" 33) (Dog "Bruce" True)
False 

-}
module GenericLens where

import Control.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics
import Control.Monad.Reader 

data Point2D
  = Point2D
    { px :: Int
    , py :: Int
    } deriving (Generic, Show, Eq)

data Point3D
  = Point3D
    { px :: Int
    , py :: Int
    , pz :: Int
    } deriving (Generic, Show, Eq)

data Point
  = Point2D_ Point2D
  | Point3D_ Point3D
  deriving (Generic, Show, Eq)

scalePoint2D :: Int -> Point2D -> Point2D
scalePoint2D c (Point2D x y) = Point2D (x * c) (y * c)

pxM :: (MonadReader env m, HasField' "px" env String) => m String
pxM = view (field @"px")

sameX :: (Eq i, HasField' "px" a i, HasField' "px" b i) => a -> b -> Bool
sameX a b = (a ^. field @"px") == (b ^. field @"px")

-- -- the field type can be inferred from the field name
-- sameField
--   :: forall k a. forall s t. (HasField' k s a, HasField' k t a)
--   => s -> t -> Bool
-- sameField x y = (x ^. field @k) == (y ^. field @k)
{- ERROR 

GenericLens.hs:61:6: error:
    * Overlapping instances for HasField k0 s s a0 a0
      Matching givens (or their superclasses):
        (HasField' k s a, HasField' k t a)
          bound by the type signature for:
                     sameField :: forall (k :: ghc-prim-0.5.1.1:GHC.Types.Symbol) a s t.
                                  (HasField' k s a, HasField' k t a) =>
                                  s -> t -> Bool

-}

--------------------------------------------------------------------------------

{-

>>> Point2D 10 20 ^. field @"px"
10

>>> Point2D 10 20 ^. position @1 
10

>>> Point2D 10 20 ^. typed @Int
error:
    • The type Point2D contains multiple values of type Int.
      The choice of value is thus ambiguous. The offending constructors are:
      • Point2D

>>> Point3D 1 2 3 & super %~ scalePoint2D 11
Point3D {px = 11, py = 22, pz = 3}

>>> Point3D 1 2 3 ^. super :: Point2D
Point2D {px = 11, py = 22}

>>> Point2D 1 2 ^? _Ctor @"Point3D"
error:
    * The type Point2D does not contain a constructor named "Point3D"
   
-} 

main = do
  putStrLn ""
  print $ Point2D 10 20 ^. field @"px"
  print $ Point2D 10 20 ^. field @"px" == 10

  putStrLn ""
  print $ Point2D 10 20 ^. position @1 
  print $ Point2D 10 20 ^. position @1 == 10


  putStrLn ""
  print $ Point3D 1 2 3 & super %~ scalePoint2D 11
  print $ (Point3D 1 2 3 & super %~ scalePoint2D 11) == Point3D {px = 11, py = 22, pz = 3}

  putStrLn ""
  print $ (Point3D 1 2 3 ^. super :: Point2D)
  print $ (Point3D 1 2 3 ^. super :: Point2D) == Point2D {px = 1, py = 2}

  putStrLn ""
  print $ (Point2D 1 2 ^? _Ctor @"Point2D") 
  print $ (Point2D 1 2 ^? _Ctor @"Point2D") == Just (1,2)
  print $ Point2D_ (Point2D 1 2) ^? _Ctor @"Point2D_" 
  print $ Point2D_ (Point2D 1 2) ^? _Ctor @"Point2D_" == Just (Point2D 1 2)
  print $ Point2D_ (Point2D 1 2) ^? _Ctor @"Point3D_" 
  print $ Point2D_ (Point2D 1 2) ^? _Ctor @"Point3D_" == Nothing 

  putStrLn ""
  -- print $ sameField @"px" (Point2D 1 2) (Point3D 1 2 3) 
  print $ sameX (Point2D 1 2) (Point3D 1 2 3) 
