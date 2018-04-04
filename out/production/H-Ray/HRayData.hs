{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module HRayData
    ( AABB
    , AnyHit
    , BoundingVolume
    , ClosestHit
    , Color
    , Context
    , D2
    , D3
    , Intersect
    , Intersection
    , Ray
    , RayResult
    , Scene
    , SceneObject
    , raycast
    ) where

import Data.Map (Map, (!))
import Linear.V2 (V2)
import Linear.V3 (V3)

import qualified Data.Map as Map hiding (Map)
import qualified Linear.V2 as V2 hiding (V2)
import qualified Linear.V3 as V3 hiding (V3)

newtype D2 =
    D2 (V2 Double)

newtype D3 =
    D3 (V3 Double)

newtype Color =
    Color D3

data Ray = Ray
    { origin    :: D3
    , direction :: D3
    }

newtype Intersection =
    Intersection D3

-- | Ray Result is either a Color
-- or a ray generator and a function to convert the ray results into Color
data RayResult
    = RayResult Color
    | RayResultAssembly [Ray]
                        ([RayResult] -> Color)

newtype Intersect =
    Intersect (forall c. (Context c) =>
                             c -> Intersection)

-- | Called when the intersection point was the closest.
newtype ClosestHit =
    ClosestHit RayResult

-- | Called when looking for any intersection point.
-- Returns a RayResult to stop loooking for intersection points or Nothing to keep looking.
newtype AnyHit =
    AnyHit (Maybe RayResult)

-- | Is used to determine if an underlying SceneObject has a chance of intersecting with the ray.
class BoundingVolume b where
    intersect :: Intersect

-- | Scene object is fully defined by it's behaviour and context.
-- | TODO: add per-object context that looks up it's parent's values in case of a map miss.
data SceneObject =
    SceneObject Intersect
                ClosestHit
                AnyHit
                AABB -- | TODO: parametrize with a bounding volume type.

data Context = Context
    { objects :: [SceneObject] -- | TODO: implement an acceleration structure for the Scene Objects instead of a plain list
    , v1s     :: Map String Double
    , v2s     :: Map String D2
    , v3s     :: Map String D3
    }

getDouble :: [Context] -> String -> Maybe Double
getDouble [] key = Nothing
getDouble (Context {..}:scenes) key =
    case lookup key v1s of
        Nothing -> getDouble scenes key
        x       -> x

getVector2 :: [Context] -> String -> Maybe D2
getVector2 [] key = Nothing
getVector2 (Context {..}:scenes) key =
    case lookup key v1s of
        Nothing -> getVector2 scenes key
        x       -> x

getVector3 :: [Context] -> String -> Maybe D3
getVector3 [] key = Nothing
getVector3 (Context {..}:scenes) key =
    case lookup key v1s of
        Nothing -> getVector3 scenes key
        x       -> x

-- | Axis Alligned Bounding Box.
data AABB = AABB
    { min :: D3
    , max :: D3
    }

-- | TODO: implement intesection with the bounding box
instance BoundingVolume AABB where
    intersect = undefined

-- | TODO: implement raycasting function.
raycast :: Scene -> Ray -> RayResult
raycast = undefined
