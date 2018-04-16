{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
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
    , Var
    , VarValue
    -- | Functions
    , getVar
    , raycast
    ) where


import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import Linear.V2 (V2)
import Linear.V3 (V3)

import qualified Data.HashMap.Lazy as HashMap hiding (HashMap)

newtype D2 = D2 (V2 Double)

newtype D3 = D3 (V3 Double)

newtype Color = Color D3

data Ray = Ray
    { origin    :: D3
    , direction :: D3
    }

newtype Intersection = Intersection D3

-- | Ray Result is either a Color
-- or a ray generator and a function to convert the ray results into Color
data RayResult
    = RayResult Color
    | RayResultAssembly [Ray]
                        ([RayResult] -> Color)

newtype Intersect = Intersect (Context -> Maybe Intersection)

-- | Called when the intersection point was the closest.
newtype ClosestHit = ClosestHit RayResult

-- | Variable defined in a context
newtype Var = Var String
    deriving (Eq, Hashable)

data VarValue =
      Vector1 Double
    | Vector2 D2
    | Vector3 D3


-- | Called when looking for any intersection point.
-- Returns a RayResult to stop loooking for intersection points or Nothing to keep looking.
newtype AnyHit = AnyHit (Maybe RayResult)

newtype Context = Context (HashMap Var VarValue)

-- | Is used to determine if an underlying SceneObject has a chance of intersecting with the ray.
class BoundingVolume b where
    intersect :: Intersect

-- | Axis Alligned Bounding Box.
data AABB = AABB
    { aabbMin :: D3
    , aabbMax :: D3
    }

-- | TODO: implement intesection with the bounding box
instance BoundingVolume AABB where
    intersect = error "Interesect is not implemented for AABB"

-- | Scene object is fully defined by it's behaviour and context.
data SceneObject =
    SceneObject Context
                Intersect
                ClosestHit
                AnyHit
                AABB -- | TODO: parametrize with a bounding volume type.

data Scene = Scene
    { objects :: [SceneObject] -- | TODO: implement an acceleration structure for the Scene Objects instead of a plain list
    , context :: Context
    }

-- | Get Variable from context hierarchy.
getVar :: [Context] -> Var -> Maybe VarValue
getVar [] _ = Nothing
getVar (Context context: contexts) key =
    case HashMap.lookup key context of
        Nothing -> getVar contexts key
        x       -> x

-- | TODO: implement raycasting function.
raycast :: Scene -> Ray -> RayResult
raycast = error "Raycast is not implemented for AABB"
