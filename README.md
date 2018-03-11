# H-Ray
Extensible multi-threaded raytracer in Haskell.

# Architecture overview
The architecture is inpired by NVIDIA's [OptiX](https://developer.nvidia.com/optix) raytracer.

Functional programming style amazingly fits the core concept of raytracing: cast individual independent rays, mutate the data using arbitrary functions and return the result. So let's take advantage of it!

## Behaviour
The raytracer consists of several different kinds of functions:

**Initial Ray Generation**    
Starts the whole raytracing process. Returned rays are traced against the scene one by one.

**Intersection**    
Checks if a ray intersects the geometry and computes an intersection point and normal.

**Bounding Volume**    
For a given geometry returns a container that encloses it.    
Examples: [AABB](https://en.wikipedia.org/wiki/AABB), [Bounding Sphere](https://en.wikipedia.org/wiki/Bounding_sphere)

**Closest Hit (a.k.a. Shading)**    
Takes the first intersection along the ray and returns one of the following:    
- Color    
- A set of new rays to compute and a function to assemble the color once they are computed

**Any Hit (a.k.a. Masking)**    
Takes the first intersection along the ray and returns one of the following:    
- Color    
- Nothing    
Can be used for visibility checks.

## Data
The raytracer is executed on a scene with context.

### Scene
The scene is represented as a set of separate objects with functions assigned to them:
- Bounding Volume
- Intersection
- Closest Hit
- Any Hit (optional)

### Context
Context holds different raytracer parameters such as:

- Physical lights
- Ambient lighting
- Setting specifying whether to use AnyHit / ClosestHit / both
- ...

Each time a set of rays is created, a context is assigned to them and passed to all the consequent functions.
