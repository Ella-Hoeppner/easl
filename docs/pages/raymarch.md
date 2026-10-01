# Walkthrough: Ray marching

## Basics

Consider a simple raymarching function written in a traditional shader language, like glsl:

```glsl
vec3 raymarch(vec3 rayOrigin, vec3 rayDirection) {
	float offset=0.;
  for(int i = 0; i < 256; i++) {
    vec3 pos = rayOrigin + rayDirection*offset;
    float dist = sdf(pos);
    offset += dist;
    if (dist > 100.) break;
    if (dist < 0.001) return rayOrigin + rayDirection*offset;
  }
  return vec3(0.);
}
```

This function takes in a ray origin and a ray direction, and marches the ray forward according to the `sdf` signed distance function. When it reaches the surface it returns the point on the surface that it terminated at, and if it can't reach the surface within 256 iteration then it instead returns `vec3(0.)` as a 

This can be straightforwardly translated to easl:

```easl
(defn raymarch [ray-origin: vec3f ray-direction: vec3f]: vec3f
  (let [@var offset 0.]
    (for [256u]
      (let [pos (+ ray-origin (* ray-direction offset))
            dist (sdf pos)]
        (+= offset dist)
        (when (> dist 100.)
          (break))
        (when (< dist 0.001)
          (return (+ ray-origin
                     (* ray-direction offset)))))))
  (vec3f 0.))
```

Now that the code is in easl, there are some quick improvements we can make using easl's unique features.

For one, rather than hard-coding the `sdf` function into the `raymarch` function body, we can simply take `sdf` as a function-typed argument. That way, this same `raymarch` function can be immediately used on any new signed distance function that we defined, whereas in glsl, without support for higher order functions, we'd just have to create a new copy of the `raymarch` function with a different function inlined into the body if we wanted to be able to raymarch over multiple different distance fields in the same program.

The other improvement is that we can return an `(Option vec3f)` rather than a bare `vec3f`. An `Option` is an enum that has two variants, `None` which carries no value, and `(Some ...)` which carries a value inside, so it makes a convenient representation as the return type for a function that may or may not converge to a proper value. This way, rather than just returning a sentinel value like `(vec3f 0.)` that all consumers are supposed to remember to treat as "the function didn't converge", we can simply return `None` at the end of the function, to be more explicit and force all consumers to explicitly handle this possibility.

With those changes made, the code now looks like this:

```easl
(defn raymarch [sdf: (Fn [vec3f] f32)
                ray-origin: vec3f
                ray-direction: vec3f]: (Option vec3f)
  (let [@var offset 0.]
    (for [256u]
      (let [pos (+ ray-origin (* ray-direction offset))
            dist (sdf pos)]
        (+= offset dist)
        (when (> dist 100.)
          (break))
        (when (< dist 0.001) 
          (return (Some (+ ray-origin 
                           (* ray-direction offset))))))))
  None)
```

To put this into action, let's write a quick little `main` function to manage a window and dispatch some shaders:

```easl
@cpu
(defn main []
  (spawn-window (fn []
                  (dispatch-render-shaders vertex fragment 3))))
```

The `@cpu` annotation above `main` indicates that this function is a CPU-side entry point, so that the compiler will know to start our program with this function.

This `main` function calls `spawn-window`, easl's built-in function for creating windows. This function takes a callback function that will be used as an update loop, telling the window what to do each frame. The callback function we pass here, the outermost `(fn [] ...)` expression, is simply a zero-argument function that calls `dispatch-render-shaders` inside. The `dispatch-render-shaders` function is the basis for all rendering in easl, and it takes three arguments: a vertex shader, a fragment shader, and a number of vertices to draw.

The first two arguments we pass here are `vertex` and `fragment`, which we haven't defined yet but we'll get to in a moment. The third argument is `3`, which indicates that we want to draw three vertices, a single triangle. Since we'll be doing all of our rendering with a raymarcher, we don't need any kind of complicated triangle geometry, so a single big triangle covering the whole screen will be enough for our purposes.

Now to the definitions of the actual vertex and fragment shaders:

```easl
(defn vertex []: vec4f
  (vec4f (match (vertex-index)
           0 (vec2f -1.)
           1 (vec2f -1. 3.)
           _ (vec2f 3. -1.))
         0.
         1.))

(defn fragment []: vec4f
  (let [resolution (vec2f (window-resolution))
        pos (-> (.xy (position))
                (/ resolution)
                (* 2.)
                (- 1.)
                (* (/ resolution
                      (f32 (min resolution.x resolution.y)))))]
    (vec4f pos 0. 1.)))
```

The `vertex` function calls `vertex-index`, a built-in function that returns the index of the current vertex, and uses a `match` block to position the corners of the triangle differently for each vertex. The `fragment` function uses the `window-resolution` function to get the current resolution of the window, and then computes `pos` (using  easl's [threading syntax](language.md#the-threading-operator--)) as a normalized position value based on the pixel coordinates that are looked up using the `position` function. If you run the code yourself at this point, you'll see a window with the four quadrants colored in a simple black-red-yellow-green pattern.

To start using our raymarcher for rendering, we'll need a few more things; an SDF (signed distance function) to march against, and a way to compute the gradient of the SDF, so that we can use the gradient information to shade the surface.

```easl
(defn sphere [pos: vec3f] f32
  (- (length pos) 1.))

(defn gradient [f: (Fn [vec3f] f32)
                pos: vec3f]: vec3f
  (let [eps 0.001]
    (/ (- (vec3f (f (+ pos (vec3f eps 0. 0.)))
                 (f (+ pos (vec3f 0. eps 0.)))
                 (f (+ pos (vec3f 0. 0. eps))))
          (f pos))
       eps)))
```

`sphere` here is an extremely simple SDF describing a sphere centered at the origin with radius 1. The `gradient` function here is another higher-order function, like the `raymarch` function itself, which can accept any arbitrary function (such `sphere`) as an input. With all of these pieces created, let's modify the `fragment` function to use our raymarcher for rendering:

```easl
(defn fragment []: vec4f
  (let [resolution (vec2f (window-resolution))
        pos (-> (.xy (position))
                (/ resolution)
                (* 2.)
                (- 1.)
                (* (/ resolution
                      (f32 (min resolution.x resolution.y)))))
        camera-origin (vec3f 0. 0. -2.)
        camera-direction (normalize (vec3f pos 1.))
        raymarch-result (raymarch sphere
                                  camera-origin
                                  camera-direction)]
    (vec4f (match raymarch-result
             (Some surface-pos) (let [grad (gradient sphere surface-pos)]
                                  (* 0.5 (+ 1. grad)))
             None (vec3f 0.))
           1.)))
```

Now `fragment` uses the normalized screen position to determine the direction of a ray, then calls `raymarch` to find the surface in the direction of the ray. `raymarch-result` is an `(Option vec3f)`, so to compute the final color we `match` on it. In the `(Some surface-pos)` branch, we return a color given by the gradient (renormalized from [-1, 1] to [0, 1]), while in the `None` branch we simply return black, `(vec3f 0.)`.

Here's the full current program, which clocks in around 60 lines. Try to run it for yourself with `easl run my_file.program` - you should see a simple sphere, with some colorful shading.

```easl
(defn raymarch [sdf: (Fn [vec3f] f32)
                ray-origin: vec3f
                ray-direction: vec3f]: (Option vec3f)
  (let [@var offset 0.]
    (for [256u]
      (let [pos (+ ray-origin
                   (* ray-direction offset))
            dist (sdf pos)]
        (+= offset dist)
        (when (< dist 0.001)
          (return (Some (+ ray-origin
                           (* ray-direction offset))))))))
  None)

(defn gradient [f: (Fn [vec3f] f32)
                pos: vec3f]: vec3f
  (let [eps 0.001]
    (/ (- (vec3f (f (+ pos (vec3f eps 0. 0.)))
                 (f (+ pos (vec3f 0. eps 0.)))
                 (f (+ pos (vec3f 0. 0. eps))))
          (f pos))
       eps)))

(defn sphere [pos: vec3f]: f32
  (- (length pos) 1.))

(defn vertex []: vec4f
  (vec4f (match (vertex-index)
           0 (vec2f -1.)
           1 (vec2f -1. 3.)
           _ (vec2f 3. -1.))
         0.
         1.))

(defn fragment []: vec4f
  (let [resolution (vec2f (window-resolution))
        pos (-> (.xy (position))
                (/ resolution)
                (* 2.)
                (- 1.)
                (* (/ resolution
                      (f32 (min resolution.x resolution.y)))))
        camera-origin (vec3f 0. 0. -2.)
        camera-direction (normalize (vec3f pos 1.))
        raymarch-result (raymarch sphere
                                  camera-origin
                                  camera-direction)]
    (vec4f (match raymarch-result
             (Some surface-pos) (let [grad (gradient sphere surface-pos)]
                                  (* 0.5 (+ 1. grad)))
             None (vec3f 0.))
           1.)))

@cpu
(defn main []
  (spawn-window (fn []
                  (dispatch-render-shaders vertex fragment 3))))
```

## Abstraction

Right now our `raymarch` and `gradient` functions have both been expressed as higher-order functions, so that they can avoid hard-coding in any particular SDF. Our `fragment` function, however, still just uses `sphere` directly, so if we ever wanted to change which sdf we're using we'd have to reach into it's internals. Let's fix that, by letting `fragment` abstract over the SDF that it uses, too:

```easl
(defn fragment [sdf: (Fn [vec3f] f32)]: vec4f
  (let [resolution (vec2f (window-resolution))
        pos (-> (.xy (position))
                (/ resolution)
                (* 2.)
                (- 1.)
                (* (/ resolution
                      (f32 (min resolution.x resolution.y)))))
        camera-origin (vec3f 0. 0. -2.)
        camera-direction (normalize (vec3f pos 1.))
        raymarch-result (raymarch sdf
                                  camera-origin
                                  camera-direction)]
    (vec4f (match raymarch-result
             (Some surface-pos) (let [grad (gradient sdf surface-pos)]
                                  (* 0.5 (+ 1. grad)))
             None (vec3f 0.))
           1.)))

@cpu
(defn main []
  (spawn-window (fn []
                  (dispatch-render-shaders
                    vertex
                    (fn [] (fragment sphere))
                    3))))
```

Notice that we had to change the way we use `fragment` inside `main` now. The fragment shader function passed to `dispatch-render-shaders` is expected to be a zero-argument function (with some caveats, see [shaders](shaders.md) for details), but our `fragment` function no longer has zero arguments, since it now accepts `sdf` as an input. So instead of passing `fragment` directly to `dispatch-render-shaders`, we instead use `(fn [] ...)` to declare a local, zero-argument function that internally calls `fragment` with `sphere` as an argument.

This call to `dispatch-render-shaders` in `main` function is getting kinda ugly now, so let's introduce one more helper function to simplify things:

```easl
(defn render-sdf [sdf: (Fn [vec3f] f32)]
  (dispatch-render-shaders
    vertex
    (fn [] (fragment sdf))
    3))

@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf sphere))))
```

`render-sdf` now hides all of the implementation complexity of the specific shaders involved in the raymarching process, and gives us a very simple tool to use from the CPU side of things in `main`: we give it an `sdf` function, and it sends that SDF to the GPU and displays the results on screen for us.

This demonstrates an important point about easl's design philosophy: Functions can cross the CPU/GPU boundary easily, and can be used in both context. The `main` and `render-sdf` functions run on the CPU, but they pass around the `sphere` function to eventually be run on the GPU, and this works fine. In general, any easl function be called either on the CPU or GPU, unless it uses context-specific features (e.g. the `vertex-index` function only works inside vertex shaders, and any function that calls it can't be called from the CPU).

For another example of how this ability to send functions across the CPU/GPU divide can be useful, let's first make a small refactor to our `fragment` function:

```easl
(def camera-origin: vec3f
     (vec3f 0. 0. -2.))

(defn camera-direction-from-pixel-coords [pixel-coords: vec2f]: vec3f
  (let [resolution (vec2f (window-resolution))
        pos (-> pixel-coords
                (/ resolution)
                (* 2.)
                (- 1.)
                (* (/ resolution
                      (f32 (min resolution.x resolution.y)))))]
    (normalize (vec3f pos 1.))))

(defn fragment [sdf: (Fn [vec3f] f32)]: vec4f
  (let [camera-direction (camera-direction-from-pixel-coords (.xy (position)))
        raymarch-result (raymarch sdf
                                  camera-origin
                                  camera-direction)]
    (vec4f (match raymarch-result
             (Some surface-pos) (let [grad (gradient sdf surface-pos)]
                                  (* 0.5 (+ 1. grad)))
             None (vec3f 0.))
           1.)))
```

Here we've moved `camera-origin` into a global constant, and extracted the logic for calculating the ray direction into a helper function. Since this `camera-direction-from-pixel-coords` helper function doesn't rely on any GPU-specific functionality, we can call it directly from the CPU. We can even call `raymarch` on the CPU!

```easl
@cpu
(defn main []
  (spawn-window
    (fn []
      (when (mouse-just-down?)
        (print
          (raymarch sphere
                    camera-origin
                    (camera-direction-from-pixel-coords
                      (vec2f (mouse-coords))))))
      (render-sdf sphere))))
```

Here we've modified our `main` function to, whenever the user clicks (as measured by teh `mouse-just-down?` built-in function), print out the result of raymarching in the direction of the mouse. The results of the `raymarch` function called on the CPU like this will be exactly the same as if you called them on the GPU (up to hardware-level differences in the floating point logic, at least). This kind of thing can be *extremely* useful for easily debugging shader programs in ways that are normally challenging.

## Higher-order helpers for SDFs

Now that we've established a solid baseline program for rendering SDFs, let's start to explore more complex kinds of distance functions.

As shown earlier, easl makes it easy to define a function locally using the `(fn [...] ...)` syntax. So if we want to render a different sdf, we don't necessarily need to declare it as a top-level function, we can also just define it in-place where we call `render-sdf`, for instance:

```easl
@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (fn [pos]
                                (sphere (- pos (vec3f 0.5 0. 0.))))))))
```

Here our local function takes an argument called `pos`, and simply calls the existing `sphere` function on that `pos` argument, but with a vector subtracted to it first, which effectively translates the sphere 0.5 units to the right. We can also build a general-purpose helper function that will let us translate any SDF in this same way:

```easl
(defn translate [sdf: (Fn [vec3f] f32)
                 translation: vec3f]: (Fn [vec3f] f32)
  (fn [pos]
    (sdf (- pos translation))))
```

Like many of the earlier functions we wrote, this function takes a function as an argument, but it also returns a new function as a return value. This can be used to simplify our `main` function now:

```
@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (translate sphere (vec3f 0.5 0. 0.))))))
```

Now let's implement another simple SDF, one for a box with arbitrary side lengths, so that we can construct more complicated scenes:

```
(defn box [pos: vec3f
           size: vec3f]: f32
  (let [q (- (abs pos) size)]
    (+ (length (max q (vec3f 0.)))
       (min (max q.x (max q.y q.z)) 0.))))
```

When you want to render more than one object in a raymarching scene, the usual tool is to render the *union* of two SDFs. The union is computed by taking the minimum of the two sdf values. Here's how you might render a shape compose of a sphere and a 

```easl
@cpu
(defn main []
  (spawn-window (fn []
                  (let [shape-a sphere
                        shape-b (fn [pos]
                                  (box pos (vec3f 0.7)))]
                    (render-sdf (fn [pos]
                                  (min (shape-a pos) (shape-b pos))))))))
```

However, for the sake of brevity, it would be nice to capture this concept of "unioning" two SDFs in another higher-order function:

```easl
(defn union [sdf-a: (Fn [vec3f] f32)
             sdf-b: (Fn [vec3f] f32)]: (Fn [vec3f] f32)
  (fn [pos]
    (min (sdf-a pos) (sdf-b pos))))
```

Then the previous example could be simplified to:

```easl
@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (union sphere
                                     (fn [pos]
                                       (box pos (vec3f 0.7))))))))
```

Right now our box is stuck in a fixed orientation, with one side facing directly towards the camera. To fix that, let's add a quick helper function for construction rotation matrices:

```easl
(defn axis-angle-rotation [axis: vec3f
                           angle: f32]: mat3x3f
  (let [a (normalize axis)
        c (cos angle)
        s (sin angle)
        t (- 1. c)]
    (mat3x3f (+ (* t a.x a.x) c)
             (+ (* t a.x a.y) (* s a.z))
             (- (* t a.x a.z) (* s a.y))
             (- (* t a.x a.y) (* s a.z))
             (+ (* t a.y a.y) c)
             (+ (* t a.y a.z) (* s a.x))
             (+ (* t a.x a.z) (* s a.y))
             (- (* t a.y a.z) (* s a.x))
             (+ (* t a.z a.z) c))))
```

This takes an axis and an angle describing a rotation, and returns a `mat3x3f` that can be used as a rotation matrix. With this, we can now rotate our whole scene, and we can even make it so that the angle changes with time using the `window-time` function:

```easl
@cpu
(defn main []
  (spawn-window (fn []
                  (let [scene (union sphere
                                     (fn [pos]
                                       (box pos (vec3f 0.7))))
                        rotation (axis-angle-rotation (vec3f 1.) (window-time))]
                    (render-sdf (fn [pos]
                                  (scene (* pos rotation))))))))
```

If you run the code at this point, you'll now see the whole sphere-cube union shape slowly rotating over time.

Still, having to manually define a new function abstract that abstracts over `pos` just to apply a rotation is less convenient than it could be, so let's define another helper function to make this code cleaner:

```easl
(defn rotate [sdf: (Fn [vec3f] f32)
              rotation: mat3x3f]: (Fn [vec3f] f32)
  (fn [pos]
    (sdf (* pos rotation))))

@cpu
(defn main []
  (spawn-window
    (fn []
      (render-sdf (rotate (union sphere
                                 (fn [pos]
                                   (box pos (vec3f 0.7))))
                          (axis-angle-rotation (vec3f 1.) (window-time)))))))
```

As a further convenience, we can define an overload of `rotate` that accepts the axis and angle arguments directly, so that the we don't have to call `axis-angle-rotation` in `main` at all:

```easl
(defn rotate [sdf: (Fn [vec3f] f32)
              rotation: mat3x3f]: (Fn [vec3f] f32)
  (fn [pos]
    (sdf (* pos rotation))))

(defn rotate [sdf: (Fn [vec3f] f32)
              axis: vec3f
              angle: f32]: (Fn [vec3f] f32)
  (let [rotation (axis-angle-rotation axis angle)]
    (fn [pos]
      (sdf (* pos rotation)))))

@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (rotate (union sphere
                                             (fn [pos]
                                               (box pos (vec3f 0.7))))
                                      (vec3f 1.)
                                      (window-time))))))
```

It's worth noting here that this second `rotate` definition has an interesting property that you might not notice if you don't look carefully: Since the local `rotation` binding that it declares is outside of the `fn` that it defines, the value of the rotation matrix will only be computed once, and each different invocation of the returned function will just use that pre-computed value. That much is standard for any language with closures, but the unique thing happening here is that this the calculation of the `rotation` matrix *happens on the CPU*, whereas the evaluation of the returned function *happens on the GPU*, in the fragment shader. The `rotate` function, after all, is being called directly on the CPU inside `main`, so any computation that it does will happen on the CPU.

Calculating a rotation matrix on the CPU before your shader runs like this is, of course, good practice, since it would be wasteful to re-calculate a shared value independently in each pixel. In any traditional graphics-programming framework, to accomplish this CPU/GPU computational split you'd have to declare a global binding site in your shader code and then, in a separate CPU-side "host" language, write some code to connect to locate that binding and explictly assign a value to it before dispatching your shaders. But because a singular easl program can target both the CPU and the GPU, and because the type system and semantics are shared between both sides of that divide, we can simply have a value like this calculated in a local binding of a helper function, and everything will work seamlessly without any explicit binding annotations. The easl compiler keeps track of all of the different pieces of data that need to cross the CPU/GPU threshold, and manages all of the data transfer logic automatically, so things just work.

We've now built a fairly nice, declarative-looking set of functions that we can use to combine and manipulate primitve SDFs into more complex shapes. However, one section still leaves a bit to be desired. In our call to `union`, we pass two shapes, one of which is simply `sphere`, while the other is this ugly-looking `(fn [pos] (box pos (vec3f 0.7)))` function. All that this represents is a box centered at the origin with a size `0.7` on each side, but the way we've written it obscures that slightly, since we have to declare a whole new explicit function to abstract over `pos`. Thankfully, we can add a new overload to the `box` function to make this look cleaner:

```easl
(defn box [pos: vec3f
           size: vec3f]: f32
  (let [q (- (abs pos) size)]
    (+ (length (max q (vec3f 0.)))
       (min (max q.x (max q.y q.z)) 0.))))

(defn box [size: vec3f]: (Fn [vec3f] f32)
  (fn [pos]
    (box pos size)))
```

The first overload is the same as before, while this second overload only takes a single argument, the size of the box. But the return type of this second overload is different; it returns a function rather than an `f32` like the first overload. This second overload is not, itself a signed distance function, but instead it is a function that *returns* signed distance functions, specifically SDFs of boxes with the size argument inlined. Notably it also invokes the first overload inside it's own body, to avoid needlessly duplicating the shared logic. This might look recursion, but it isn't, it's simply two different functions that happen to share the same name. The typechecker fully supports overloaded functions, and will decide which one to use based on the types you provide.

With this new overload, we can now simplify `main` in a way that fully avoids having to use an explicit `fn` anywhere:

```
@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (rotate (union sphere
                                             (box (vec3f 0.7)))
                                      (vec3f 1.)
                                      (window-time))))))
```

Our `sphere` and `box` functions are a bit inconsistent here though, in that the `box` function allows you to construct a box of any size, while the `sphere` function just assumes a radius of 1. Let's fix that:

```
(defn sphere [pos: vec3f
              radius: f32]: f32
  (- (length pos) radius))

(defn sphere [radius: f32]: (Fn [vec3f] f32)
  (fn [pos] (sphere pos radius)))

(defn sphere [pos: vec3f]: f32
  (sphere pos 1.))
```

Now we have a base `sphere` overload that accepts an explicit radius argument, and a second overload that just accepts the radius and returns an SDF, just like the `box` overload. But we also add a third overload, which simply takes a `pos` and no `radius`, and assumes a radius of 1, like before. With these three overloads present, our old `main` will still compile fine, since the third `sphere` overload has the same signature it expected before. But now it's easy to change the radius of the sphere by simply calling `sphere` as a fn and passing in the radius, e.g.:

```
@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (rotate (union (sphere 0.9)
                                             (box (vec3f 0.7)))
                                      (vec3f 1.)
                                      (window-time))))))
```

## Final features

So far we've just been coloring all of our geometry directly with the gradient, which is fine for debugging, but not what you'd want in a practical raymarching engine. Let's fix that:

```
(defn fragment [sdf: (Fn [vec3f] f32)
                surface-color: (Fn [vec3f vec3f vec3f] vec3f)]: vec4f
  (let [camera-direction (camera-direction-from-pixel-coords (.xy (position)))
        raymarch-result (raymarch sdf
                                  camera-origin
                                  camera-direction)]
    (vec4f (match raymarch-result
             (Some surface-pos) (surface-color surface-pos
                                               camera-direction
                                               (gradient sdf surface-pos))
             None (vec3f 0.))
           1.)))

(defn render-sdf [sdf: (Fn [vec3f] f32)
                  surface-color: (Fn [vec3f vec3f vec3f] vec3f)]
  (dispatch-render-shaders
    vertex
    (fn []
      (fragment sdf surface-color))
    3))

(defn render-sdf [sdf: (Fn [vec3f] f32)]
  (dispatch-render-shaders
    vertex
    (fn []
      (fragment sdf
                (fn [pos view-direction normal]
                  (* 0.5 (+ 1. normal)))))
    3))
```

Now `fragment` and `render-sdf` each take a new argument, `surface-color`, which is a function that is used to choose which color to display once a ray has hit the surface. The passed function needs to take three `vec3f` arguments, representing the surface position, view direction, and surface normal, respectively, all of which can be useful for different kinds of lighting calculations. `render-sdf` also has an overload that passes the simple gradient-based shading that we used before, so our old `main` function that didn't pass any value for this `surface-color` argument remains valid. But now we can modify our `main` to pass in arbitrary surface-coloring logic:

```
@cpu
(defn main []
  (spawn-window
    (fn []
      (render-sdf (rotate (union (sphere 0.9)
                                 (box (vec3f 0.7)))
                          (vec3f 1.)
                          (window-time))
                  (let [light-pos (vec3f 2. 2. -5.)
                        surface-color (vec3f 1. 0.5 0.5)]
                    (fn [surface-pos
                         view-direction
                         surface-normal]
                      (let [light-dir (normalize (- light-pos surface-pos))
                            halfway-dir (normalize
                                          (+ light-dir (- view-direction)))]
                        (* surface-color
                           (+ (* 0.2
                                     (max 0.
                                          (dot surface-normal light-dir)))
                                  (pow (max 0.
                                            (dot surface-normal halfway-dir))
                                       50.))))))))))
```

This example implements the simple [blinn-phong](https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_reflection_model) lighting model. To simplify our `main`, we could again abstract this out to a helper function:

```
(defn blinn-phong [surface-pos: vec3f
                   view-direction: vec3f
                   surface-normal: vec3f
                   color: vec3f
                   light-pos: vec3f
                   diffuse-factor: f32
                   specular-power: f32]: vec3f
  (let [light-dir (normalize (- light-pos surface-pos))
        halfway-dir (normalize (+ light-dir (- view-direction)))]
    (* color
       (+ (* diffuse-factor
             (max 0.
                  (dot surface-normal light-dir)))
          (pow (max 0.
                    (dot surface-normal halfway-dir))
               specular-power)))))

(defn blinn-phong [color: vec3f
                   light-pos: vec3f
                   diffuse-factor: f32
                   specular-power: f32]: (Fn [vec3f vec3f vec3f] vec3f)
  (fn [surface-pos
       view-direction
       surface-normal]
    (blinn-phong surface-pos
                 view-direction
                 surface-normal
                 color
                 light-pos
                 diffuse-factor
                 specular-power)))

@cpu
(defn main []
  (spawn-window (fn []
                  (render-sdf (rotate (union (sphere 0.9)
                                             (box (vec3f 0.7)))
                                      (vec3f 1.)
                                      (window-time))
                              (blinn-phong (vec3f 1. 0.5 0.5)
                                           (vec3f 2. 2. -5.)
                                           0.2
                                           50.)))))
```

There's one signficant problem with the approach we've taken so far, though. While we can render as many different objects as we want using the `union` function, every object has to be the exact same color, since we control the color globally through the a single argument that we pass to `blinn-phong`. What if we wanted different shapes to have different colors, or better yet, entirely different styles of lighting?

Here's one way that we could accomplish that:

```
(struct Shape
  sdf: (Fn [vec3f] f32)
  material: (Fn [vec3f vec3f vec3f] vec3f))

@cpu
(defn main []
  (spawn-window (fn []
                  (let [shapes [(Shape (sphere 0.9)
                                       (blinn-phong (vec3f 1. 0.5 0.5)
                                                    (vec3f 2. 2. -5.)
                                                    0.2
                                                    50.))
                                (Shape (rotate (box (vec3f 0.7))
                                               (vec3f 1.)
                                               (window-time))
                                       (blinn-phong (vec3f 0.5 1. 0.5)
                                                    (vec3f 2. 2. -5.)
                                                    0.2
                                                    50.))]]
                    (render-sdf (fn [pos]
                                  (let [@var closest-dist 1000000.]
                                    (for [i (array-length shapes)]
                                      (= closest-dist
                                         (min closest-dist
                                              ((.sdf (shapes i)) pos))))
                                    closest-dist))
                                (fn [surface-pos
                                     view-direction
                                     surface-normal]
                                  (let [@var closest-dist 1000000.
                                        @var closest-index 0u]
                                    (for [i (array-length shapes)]
                                      (= closest-dist
                                         (min closest-dist
                                              ((.sdf (shapes i)) surface-pos))))
                                    ((.material (shapes closest-index))
                                     surface-pos
                                     view-direction
                                     surface-normal))))))))
```

