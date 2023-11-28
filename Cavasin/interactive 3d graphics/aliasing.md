# Aliasing

Scan conversion is a sampling process, and as every sampling process, can introduce aliasing which results in visual artefacts in the final image.

## Screen-based antialiasing

The general strategy of screen-based antialiasing schemes is to use more samples per screen grid cell, blended in some fashion, to compute a better pixel color. This is achieved at rasterization stage by using a sampling pattern and a weighted sum to produce the pixel color:

$$
c(x,y)=\sum_{i=1}^nw_i\,s(i,x,y)
$$

Where $n$ is the number of samples being taken per-pixel, $s(i,x,y)$ is the color of the sample $i$ in the pixel at $(x,y)$ (the position of the sample depends on $i$). Most methods used in real-time rendering systems give a uniform weight to their samples $w_i=\frac1n$.

![sampling](img/sampling.svg)

### Supersampling (FSAA, SSAA)

Full-scene antialiasing (FSAA), also known as "supersampling antialiasing" (SSAA), renders the scene at a higher resolution and then filters neighboring pixels to create an image. This method is costly, as samples are in fact full-blown fragments and must be fully shaded and filled. FSAA's main advantage is simplicity.

NVIDIA's dynamic super resolution feature is a more elaborate form of supersampling, where the scene is rendered at some higher resolution and a 13-sample Gaussian filter is used to generate the displayed image.

### Multisampling (MSAA)

Multisampling antialiasing (MSAA) lessens the high computational costs by shading the fragments once per pixel. Pixels may have, say, four $(x,y)$ sample locations, each with their own color and z-depth, but the fragment shader is evaluated once and the result is written only on the samples covered by the primitive. The samples store their own $z$ because they are independently z-tested.

If the primitive covers all MSAA positional samples, the shading sample is evaluated at the center of the pixel. Otherwise, the shading sample's position can be shifted to better represent the positions covered. Doing so avoids shade sampling off the edge of a texture, for example. This position adjustment is called *centroid sampling* or *centroid interpolation*.

Once all geometry has been rendered to a multiple-sample buffer, a resolve operation is then performed. This procedure averages the sample colors together to determine the color for the pixel.

MSAA focuses effort on sampling the primitive's pixel coverage at a higher rate and sharing the computed shade.

## Post-processing antialiasing

The general strategy of screen-based antialiasing schemes is to use image manipulation algorithms to hide aliasing artifacts in the rendered image.

### FXAA

FXAA (Fast Approximate Anti-Aliasing) looks for differences in colours ad depth between neighbouring pixels. Significant differences are likely edges, and are smoothed out.
