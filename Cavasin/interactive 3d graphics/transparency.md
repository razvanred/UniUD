# Transparency

There are many different ways in which semitransparent objects can allow light to pass through them, we will deal with the simplest form of transparency, in which the semitransparent object acts as an attenuator of the colors of the objects behind it.

## Screen-door transparency

The transparent triangles are rendered with a pixel-aligned checkerboard bit mask, so that every other pixel is rendered and objects behind are partially visible. The 50% checkerboard is limiting, only one transparent object can be convincingly rendered on one area of the screen.

## Alpha blending transparency

Most transparency algorithms blend the transparent object's color with the color of the object behind it. A pixel's alpha can represent either opacity, coverage, [or both](https://jcgt.org/published/0004/02/03/), depending on the circumstances. To make an object appear transparent, it is rendered on top of the existing scene with an alpha of less than $1$.

### The *over* operator

Blending a *source* color $c_s$ on top of a *destination* color $c_d$ is done using the *over* operator:

$$
c_o=\alpha_sc_s+(1-\alpha_s)c_d
$$

The alpha value models how much the material covers the pixel, and the over operator simulates the real-world effect of a gauzy fabric, as the screen-door transparency. The over operator doesn't convincing simulate other transparent effects, such as viewing through colored glass or plastic:

![transparency types](img/transparency%20types.png)

The equation can also be modified so that blending front-to-back gives the same result (*under* operator), but only without opaque objects rendered first. All transparent objects are drawn to a separate color buffer with *under*, then this color buffer is merged atop the opaque view of the scene using *over*.

Both blending operators are order-dependent.

### Issues

The GPU renders fragments in the order given, without sorting, and this can produce serious blending artifacts. The correct result is guaranteed with fragment-level depth sorting, but it can be prohibitively expensive. Approximations are possible, for example, object-level sorting is enough as long as there are no compenetrating primitives. Turning off culling can further reduce artifacts.

## Depth peeling

Depth peeling is an order-independent, multiple-pass, transparency (OIT) algorithm, that is, a transparency rendering method which doesn't require explicit sorting, but performs multiple rasterization passes to produce an image. The basic idea is to use an "implicit sort" to extract (*peel*) a depth layer at each pass, starting from the front-most layer.

The basic algorithm uses two z-buffers. The first pass computes the front-most fragments, through a simple z-tested render. Each successive pass renders the second-nearest fragments on a per fragmen basis. A second z-buffer is required to store the previous nearest fragment depth. As a final step, all the layers are blended together from back to front with *over*. Opaque fragments overwrite the previous ones.

![depth peeling](img/depth%20peeling.svg)

The algorithm above discards any form of z-rejection, every hidden fragment is shaded and overwritten at blending. We can significantly reduce overdraw by rendering all opaque objects to a separate color buffer beforehand. We can also switch to back to front peeling.

If peeling front to back, only transparent objects are processed. The first pass is made to populate the z-buffer. On subsequent passes, if the $z$ of a fragment matches the value in the z-buffer and it's in front of the opaque z-buffer, then it is rendered on the transparency color buffer. We also mark on the second z-buffer the second-nearest fragment encountered (if any). Successive passes continue to peel and blend transparent layers with *under*. As a final step, the transparent image is blended *over* the opaque image.\
Note how fragments behind opaque fragments are not drawn (and also note that this implementation uses three z-buffers?). The advantage of front to back peeling is that the most important transparent layers are rendered first, as overlapping surfaces become harder to distinguish.

If peeling back to front, the first pass uses the already populated opaque z-buffer. On subsequent passes, only transparent objects are rendered. We render the second-farthest fragment on the transparency color buffer (if any), and we store its $z$ on the second z-buffer. Successive passes continue to peel and blend transparent layers with *over*.\
Note how, for each fragment, the peeling starts from an opaque-originated z-value. The advantage of back to front peeling is that the transparent colors can be blended immediately over of the opaque color buffer.

In general, the peeling stops when the number of pixels rendered by a pass falls below some minimum, or a fixed number of passes can be specified.
