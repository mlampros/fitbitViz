

## fitbitViz 1.0.2

* I've added the Dockerfile to build the image and I've modified the README.md file with instructions on how to use the image
* I've modified the *'rayshader_3d_DEM()'* function by adding the *add_shadow_rescale_original* parameter (it defaults to FALSE) because I received: *Error: non-conformable arrays*
* I've modified the *'meshgrids_XY_LatLon()'* function to suppress a warning due to the internal use of the *'geodist::geodist()'* function
* I've set *'eval = FALSE'* to the last code snippet in the vignette (call to *'rgl'*) because I received *'PhantomJS not found. You can install it with webshot::install_phantomjs()'* (the *webshot* package is not installed by default on CRAN)


## fitbitViz 1.0.1

* I've fixed an error in the **leafGL_point_coords()** function (I replaced the **color** with the **fillColor** parameter)
* I've updated the **README.md** file with instructions on how to setup *fitbitViz* with *blogdown* and *Github Actions*
* I've included the Github URL in the DESCRIPTION file


## fitbitViz 1.0.0

