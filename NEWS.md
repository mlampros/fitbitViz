

## fitbitViz 1.0.5

* I removed *'rgdal'* from 'Suggests' as it is no longer required (see: https://r-spatial.org/r/2022/12/14/evolution2.html)
* I updated the *README.md* file because the registration of an Application of the fitbit web API has changed
* I added the *'simplifyVector'* parameter to the *'base_url_request()'* function
* I added the *'fitbit_data_type_by_date()'* function which includes the new fitbit data types 'spo2' (Blood Oxygen Saturation), 'hrv' (Heart Rate Variability), 'br' (Breathing Rate), 'temp' (Temperature) and 'cardioscore' (Cardio Fitness Score or VO2 Max). For more details see: https://dev.fitbit.com/build/reference/web-api/
* I added the *'refresh_token_app()'* function which allows the user to refresh the token once the initial access token of the existing Fitbit Application is expired (after 8 hours)
* I added a deprecation message to the *'heart_rate_variability_sleep_time()'* function because it is replaced by the 'fitbit_data_type_by_date()' function when the 'type' parameter is set to 'hrv' (Heart Rate Variability)
* I updated the vignette by adding information regarding the *'refresh_token_app()'* function. I also replaced the *'heart_rate_variability_sleep_time()'* with the *'fitbit_data_type_by_date()'* function by setting the 'type' parameter to 'hrv'
* I added the internal *'plot_data_type()'* function


## fitbitViz 1.0.4

* I've added the *'fitbitViz.R'* file to include the *'utils::globalVariables()'* dependency
* I've modified internally the code of the *'sleep_single_day()'* and *'sleep_time_series()'* functions to account for an error case


## fitbitViz 1.0.3

* I modified the *crop_DEM()* internally and I removed the *'CRS'* and *'digits'* parameters.
* By using the *terra* package the *sp* and *exactextractr* packages are not required
* I modified the *'rayshader_3d_DEM()'* function to accept a 'long' and 'lat' rather than an 'x' and 'y' pair of coordinates. I removed the *'rst_bbx'* parameter because it's no longer required
* The workaround using the *'meshgrids_XY_LatLon()'* function to define the 'x' and 'y' is no longer required after the adjustment of the *'rayshader_3d_DEM()'* function. Thus, the *'OpenImageR'* and *'geodist'* packages are also no longer required.
* I've added *'rgdal'* in 'Suggests' otherwise the package fails on CRAN. I also received the following message from CRAN: "rgdal is installed but you failed to declare the dependence. See 'Writing R Extensions'."


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

