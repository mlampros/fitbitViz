
[![R-CMD-check](https://github.com/mlampros/fitbitViz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mlampros/fitbitViz/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fitbitViz)](http://cran.r-project.org/package=fitbitViz)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/fitbitViz?color=blue)](http://www.r-pkg.org/pkg/fitbitViz)
[![status](https://tinyverse.netlify.app/badge/fitbitViz)](https://CRAN.R-project.org/package=fitbitViz)


## fitbitViz

**2026-04-27**:

- **Breaking change: Fitbit Web API dependency removed.** All functions that previously required a `user_id` and `token` have been refactored to accept pre-downloaded data directly, eliminating the dependency on `httr`, `jsonlite`, and `base64enc`. This change was driven by recurring Fitbit API policy changes (GitHub issues [#4](https://github.com/mlampros/fitbitViz/issues/4), [#5](https://github.com/mlampros/fitbitViz/issues/5), [#7](https://github.com/mlampros/fitbitViz/issues/7), [#8](https://github.com/mlampros/fitbitViz/issues/8), [#11](https://github.com/mlampros/fitbitViz/issues/11)).
- Users who need the previous API-based version can install it with `remotes::install_github("mlampros/fitbitViz@v1.0.7")` or from the CRAN archive at `https://cran.r-project.org/src/contrib/Archive/fitbitViz/fitbitViz_1.0.7.tar.gz` (the version 1.0.7 is no longer maintained)

<br>

The **fitbitViz** R package allows the extraction of data and the visualization of *ggplot2*, *Leaflet* and *3-dimensionsal Rayshader Maps* based on *Fitbit* data. If you own any of the **Fitbit activity trackers** you can take advantage of this package.

The 3-dimensional Rayshader Map requires the installation of the [CopernicusDEM](https://github.com/mlampros/CopernicusDEM) R package which includes the **30- and 90-meter elevation data**.

<br>

### Package Installation & Citation:

<br>

To install the package from CRAN use, 

```R
install.packages("fitbitViz")

```
<br>

and to download the latest version of the package from Github,

```R
remotes::install_github('mlampros/fitbitViz')

```

<br>

If you use the **fitbitViz** R package in your paper or research please cite `https://CRAN.R-project.org/package=fitbitViz`:

<br>

```R
@Manual{,
  title = {{fitbitViz}: Fitbit Visualizations},
  author = {Lampros Mouselimis},
  year = {2026},
  note = {R package version 1.0.8},
  url = {https://CRAN.R-project.org/package=fitbitViz},
}
```

<br>
