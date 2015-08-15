ipdw
======

![ipdw Downloads](http://cranlogs.r-pkg.org/badges/ipdw)
[![Build status](https://ci.appveyor.com/api/projects/status/5vb8krkx50r1qhbp?svg=true)](https://ci.appveyor.com/project/jsta/ipdw)

The `R` package [ipdw](http://jsta.github.io/public/stachmadden2015am.pdf)  provides the functionality to perform interpolation of georeferenced point data using inverse path distance weighting. Interpolation is accomplished in two steps. First, path distances are calculated from each georeferenced (measurement) point to each prediction point. Path distances, which honor barriers in the landscape, are calculated based on cell-to-cell movement through an underlying `Raster` object that represents movement cost. These path distances are subsequently used as interpolation weights. The two-step routine follows the order of operations described in Suominen et al. (2010) substituting the ESRI path distance algorithm with the `gdistance` wrapped version of the `igraph` adjacency algorithm.    

The ipdw package was developed with coastal marine applications in mind where path distances (as the fish swims) rather than Euclidean (as the crow flies) distances more accurately represent spatial connectivity. Interpolation of sparse grids in coastal areas otherwise end up bleeding through land areas.

##Installation
  ```R
  install.packages("ipdw")
  install.packages('devtools')  # package devtools needed
  devtools::install_github('jsta/ipdw')
  ```

##Examples
**see vignette**

##References
**Joseph Stachelek and Christopher J. Madden (2015)**. Application of Inverse Path Distance weighting for high density spatial mapping of coastal water quality patterns.
*International Journal of Geographical Information Science*
[preprint](http://jsta.github.io/public/stachmadden2015am.pdf) | [journal](http://dx.doi.org/10.1080/13658816.2015.1018833)

**Tapio Suominen, Harri Tolvanen, and Risto Kalliola (2010)**. Surface layer salinity gradients and flow patterns in the archipelago coast of SW Finland, northern Baltic Sea.
*Marine Environmental Research*
[journal](http://dx.doi.org/10.1016/j.marenvres.2009.10.009)
  
