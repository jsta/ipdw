The ipdw package provides the functionality to perform interpolation of georeferenced point data using inverse path distance weighting. Interpolation is accomplished in two steps. First, path distances are calculated from each georeferenced (measurement) point to each prediction point. Path distances, which honor barriers in the landscape, are calculated based on cell-to-cell movement through an underlying Raster object that represents movement cost. These path distances are subsequently used as interpolation weights. The two-step routine follows the order of operations described in Suominen et al. (2010) substituting the ESRI path distance algorithm with the gdistance wrapped version of the igraph adjacency algorithm.    

The ipdw package was developed with coastal marine applications in mind where path distances (as the fish swims) rather than Euclidean (as the crow flies) distances more accurately represent spatial connectivity. Interpolation of sparse grids in coastal areas otherwise end up bleeding through land areas.
