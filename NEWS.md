## ipdw 2.0.0

* Converted external (user-facing) operations from `sp` to `sf` (#19)
* Dropped reliance on retiring `rgdal` and `rgeos` packages (#22)

## ipdw 1.0-0

* Maintenance release to reflect archiving of the `geoR` package and updated maintainer contact info

## ipdw 0.2-9

* Maintenance release to satisfy CRAN checks

## ipdw 0.2-8

* Fixed class checks that will throw errors in R version 4.0 (#20)
* Better projection detection, handling, and docs (#15, #17)

## ipdw 0.2-7

* Added argument for direct control of `costRasterGen` output resolution (#8)
* Added argument for adjusting distance decay power (#11)
* Added argument to avoid extrapolation by trimming output  (#12)
* Range and distance decay parameters are saved in output object (#13)

## ipdw 0.2-6

* Converted vignette to html
* More specific namespace handling
* Added a `NEWS.md` file to track changes to the package.
