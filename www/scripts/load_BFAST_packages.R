########################################
# include all the needed packages here #

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)
packages(Formula)
packages(gdalUtils)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)
#packages(tictoc)

## Packages for BFAST
packages(bfast)
# set bfast to run 'fast'
set_fast_options() 
packages(bfastSpatial)
packages(parallel)
