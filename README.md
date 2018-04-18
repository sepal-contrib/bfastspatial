#### BFAST-analysis spatial wrapper

The Breaks for Additive Seasonal and Trend (BFAST) method enables to analyze the dynamics of satellite dense time series and overcome the major challenge to distinguish land-cover change from seasonal phenological variations. 

Verbesselt et al. (2010), Dutrieux et al. (2015) and DeVries et al. (2015) used this approach to demonstrate that time series can be decomposed into trend, seasonal, and remainder components and that the time and number of changes can be detected at high temporal resolution (i.e., 16 days), enabling detection of tree cover change and separation from the phenology signal.

The same authors developed the bfastSpatial package (REFERENCE) which provides utilities to perform change detection analysis on time-series of spatial gridded data, such as the Landsat satellite imagery that cover our period of interest.

The package has been tested in the early versions of the cloud based platform developed by FAO for parallel processing of remote sensing data (https://sepal.io). It has been recently adapted into a functional processing chain (https://github.com/yfinegold/runBFAST/) which is wrapped in the current Shiny application.
# bfast_shiny
