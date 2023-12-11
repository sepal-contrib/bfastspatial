Time series analysis
====================

Overview
--------

The Breaks for Additive Seasonal and Trend (BFAST) method enables users to analyse the dynamics of satellite dense time series and overcome the major challenge of distinguishing land cover change from seasonal phenological variations.

`Verbesselt et al. (2010) <https://doi.org/10.1016/j.rse.2010.08.003>`__, `Dutrieux et al. (2015) <https://doi.org/10.1016/j.isprsjprs.2015.03.015>`__ and `DeVries et al. (2015) <https://doi.org/10.1016/j.rse.2015.08.020>`__ used this approach to demonstrate that time series can be decomposed into trend, seasonal and remainder components, and that the time and number of changes can be detected at high temporal resolution (i.e. 16 days), enabling detection of tree cover change and separation from the phenology signal.

The same authors developed the `bfastSpatial package <https://www.rdocumentation.org/packages/bfastSpatial/versions/0.6.2>`__, which provides utilities to perform change detection analysis on time series of spatial gridded data, such as the Landsat satellite imagery that covers our period of interest.

The package has been tested in the early versions of the cloud-based platform developed by the Food and Agriculture Organization of the United Nations (FAO) for parallel processing of remote sensing data (https://sepal.io). It has been recently adapted into a functional processing chain (https://github.com/yfinegold/runBFAST), which is wrapped in the current Shiny application.

.. attention:: 

    Since the processing of time series using BFAST is computed in SEPAL, selecting a powerful instance is recommended. The computation is parallelized on several central processing units (CPUs), so an instance with a large number of CPUs is recommended as well. Select the terminal button and choose a new instance. Instance ”m16” with 16 CPUs might be fast enough to do the processing.

Introduction
------------

In the introduction section: the **Welcome**, **Intro to BFAST** and **Parameterization** tabs provide additional information about the mechanics of the algorithm. It is highly recommended to read the text in these tabs.

.. note:: 
    
    The application is only currently available in English. French and Spanish translations will be available soon. If you are interested in translating the application into another language, contact remi.dannunzio@fao.org or yelena.finegold@fao.org

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/introduction.png
    :group: bfastspatial

Download the test dataset
-------------------------

If this is your first time using the time series analysis tool, the SEPAL team highly recommends using the example dataset first. This will help you understand the logic and parameters of the analysis with already well-prepared data; afterwards, you can apply it to your own data. The example dataset can be downloaded in the **Download test data** tab.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/test_download.png
    :group: bfastspatial
 
Select :guilabel:`Download test dataset`. The data is downloaded into the :code:`bfast_data_test` folder in your root directory. The file location and information about the download will appear in the lower-right corner. 

Run time series analysis
------------------------

Select the **Process** tab in the left column.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/process_tab.png
    :group: bfastspatial
 
First, select the **Time series** folder. Select the **Time series** folder button to navigate to the folder with your downloaded data (either downloaded from the **SEPAL search option** or the **Test dataset**).

Select the whole **Time series** folder in your download folder. If your area of interest (AOI) had different features, the application will ask you to select which one you want to process (you can select one, some of them, or all of them) (see the example below with 140 distinct features).

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/select_ts.png
    :group: bfastspatial

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/select_ts_tile.png
    :group: bfastspatial

There is an option to apply a mask and run **BFAST** only on areas inside the mask. You can select a file with 0 and 1 values (0 values will be excluded and 1 included in the computation).

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/select_mask.png
    :group: bfastspatial

If you would like to use a mask, select the **FNF mask**. Then, select the raster file by selecting the **forest/non-forest mask** button and navigating to and selecting the mask file. 

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/fnf_mask.png
    :group: bfastspatial

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/browse_mask.png
    :group: bfastspatial

Next, change the **Parameters** for your study area. At this stage, the BFAST Explorer described in **Section 2** can be very useful. You can use it to understand seasonal and interannual patterns of the land cover that you are analysing over your study area. You can do this over several pixels to have a better idea. 

.. note::

    Remember that this module will define a historical period and a monitoring period, so it corresponds to the option “bfastmonitor” in the **BFAST Explorer** module.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/parameters.png
    :group: bfastspatial
 
The parameters include:

-   **History beginning year** – The year that marks the start of the historical period. The actual start date will depend on the history parameter chosen.
-   **Monitoring start and end years** – The monitoring start year is the year that marks the end of the historical period and the start of the monitoring period. The monitoring end year marks the end of the monitoring period.
-   **History parameter** – Specifies the start of a stable historical period. The options are:

    -   **reverse ordered CUSUM (ROC)** – looks backwards in time, using a stepwise approach, to identify a stable historical period.
    -   **Bai and Perron breakpoint estimation (BP)** – identifies a stable historical period and can be used to identify disturbances in the historical period.
    -   **all** – uses all available observations.
    -   **numeric** – the start date can be specified using the year (e.g. 2011).

-   **Elements of the formula** – the formula describes the type of regression model applied. The options are: 

    -   **trend + harmon** – a linear trend and a harmonic season component. 
    -   **harmon** – a harmonic season component.
    -   **trend** – a linear trend.

-   **Order parameter** – Specifies the order of the harmonic term, defaulting to 3.
-   **Type parameter** – Specifies the type of monitoring process (for additional documentation on the type parameter, see the `strucchange package documentation <https://cran.r-project.org/web/packages/strucchange/index.html>`__). The options are:

    -   **Moving sums of residuals (MOSUM)** – residuals are calculated as the difference between expected values and actual observations in a monitoring period based on OLS residuals.
    -   **Cumulative sum (CUSUM)** – cumulative sums of standardized residuals (MOSUM uses a moving sum, while CUSUM uses a cumulative of the same residuals).
    -   **Moving estimates (ME)** – the moving estimates process is returned.
    -   **Fluctuation** – returns the recursive estimates process.

-   **Raster band outputs** – Result layers to be returned. Can be any combination of :code:`breakpoint`, :code:`magnitude`, :code:`error`, :code:`history`, :code:`r.squared`, :code:`adj.r.squared`, :code:`coefficients`. By default: :code:`breakpoint`, :code:`magnitude` and :code:`error` are returned by the function. It is important to know which layers have been requested and in which order they will be exported because the layer names are not specified. Note that if :code:`coefficients` is included, the output will include the following: **(Intercept)** and any trend and/or harmonic coefficients, depending on the values of formula and order.
-   **Computation mode** – choose between running the calculation for the entire monitoring period (overall) or each year of the monitoring period (sequential):

    -   **Overall** – runs BFAST one time for the monitoring period and provides a maximum of one breakpoint for the entire monitoring period.
    -   **Sequential** – runs BFAST for each year of the monitoring period. The output will be per year of the monitoring period and will provide a maximum of one breakpoint per year in the monitoring period. This option does not create the thresholded output and will not display the output within the application. To view the results, use the visualizer in SEPAL or download the results to your local computer. 

Once you have decided on your parameters, run BFAST by selecting the **Launch BFAST calculation** button in the **Results** box. 

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/launch.png
    :group: bfastspatial

Depending on the size of your area and the size of your instance, BFAST can take a long time run. It is not necessary to keep this application open for the results to be created; it is only necessary to make sure that the instance is running.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/log.png
    :group: bfastspatial
 
If your AOI has multiple polygons and contains many numeric folders, it will run the BFAST calculation for each of the folders recursively.

If you are running a large area or have a weak internet connection, which might cause the application to disconnect, you can go to your **User resources** in SEPAL and set the amount of time your session should stay open (see following image), which allows you to shut down SEPAL without stopping the calculation.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/instance.png
    :group: bfastspatial

.. tip:: 

    If the page turns grey and you see **Disconnected from the server**, the process is still running and you can follow the previous step to make sure your session remains active.

If you have a small study area or have the time, you can wait for the algorithm to finish running and view one of the outputs: the thresholded magnitude. 

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/finished.png
    :group: bfastspatial

When the calculation is complete, you will see the text: :code:`Done processing!!! Click on DISPLAY THE RESULTS`. Select the :guilabel:`Display BFAST results from this session` button to display the thresholded magnitude.
 
By default, the output from BFAST includes 3 bands: the **breakpoint**, **magnitude** and **error**. 

An additional output is calculated in this application, which is the **thresholded magnitude**. The thresholded magnitude is calculated using the magnitude output, calculating the mean magnitude value over the AOI and applying thresholds of up to +/- 4 standard deviations from the mean. This layer indicates the positive or negative intensity of change of each pixel. Above 2 standard deviations, you can interpret that a change has certainly occurred compared to the historical period modelled.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/preview.png
    :group: bfastspatial

.. note:: 
    
    If you are not using the instance anymore to process additional time series, please close the instance by selecting the **Trashbin** button.

You can also download your results to your hard drive using FileZilla (e.g. ArcGIS). 

Here are some examples of how layers can be displayed: 

BFAST was computed over the following area in Indonesia over the years 2013–2019 (the years 2013–2016 were used as the historical period and 2016–2019 as the monitoring period).

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/result_rgb.png
    :group: bfastspatial

**Band_1** shows the date when the breakpoint was detected. The output is stored as a decimal date. 

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/result_band_1.png
    :group: bfastspatial

**Band_2** shows the BFAST magnitude of change (in this case, the mean of the cumulative increase or decrease of NDMI since the monitoring period started). It would indicate pixels where vegetation has become wetter or drier. The values can be considered as relative changes, where units are related to the average deviation from the trend of NDMI.

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/result_band_2.png
    :group: bfastspatial
  
**Band_3** shows the errors (pixels where the algorithm did not locate enough data to compute the trends).

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/result_band_3.png
    :group: bfastspatial
 
Finally, you will find an additional layer called **Threshold. The thresholded magnitude is calculated using the magnitude output, calculating the mean magnitude value over the AOI and applying thresholds of up to +/- 4 standard deviations from the mean. The layer is a thematic, classification map which has values ranging from 0–10, corresponding to the legend below (you can see how to name them in the following figure).

.. thumbnail:: https://raw.githubusercontent.com/12rambau/bfastspatial/master/www/tutorial/img/result_sigma.png
    :group: bfastspatial
