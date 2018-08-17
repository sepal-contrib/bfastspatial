############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Time series analysis" })

output$t0_title <- reactive({  "Intro" })
output$t1_title <- reactive({  "Analysis" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Download test dataset"})
output$download_csv_button      <- reactive({'Download as tabular data (.csv)'})
output$start_button             <- reactive({'Launch BFAST calculation'})
output$text_option_date_break   <- reactive({"Historical / Monitoring break point"})
output$seq_checkbox             <- reactive({"Compute sequential ?"})
# output$checkbox_usemask         <- reactive({"Use a mask?"})

############################ SERVER FIELDS
output$field_zone_attr_value   <- reactive({"Attribute column defining the zones"})

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$welcome_description <- reactive({
  HTML(paste0(
    "
    <br/>
The time series analysis SEPAL application uses BFAST monitor to identify 
changes within a time series of satellite imagery.
    <br/>
    <br/>

First choose the language.
    <br/>  "
    ))})
output$title_language <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$title_description <- reactive({"About BFAST"})
output$title1_description <- reactive({"Welcome"})
output$welcome <- reactive({"Welcome to time series analysis using BFAST"})

output$title2_description <- reactive({"Description"})
output$title3_description <- reactive({"Intro to BFAST"})
output$title4_description <- reactive({"Parameterization"})
output$title5_description <- reactive({"References"})


output$body_description  <- reactive({
  HTML(paste0(
    " <br/>
    The time series should be created with the SEPAL BROWSE/time-series functionality.
    For instructions for downloading a time series using SEPAL, see" ,
    a(href="https://github.com/openforis/bfastspatial/blob/master/www/tutorial/timeseries_SEPAL_comp.pdf",
    " the time series tutorial.",target="_blank"),"
    <br/>
    <br/>
    For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

output$bfast_description  <- reactive({
  HTML(paste0(
    "BFAST, Breaks For Additive Season and Trend, integrates the decomposition of time series into trend, 
season, and remainder components with methods for detecting and characterizing change within time series. 
BFAST iteratively estimates the time and number of abrupt changes within time series, and characterizes 
change by its magnitude and direction. BFAST can be used to analyze different types of time series 
(e.g. Landsat, MODIS) and can be applied to other disciplines dealing with seasonal or non-seasonal time 
series, such as hydrology, climatology, and econometrics. The algorithm can be extended to label detected 
changes with information on the parameters of the fitted piecewise linear models. ", 
    a(href="http://bfast.r-forge.r-project.org/",
      "(BFAST R package documentation) ",target="_blank")
  ))})

output$parameter_description  <- reactive({
  HTML(paste0(
    "Parameters 
<br/>
    formula <br/>
    formula for the regression model. The default is response ~ trend + harmon,
    i.e., a linear trend and a harmonic season component. Other specifications are
    possible using all terms set up by bfastpp, i.e., season (seasonal pattern with
    dummy variables), lag (autoregressive terms), slag (seasonal autoregressiveterms), 
    or xreg (further covariates). See bfastpp for details.
<br/><br/>
    order <br/>
 numeric. Order of the harmonic term, defaulting to 3.
<br/><br/>
    lag <br/>
numeric. Order of the autoregressive term, by default omitted.
<br/><br/>
    history <br/>
specification of the start of the stable history period. Can either be a character,
    numeric, or a function. If character, then selection is possible between
    reverse-ordered CUSUM (ROC, default), Bai and Perron breakpoint estimation
    (BP), or all available observations (all). If numeric, the start date can
    be specified in the same form as start. If a function is supplied it is called as
    history(formula, data) to compute a numeric start date.
<br/><br/>
    type <br/>character specifying the type of monitoring process. By default, a MOSUM
    process based on OLS residuals is employed. See mefp for alternatives.",
    a(href="http://bfast.r-forge.r-project.org/",
      "(BFAST R package documentation) ",target="_blank")
  ))})
output$title_download_testdata <- reactive({"Download test data"})

output$references_text  <- reactive({
  HTML(paste0(
    "TBA"
  ))})
############################ INTRODUCTION TAB - BOX 2
output$title_ts_dir <- reactive({"Time series input"})

output$body_ts_dir  <- reactive({
  HTML(paste0(
    "Choose the folder where the time series were downloaded"
    )
    )})

output$body_output_dir  <- reactive({
  HTML(paste0(
    "The DATA directory contains the following tiles:"
  )
  )})

############################ INTRODUCTION TAB - BOX 5
output$title_result <- reactive({"Results"})


############################ INTRODUCTION TAB - BOX 4
output$title_disclaimer <- reactive({"Disclaimer"})

output$body_disclaimer  <- reactive({
  HTML(paste0(
    "FAO declines all responsibility for errors or deficiencies in the database 
    or software or in the documentation accompanying it for program maintenance and 
    upgrading as well as for any damage that may arise from them.<br/>
    FAO also declines any responsibility for updating the data and assumes 
    no responsibility for errors and omissions in the data provided.<br/>
    Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
))})







