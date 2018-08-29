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
output$display_button           <- reactive({'Display BFAST results'})

output$text_option_h_date_break <- reactive({"History beginning year"})
output$text_option_m_date_break <- reactive({"Monitoring start and end years"})
output$seq_checkbox             <- reactive({"Compute sequential?"})
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
    "<h4> Parameters </h4>
<br/>
    <i><b> Historical / Monitoring break point </b></i><br/>
The year that marks the end of the historical period and the start of the monitoring period. 
Monitoring will start in January of the year specified and the historical period will end in December of the previous year. 
    <br/><br/>
    <i><b> History </b></i><br/>
    Specifies the start of the stable history period. The options are:<br/><ul>
    <li>reverse-ordered CUSUM (<i>ROC</i>), looks backward in time, using a stepwise approach, 
to identify a stable history period </li>
    <li>Bai and Perron breakpoint estimation (<i>BP</i>), also identifies a stable history period 
and can additionally be used to identify disturbances in the history period. </li>
    <li><i>all</i>, uses all available observations. </li>
    <li>numeric, i.e., <i>2011</i> , the start date can be specified using the year. </li></ul>
<br/>
    <i><b> Formula </b></i> <br/>
    The formula describes the type of regression model applied. The options are: <br/><ul>
    <li><i>trend + harmon</i>, a linear trend and a harmonic season component </li>
    <li><i>harmon</i>, a harmonic season component </li> 
    <li><i>trend</i>, a linear trend </li></ul>
<br/>
    <i><b> Order  </b></i> <br/>
    Specifies the order of the harmonic term, defaulting to 3.
<br/><br/>
    <i><b>Type </b></i><br/>
    Specifies the type of monitoring process.  The options are: <br/><ul>
    <li>moving sums of residuals (<i>MOSUM</i>), where residuals are calculated as the difference between 
    expected values and actual observations in a monitoring period based on OLS residuals. </li>
    <li>cumulative sum (<i>CUSUM</i>), cumulative sums of standardized residuals (MOSUM uses a moving sum, 
    while CUSUM uses a cumulative of the same residuals) </li>
    <li>moving estimates (<i>ME</i>), the moving estimates process is returned </li>
    <li><i>fluctuation</i>, returns the recursive estimates process </li>
for additional documentation on the type parameter see the strucchange package documentation
",
    a(href="https://www.jstatsoft.org/article/view/v007i02",
      "(strucchange package documentation.)",target="_blank"),
    '</li></ul>'
  ))})
output$title_download_testdata <- reactive({"Download test data"})

output$references_text  <- reactive({
  HTML(paste0(
    "TBA"
  ))})
############################ INTRODUCTION TAB - BOX 2
output$title_opt_dir <-  reactive({"Parameters"})

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







