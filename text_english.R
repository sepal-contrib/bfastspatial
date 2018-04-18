############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "BFAST analysis" })

output$t0_title <- reactive({  "BFAST" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Download test dataset"})
output$download_csv_button      <- reactive({'Download as tabular data (.csv)'})
output$start_button             <- reactive({'Launch BFAST calculation'})
output$text_option_date_break   <- reactive({"Historical / Monitoring break point"})
output$seq_checkbox             <- reactive({"Compute sequential ?"})

############################ SERVER FIELDS
output$field_zone_attr_value   <- reactive({"Attribute column defining the zones"})

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$title_language <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$title_description <- reactive({"Description"})

output$body_description  <- reactive({
  HTML(paste0(
    "Perform a BFAST analysis on a given time series
    <br/>
    <br/>
    The time series should be created with the SEPAL BROWSE/time-series functionnality
    <br/>
    <br/>
    For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})


output$title_download_testdata <- reactive({"Download test data"})

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







