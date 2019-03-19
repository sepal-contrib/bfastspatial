####################################################################################
####### BFAST
####### SEPAL shiny application
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org - yelena.finegold@fao.org
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or
# software or in the documentation accompanying it, for program maintenance and
# upgrading as well as for any # damage that may arise from them. FAO also declines
# any responsibility for updating the data and assumes no responsibility for errors
# and omissions in the data provided. Users are, however, kindly asked to report any
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2018/01/18
## bfast / server # branch for tiling internal
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("www/scripts/text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "Français") {
      source("www/scripts/text_french.R", 
             local = TRUE, 
             encoding = "UTF-8")
      #print("fr")
    }
    if (input$language == "Español") {
      source("www/scripts/text_spanish.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("sp")
    }
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  options(echo=TRUE)
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  ##################################################################################################################################
  ## Allow to download test data
  output$dynUI_download_test <- renderPrint({
    req(input$download_test_button)
    
    dir.create(file.path("~", "bfast_data_test"),showWarnings = F)
    
    withProgress(message = paste0('Downloading data in ', dirname("~/bfast_data_test/")),
                 value = 0,
                 {
                   system("wget -O ~/bfast_data_test/bfast_data_test.zip  https://github.com/openforis/data_test/raw/master/bfast_data_test.zip")
                   system("unzip -o ~/bfast_data_test/bfast_data_test.zip  -d ~/bfast_data_test/ ")
                   system("rm ~/bfast_data_test/bfast_data_test.zip")
                 })
    
    list.files("~/bfast_data_test/")
  })
  
  ##################################################################################################################################
  ############### Select input file (raster OR vector)
  shinyDirChoose(
    input,
    'time_series_dir',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ##################################################################################################################################
  ############### Select Forest-Non Forest mask
  shinyFileChoose(
    input,
    'mask_file',
    filetype = "tif",
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  
  ################################# Data file path
  data_dir <- reactive({
    validate(need(input$time_series_dir, "Missing input: Please select time series folder"))
    req(input$time_series_dir)
    df <- parseDirPath(volumes, input$time_series_dir)
  })
  
  ################################# Display tiles inside the DATA_DIR
  output$outdirpath = renderPrint({
    basename(list.dirs(data_dir(),recursive = F))
  })
  
  ################################# Output directory path
  mask_file_path <- reactive({
    req(input$mask_file)
    df <- parseFilePaths(volumes, input$mask_file)
    file_path <- as.character(df[, "datapath"])
  })
  
  ################################# Setup from the archives the Date Range
  list_year <- reactive({
    req(data_dir())
    data_dir <- data_dir()
    first <- list.dirs(data_dir,recursive = F)[1]
    dates <- unlist(read.csv(paste0(first,'/','dates.csv'),header = FALSE))
    years <- str_split_fixed(unlist(dates),"-",3)[,1]
    years
    # list <- list.files(data_dir,pattern = glob2rx("*_stack*.tif"),recursive = T)
    # unlist(lapply(list,function(x){unlist(strsplit(x,split = "_"))[length(unlist(strsplit(x,split = "_")))-1]}))
  })
  
  ################################# Take the minimum as beginning Date
  beg_year <- reactive({
    req(list_year())
    validate(need(list_year(), "Missing time series data, make sure the data has been downloaded properly"))
    min(list_year())
  })
  
  ################################# Take the maximum as ending Date
  end_year <- reactive({
    req(list_year())
    max(list_year())
  })
  
  # #################################################################################################################################
  # ############## Option buttons --> KEEP IF ARCHIVE READING IS NOT OPTIMAL
  # output$ui_option_h_beg <- renderUI({
  #   req(input$time_series_dir)
  #   selectInput(inputId = 'option_h_beg',
  #               label = "Historical year beginning",
  #               choices = 2000:2020,
  #               selected = as.numeric(beg_year())
  #               )
  # })
  # 
  # output$ui_option_m_end <- renderUI({
  #   req(input$time_series_dir)
  #   selectInput(inputId = 'option_m_end',
  #               label = "Monitoring year end",
  #               choices = as.numeric(input$option_h_beg):2020,
  #               selected = as.numeric(end_year())
  #   )
  # })
  
  ################################# Take the average date for the beginning and end of historical period
  output$ui_option_h_beg <- renderUI({
    # validate(need(beg_year()!=end_year(), "Need data from multiple years"))
    req(input$time_series_dir)
    
    sliderInput(inputId = 'option_h_beg',
                label = textOutput("text_option_h_date_break"),
                min = as.numeric(beg_year()),
                max = as.numeric(end_year()),
                value = as.numeric(beg_year()),
                sep = ""
    )
  })
  ################################# Take the average date for the beginning and end of monitoring period
  output$ui_option_m_beg <- renderUI({
    validate(need(input$time_series_dir, "Missing input: Please select time series folder"))
    req(input$option_h_beg)
    
    # validate(need(beg_year()!=end_year(), "Need data from multiple years"))
    req(input$time_series_dir)
    sliderInput(inputId = 'option_m_beg',
                label = textOutput("text_option_m_date_break"),
                min = as.numeric(input$option_h_beg),
                max = as.numeric(end_year()),
                value = c((as.numeric(input$option_h_beg) + as.numeric(end_year()))/2,as.numeric(end_year())),
                sep = ""
    )
  })
  
  
  
  output$ui_option_order <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_order',
                label = "Order parameter",
                choices = 1:5,
                selected = 1
    )
  })
  
  output$ui_option_history <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_history',
                label = "History parameter",
                choices = c("ROC", "BP", "all",as.numeric(beg_year())),
                selected = "ROC"
    )
  })
  
  
  output$ui_option_type <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_type',
                label = "Type parameter",
                choices = c("OLS-CUSUM", "OLS-MOSUM", "RE", "ME","fluctuation"),
                selected = "OLS-CUSUM"
    )
  })
  
  output$ui_option_formula <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_formula',
                label = "Elements of the formula",
                choices = c("harmon","trend"),
                multiple = TRUE,
                selected = "harmon"
    )
  })
  output$ui_option_returnLayers <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_returnLayers',
                label = "Raster band outputs",
                choices = c("breakpoint", "magnitude", "error", "history", "r.squared", "adj.r.squared", "coefficients"),
                multiple = TRUE,
                selected = c("breakpoint", "magnitude", "error")
    )
  })
  
  output$ui_option_sequential <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = "option_sequential",
                label = "Computation mode",
                choices = c("Overall","Sequential"),
                selected= "Overall"
    )
  })
  
  output$ui_option_useMask <- renderUI({
    req(input$time_series_dir)
    # req(input$mask_file)
    selectInput(inputId = "option_useMask",
                label = "Use a Forest/Non-Forest mask? (Optional)",
                choices = c("No Mask","FNF Mask"),#,"Sequential"),
                selected= "No Mask"
    )
  })
  
  output$ui_tiles <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = "option_tiles",
                label = "Which folders do you want to process?",
                choices = basename(list.dirs(data_dir(),recursive = F)),
                selected= basename(list.dirs(data_dir(),recursive = F)),
                multiple = TRUE
    )
  })
  
  output$ui_option_chunk <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = "option_chunk",
                label = "Processing chunk size",
                choices = c(128,256,512,1024),#,"Sequential"),
                selected= 512
    )
  })
  
  
  ##################################################################################################################################
  ############### Parameters title as a reactive
  parameters <- reactive({
    req(input$time_series_dir)
    
    data_dir            <- paste0(data_dir(),"/")
    print(data_dir)
    
    historical_year_beg <- as.numeric(beg_year())
    monitoring_year_beg <- as.numeric(input$option_m_beg)
    monitoring_year_end <- as.numeric(end_year())
    monitoring_year_end <- as.numeric(input$option_m_beg)
    
    order               <- as.numeric(input$option_order)
    history             <- as.character(input$option_history)
    mode                <- as.character(input$option_sequential)
    type                <- as.character(input$option_type)
    mask                <- as.character(input$option_useMask)
    formula_elements    <- unlist(input$option_formula)
    returnLayers        <- as.character(input$option_returnLayers)
    
    type_num            <- c("OC","OM","R","M","f")[which(c("OLS-CUSUM", "OLS-MOSUM", "RE", "ME","fluctuation")==type)]
    mask_opt            <- c("","_msk")[which(c("No Mask","FNF Mask")==mask)]
    formula             <- paste0("response ~ ",paste(formula_elements,sep = " " ,collapse = "+"))
    
    title <- paste0("O_",order,"_H_",paste0(history,collapse = "-"),"_T_",type_num,"_F_",paste0(substr(formula_elements,1,1),collapse= ""),mask_opt,historical_year_beg,'_',monitoring_year_beg,'_',monitoring_year_end)
    
  })
  
  ##################################################################################################################################
  ############### Insert the start button
  output$StartButton <- renderUI({
    req(input$time_series_dir)
    validate(need(input$option_tiles, "Missing input: Please select at least one folder to process"))
    validate(need(input$option_formula, "Missing input: Please select at least one element in the formula"))
    validate(need(input$option_returnLayers, "Missing input: Please select at least one layer to export"))
    
    actionButton('bfastStartButton', textOutput('start_button'))
  })
  ##################################################################################################################################
  ############### Insert the display button
  output$DisplayButtonCurrent <- renderUI({
    req(input$time_series_dir)
    req(input$bfastStartButton)
    
    validate(need(input$option_tiles, "Missing input: Please select at least one folder to process"))
    validate(need(input$option_formula, "Missing input: Please select at least one element in the formula"))
    validate(need(input$option_returnLayers, "Missing input: Please select at least one layer to export"))
    
    actionButton('bfastDisplayButton_c', textOutput('display_button_c'))
  })
  ##################################################################################################################################
  ############### Insert the display button
  output$DisplayButtonAvailable <- renderUI({
    req(input$time_series_dir)
    # req(input$bfastStartButton)
    
    actionButton('bfastDisplayButton_a', textOutput('display_button_a'))
  })
  
  
  ##################################################################################################################################
  ############### Run BFAST
  bfast_res <- eventReactive(input$bfastStartButton,
                             {
                               req(input$time_series_dir)
                               req(input$bfastStartButton)
                               
                               data_dir            <- paste0(data_dir(),"/")
                               print(data_dir)
                               progress_file <- file.path(data_dir(), "processing.txt")
                               system(paste0('echo "Preparing data..." > ', progress_file))
                               historical_year_beg <- as.numeric(input$option_h_beg)
                               monitoring_year_beg <- as.numeric(input$option_m_beg)[1]
                               monitoring_year_end <- as.numeric(input$option_m_beg)[2]
                               
                               order               <- as.numeric(input$option_order)
                               history             <- as.character(input$option_history)
                               mode                <- as.character(input$option_sequential)
                               type                <- as.character(input$option_type)
                               mask                <- as.character(input$option_useMask)
                               formula_elements    <- unlist(input$option_formula)
                               returnLayers        <- c(as.character(input$option_returnLayers))
                               
                               chunk_size          <- as.numeric(input$option_chunk)
                               
                               type_num            <- c("OC","OM","R","M","f")[which(c("OLS-CUSUM", "OLS-MOSUM", "RE", "ME","fluctuation")==type)]
                               mask_opt            <- c("","_msk")[which(c("No Mask","FNF Mask")==mask)]
                               formula             <- paste0("response ~ ",paste(formula_elements,sep = " " ,collapse = "+"))
                               
                               print(order)
                               print(history)
                               print(formula)
                               print(type)
                               print(returnLayers)
                               print(chunk_size)
                               
                               mask_file_path <- input$mask_file_path
                               title <- paste0("O_",order,"_H_",paste0(history,collapse = "-"),"_T_",type_num,"_F_",paste0(substr(formula_elements,1,1),collapse= ""),mask_opt,'_',mode,'_',historical_year_beg,'_',monitoring_year_beg,'_',monitoring_year_end)
                               
                               tiles <- input$option_tiles
                               
                               save(data_dir,historical_year_beg,monitoring_year_end,monitoring_year_beg,order,history,mode,chunk_size,type,mask,formula_elements,type_num,mask_opt,formula,title,tiles,mask_file_path,returnLayers,
                                    file = paste0(data_dir,"/my_work_space.RData"))
                               
                               system(paste0("nohup Rscript www/scripts/bfast_run_chunks.R ",data_dir,' & '))
                               
                               print("done")
                               
                               
                             })
  
  #############################################################
  # Progress monitor function
  output$print_PROGRESS = renderText({
    invalidateLater(1000)
    req(bfast_res())

    progress_file <- file.path(data_dir(), "processing.txt")
    if(file.exists(progress_file)){
      NLI <- as.integer(system2("wc", args = c("-l", progress_file," | awk '{print $1}'"), stdout = TRUE))
      NLI <- NLI + 1 
      invalidateLater(1000)
      paste(readLines(progress_file, n = NLI, warn = FALSE), collapse = "\n")
    }
    else{
      invalidateLater(2001)
      print("Preparing data...")
    }
    
  })
  
  # does the data exist? 
  # 
  # IsThereNewFile=function(){  #  cheap function whose values over time will be tested for equality;
  #     #  inequality indicates that the underlying value has changed and needs to be 
  #     #  invalidated and re-read using valueFunc
  #     
  #     filenames <- list.files(pattern="*.tif", full.names=TRUE)
  #     length(filenames)
  #   }
  
  
  
  # outputcreated <- reactive({
  # validate(
  #   need(file.exists(paste0(data_dir,"/*/results/bfast_",title,"/bfast_",title,"_threshold.tif")),'Calculation In Progress'),
  #   need(file.info(paste0(data_dir,"/*/results/bfast_",title,"/bfast_",title,"_threshold.tif"))$mtime > Sys.time()-5,'Calculation In Progress')
  # )
  # x <- readRDS('LargeComputationOutput')
  # })
  
  
  # vrtout <- eventReactive(input$bfastDisplayButton,
  #                         {
  #                           req(input$bfastDisplayButton)
  #                           
  #   
  #   # validate(
  #   #   need(
  #       # req(file.exists(paste0(data_dir(), list.files(data_dir(), pattern="\\_threshold.tif$", recursive = T))),'Calculation In Progress')
  #   #     )
  #   # )
  #  
  #   
  #   print(title)
  #   req(bfast_res())
  #   # req(file.exists(paste0(data_dir(),"/1/results/bfast_",title,"/bfast_",title,"_threshold.tif")))
  #   #############################################################
  #   ### MERGE AS VRT
  #   print(title)
  # 
  # })
  ##################################################################################################################################
  ############### Processing time as reactive
  process_time <- reactive({
    req(bfast_res())
    
    log_filename <- list.files(data_dir(),pattern="log",recursive = T)[1]
    print(paste0(data_dir(),"/",log_filename))
    readLines(paste0(data_dir(),"/",log_filename))
  })
  
  ## list of available results to display
  available_results <- reactive({
    req(input$time_series_dir)
    list.files(path= data_dir(), pattern = "_threshold.vrt$", recursive = TRUE)
  })
  
  output$list_thres <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = "results_thres",
                label = "Results to display in output data directory",
                choices = basename(available_results()),
                selected= basename(available_results()),
                multiple = FALSE
    )
  })
  
  dis_result <- reactiveValues()
  
  observeEvent(input$bfastDisplayButton_a, {
    req(input$time_series_dir)
    
    dis_result$a<- paste0(data_dir(),'/',input$results_thres)
  })
  
  observeEvent(input$bfastDisplayButton_c, {
    req(bfast_res())
    req(input$time_series_dir)
    
    data_dir <- data_dir()
    load(paste0(data_dir(),"/my_work_space.RData"))
    dis_result$a <- paste0(data_dir,"/bfast_",title,"_threshold.vrt")
    
  })  
  ############### Display the results as map
  ## render the map
  output$display_res  <-  renderLeaflet({
    print('Check: Display the map')
    # req(bfast_res())
    # req(vrtout())
    if (is.null(dis_result$a)) return()
    
    print(dis_result$a)
    r<- raster(dis_result$a)
    
    ## this is a so far failed attempt to add labels to the legend :( 
    rf <- as.factor(r)
    
    print(rf)
    print(levels(rf))
    
    lab <- factor(c("No change",
                    "Small negative","Medium negative","Large negative","Very large negative",
                    "Small positive","Medium positive","Large positive","Very large positive"))
    
    colorspal <- factor(c("#fdfdfd",
                          "#f8ffa3","#fdc980","#e31a1c","#a51013",
                          "#c3e586","#96d165","#58b353","#1a9641"))
    
    pal <- colorNumeric(c("#fdfdfd",
                          "#f8ffa3","#fdc980","#e31a1c","#a51013",
                          "#c3e586","#96d165","#58b353","#1a9641"), 
                        values(rf),
                        na.color = "transparent")
    
    m <- leaflet() %>% addTiles() %>%
      addProviderTiles('Esri.WorldImagery') %>% 
      
      addProviderTiles("CartoDB.PositronOnlyLabels")%>% 
      
      addRasterImage(rf, colors = pal, opacity = 0.8, group='Results') %>%
      
      addLegend(colors=colorspal,
                values = values(rf),
                title = "BFAST results"
                ,labels= lab) %>% 
      
      addLayersControl(
        overlayGroups = c("Results"),
        options = layersControlOptions(collapsed = FALSE)
      )  
    
  })
  
  ##################################################################################################################################
  ############### Display parameters
  output$parameterSummary <- renderText({
    req(input$time_series_dir)
    print(paste0("Parameters are : ",parameters()))
  })
  
  ##################################################################################################################################
  ############### Display time
  output$message <- renderText({
    req(bfast_res())
    # req(vrtout())
    
    print("processing time")
    process_time()
  })
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
