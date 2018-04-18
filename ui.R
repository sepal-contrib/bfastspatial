####################################################################################
#######          BFAST wrapper                                  ####################
#######    contributors:  Remi d'Annunzio                       ####################
#######              FAO Open Foris SEPAL project               ####################
#######    remi.dannunzio@fao.org | yelena.finegold@fao.org     ####################
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
## Last update: 2018/04/16
## bfast / ui
####################################################################################


print("Starting the process")

options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

source("www/scripts/load_packages.R",echo = TRUE)



####################################################################################
####### Start User Interface

shinyUI(
  
  dashboardPage(
    skin='green',
    
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
      title= textOutput('title'),
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem(textOutput('t0_title',inline=T), tabName = "main_tab", icon = icon("dashboard")),
        hr(),
        br(),
        br(),
        menuItem(textOutput('source_code',inline=T), icon = icon("file-code-o"),href = "https://github.com/openforis/accuracy-assessment"),
        menuItem(textOutput('bug_reports',inline=T), icon = icon("bug")        ,href = "https://github.com/openforis/accuracy-assessment/issues")
      )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        ####################################################################################
        # New Tab
        tabItem(tabName = "main_tab",
                fluidRow(
                  # ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: #00994d;}"),
                  
                  ## CSS format for errors, making the message in purple
                  tags$head(tags$style(HTML(".shiny-output-error-validation {color: #cc00ff;font-family:courier;font-size: 120%;}"))),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('title_language'), width=4,status = "success", solidHeader= TRUE,
                    selectInput(
                      'language','',choices = c("English","Français","Español")),
                    uiOutput("chosen_language")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('title_description'), width=4,status = "success", solidHeader= TRUE,
                    htmlOutput('body_description')
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('title_download_testdata'), width=4,status = "success", solidHeader= TRUE,
                    actionButton("download_test_button",
                                 textOutput('download_testdata_button')),
                    uiOutput("dynUI_download_test")
                  )
                  
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= textOutput('title_ts_dir'),width=6, status = "success", solidHeader= TRUE,
                      htmlOutput('body_ts_dir'),
                      shinyDirButton(id = 'time_series_dir',
                                       label = "Time Series Folder",  
                                       title = "Browse"),
                      br(),
                      textOutput("time_series_dir_path"),
                      br(),
                      htmlOutput('body_output_dir'),
                      textOutput("outdirpath"),
                      br(),
                      shinyFilesButton(id = 'mask_file',
                                       label = "Forest / Non-Forest mask",  
                                       title = "Browse",
                                       multiple=F),
                      uiOutput("ui_option_useMask"),
                      textOutput("parameterSummary")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('title_opt_dir'),width=6, status = "success", solidHeader= TRUE,
                      htmlOutput('body_opt_dir'),
                      uiOutput("ui_option_h_beg"),
                      uiOutput("ui_option_m_end"),
                      uiOutput("ui_option_m_beg"),
                      uiOutput("ui_option_order"),
                      uiOutput("ui_option_history"),
                      uiOutput("ui_option_type"),
                      uiOutput("ui_option_formula"),
                      uiOutput("ui_option_sequential")
                       
                  )
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title=textOutput('title_result'),width=12,status = "success", solidHeader= TRUE,
                      uiOutput("StartButton"),
                      #dataTableOutput("show_table"),
                      plotOutput("display_res"),
                      uiOutput("message")
                  )
                  ####################################################################################
                  # End of the Box
                  
                ),
                ####################################################################################
                # End of the fluid row
                
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title=textOutput('title_disclaimer'),width=12,status = "success", solidHeader= TRUE,
                      br(),
                      htmlOutput('body_disclaimer'),
                      br(),
                      br(),
                      img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                      img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                      img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                      br()
                  )
                  ####################################################################################
                  # End of the Box
                  
                )
                ####################################################################################
                # End of the fluid row
                
        )
        ####################################################################################
        # End of the tabItem 
        
      )
      ####################################################################################
      # End of the tabItem list
      
    )
    ####################################################################################
    # End of the Dashboard Body
    
  )
  ####################################################################################
  # End of the Dashboard Page 
  
)
####################################################################################
# End of the User Interface