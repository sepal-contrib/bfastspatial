####################################################################################
#######                      BFAST wrapper                      ####################
#######    contributors:  Remi d'Annunzio & Yelena Finegold     ####################
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
        menuItem(textOutput('t0_title',inline=T), tabName = "intro_tab", icon = icon("dashboard")),
        menuItem(textOutput('t1_title',inline=T), tabName = "main_tab", icon = icon("area-chart")),
        hr(),
        br(),
        br(),
        menuItem(textOutput('source_code',inline=T), icon = icon("file-code-o"),href = "https://github.com/openforis/bfastspatial"),
        menuItem(textOutput('bug_reports',inline=T), icon = icon("bug")        ,href = "https://github.com/openforis/bfastspatial/issues")
      )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        ####################################################################################
        # New Tab
        tabItem(tabName = "intro_tab",
                fluidRow(
                  # ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: #00994d;}"),
                  
                  ## CSS format for errors, making the message in purple
                  tags$head(tags$style(HTML(".shiny-output-error-validation {color: #cc00ff;font-family:courier;font-size: 120%;}"))),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('title_description'), width=6,status = "success", solidHeader= TRUE,
                    tabBox(width=3000,
                           tabPanel(textOutput('title1_description'),
                                    tags$h4(textOutput('welcome')),
                                    
                                    htmlOutput('welcome_description'),
                                    selectInput(
                                      'language','',choices = c("English","Français","Español")),
                                    uiOutput("chosen_language"),
                                    htmlOutput('body_description'),
                                    br(),
                                    img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                                    img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                                    img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                                    br()                                  
                                    
                           # end tabPanel
                           ),
                          
                           tabPanel(textOutput('title3_description'),
                                    htmlOutput('bfast_description'),
                                    img(src="thumbnails/bfastmonitor_1.png", height = 200, width = 500)
                                    
                           # end tabPanel
                           ),
                          tabPanel(textOutput('title4_description'),
                                   htmlOutput('parameter_description')
                                   # end tabPanel
                          ),
                           tabPanel(textOutput('title_download_testdata'),
                                    actionButton("download_test_button",
                                                 textOutput('download_testdata_button')),
                                    uiOutput("dynUI_download_test")
                           # end tabPanel
                           ),
                           tabPanel(textOutput('title_disclaimer'),
                                    br(),
                                    htmlOutput('body_disclaimer'),
                                    br(),
                                    br(),
                                    img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                                    img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                                    img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                                    br()                                  
                           # end tabPanel
                           ),
                          tabPanel(textOutput('title5_description'),
                                   htmlOutput('references_text')
                                   # end tabPanel
                          )
                    # end tabBox
                           )


                  ####################################################################################
                  # End of the Box
                  
                )
                ####################################################################################
                # End of the fluid row
                
        )
        ),
        ####################################################################################
        # End of the tabItem 
        tabItem(tabName = "main_tab",
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
                      uiOutput("ui_tiles"),
                      br(),
                      # checkboxInput("checkbox_useMask",label=textOutput('checkbox_usemask')),
                      # br(),
                      uiOutput("ui_option_useMask"),
                      br(),
                      
                      shinyFilesButton(id = 'mask_file',
                                       label = "Mask file",  
                                       title = "Browse",
                                       multiple=F)
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('title_opt_dir'),width=6, status = "success", solidHeader= TRUE,
                      htmlOutput('body_opt_dir'),
                      uiOutput("ui_option_h_beg"),
                      uiOutput("ui_option_m_end"),
                      uiOutput("ui_option_m_beg"),
                      uiOutput("ui_option_history"),
                      uiOutput("ui_option_formula"),
                      uiOutput("ui_option_order"),
                      uiOutput("ui_option_type"),
                      uiOutput("ui_option_returnLayers"),
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
                      verbatimTextOutput("print_PROGRESS"),
                      uiOutput("list_thres"),
                      
                      uiOutput("DisplayButtonCurrent"),
                      uiOutput("DisplayButtonAvailable"),
                      
                      # withSpinner(
                        leafletOutput("display_res")
                      # )
                  ,
                      uiOutput("message")
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