# POPHELPERSHINY
# UI.R
# Roy Mathew Francis

source("functions.R")

shinyUI(
  fluidPage(theme=shinythemes::shinytheme("flatly"),
            tags$head(tags$link(rel="stylesheet",type="text/css",href="styles.css")),
            fluidRow(style="padding-right:4%;padding-left:4%;",
                     column(12,
                            headerPanel(windowTitle="Pophelper | Population structure analyses",
                                        HTML("<div><img src='bannerv2.png' class='img-responsive' alt='Pophelper | Population structure analyses'</div>")
                            ),
                            tags$head(
                              tags$link(rel="shortcut icon", href="favicon.ico"),
                              includeScript("google-analytics.js")
                            ),
                            fluidRow(style="margin-left: 0px; margin-right:0px",                 
                                     column(12,
                                            tags$br(),
                                            tabsetPanel(id="tabset_main",
                                                        tabPanel("Upload",
                                                                 fluidRow(
                                                                   column(3,
                                                                          tags$br(),
                                                                          wellPanel(
                                                                            h4('Data Upload'),
                                                                            selectInput("in_filetype","Input format",choices=c("Auto","Basic","Baps","Clumpp","Structure","Tess"),selectize=TRUE,multiple=FALSE,selected="Auto"),
                                                                            shinyBS::bsTooltip(id="in_filetype",title="Input file format. See Guide for format descriptions.",placement="top",trigger="hover"),
                                                                            fileInput("in_filesmain",label="Upload file(s)",multiple=T),
                                                                            
                                                                            helpText("Upload a zipped file (.zip) with one or more run files of a single filetype/format."),
                                                                            helpText("Check out the 'Guide' tab for more detailed instructions and sample files to download.")
                                                                          )
                                                                   ),
                                                                   column(8,
                                                                          tags$br(),
                                                                          DT::dataTableOutput("table_basic")
                                                                          #verbatimTextOutput("out_display")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Data",
                                                                 fluidRow(
                                                                   column(3,
                                                                          tags$br(),
                                                                          # download tab data
                                                                          uiOutput("ui_downloadtabulated"),
                                                                          uiOutput("ui_downloadsummarised")
                                                                   ),
                                                                   column(9,
                                                                          tags$br(),
                                                                          tabsetPanel(id="tabset_data",
                                                                                      tabPanel("Tabulated data",
                                                                                               fluidRow(
                                                                                                 column(8,
                                                                                                        tags$br(),
                                                                                                        DT::dataTableOutput("table_tabulated")
                                                                                                 )
                                                                                               )      
                                                                                      ),
                                                                                      tabPanel("Summarised data",
                                                                                               fluidRow(
                                                                                                 column(8,
                                                                                                        tags$br(),
                                                                                                        DT::dataTableOutput("table_summarised")
                                                                                                 )
                                                                                               )
                                                                                      )
                                                                          )
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Evanno",
                                                                 uiOutput("ui_evannopage")
                                                        ),
                                                        tabPanel("Plot",
                                                                 fluidRow(
                                                                   column(3,
                                                                          verbatimTextOutput("out_display2"),
                                                                          uiOutput("ui_plotoptions"),
                                                                          uiOutput("ui_downloadplotoptions")
                                                                   ),
                                                                   column(9,
                                                                          tags$br(),
                                                                          tabsetPanel(id="tabset_plot",
                                                                                      tabPanel("Standard Plot",
                                                                                               fluidRow(
                                                                                                 column(4,
                                                                                                        uiOutput("ui_stdplotoptions")
                                                                                                 ),
                                                                                                 column(8,
                                                                                                        tags$br(),
                                                                                                        uiOutput("ui_plot_barplot")
                                                                                                 )
                                                                                                 # column(4,
                                                                                                 #        div(id="div_display_plot",style="overflow-y:scroll; max-height: 800px",
                                                                                                 #            verbatimTextOutput("out_display_plot")
                                                                                                 #        )
                                                                                                 #        )
                                                                                               )),
                                                                                      tabPanel("Interactive Plot",
                                                                                               fluidRow(
                                                                                                 column(4,
                                                                                                        uiOutput("ui_intplotoptions")
                                                                                                 ),
                                                                                                 column(8,
                                                                                                        tags$br(),
                                                                                                        fluidRow(                                                                                                   
                                                                                                          column(2,                                                                                                          
                                                                                                                 div(id="info-icon-1", style="margin-top:10.5px;margin-bottom:10.5px",
                                                                                                                     icon("info-circle", class="fa-lg")),
                                                                                                                 shinyBS::bsTooltip(id="info-icon-1", title = "Click and drag to zoom in. Hold down shift and drag to slide.",
                                                                                                                                    placement = "top", trigger = "hover")
                                                                                                          )),
                                                                                                        fluidRow(
                                                                                                          column(12, htmlOutput("out_hcontainer")),
                                                                                                          highcharter::highchartOutput("out_hcontainer2", height = "0", width = "0")
                                                                                                        )
                                                                                                 )    
                                                                                               )
                                                                                      )
                                                                          )
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Guide",
                                                                 fluidRow(
                                                                   column(7,
                                                                          includeMarkdown("guide.md")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Versions",
                                                                 fluidRow(
                                                                   column(7,
                                                                          includeMarkdown("versions.md")
                                                                   )
                                                                 )
                                                        ) #tabPanel
                                            ) #tabSetPanel
                                            
                                     )# column
                            ), # fluidRow
                            tags$hr(),
                            HTML(paste0("<small>",fn_pophelper(),". Last Updated: ",fn_update(),"<small></br>")),
                            HTML(paste0("<small>",format(Sys.Date(),"%Y")," Roy M Francis</small></br>"))
                     ) ##column middle
            ) #fluidRow
  ) #fluidpage
) #shinyUI
