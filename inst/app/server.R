# POPHELPER SHINY
# SERVER.R
# Roy Mathew Francis
# v2.0.0
# 13-Aug-2017

# Ensure CLUMPP has executable permissions on server
options(shiny.maxRequestSize=((1*1024^2)/5))
rootwd <- getwd()

## SERVER =======================================================================

shinyServer(function(input,output,session) {
  
  # RV: store ------------------------------------------------------------------
  # reactive values in store initialised
  
  store_general <- reactiveValues(basic=NULL,
                                  qlist=NULL,
                                  tabulateq=NULL,
                                  summariseq=NULL,
                                  currwd=rootwd,
                                  newwd=fn_dir(rootwd))
  store_evanno <- reactiveValues(evanno_check=NULL,
                                 evanno=NULL)
  store_plot_helper <- reactiveValues(selected_run=NULL,
                                      selected_tabulateq=NULL,
                                      selected_summariseq=NULL,
                                      qlist=NULL,
                                      qlist_aligned=NULL,
                                      qlist_merged=NULL,
                                      indlab=NULL,
                                      grplab=NULL,
                                      kvec=NULL,
                                      grplabtitle="None",
                                      grplabtext="None",
                                      
                                      grplaborder="None",
                                      grplabset=NULL,
                                      sortind="None")
  
  # RFN: fn_getfilenames -------------------------------------------------------
  # upload runs and save basic table
  
  fn_getfilenames <- reactive({
    validate(fn_validate(try(input$in_filesmain),message1="No uploaded file(s)."))
    
      cat("Running fn_getfilenames() ...\n")
      inputdata <- input$in_filesmain
      cat(paste0(round(((sum(inputdata[[2]])/1024)/1024),3)," MB uploaded.\n"))
      if(((sum(inputdata[[2]])/1024)/1024) > 5) stop("Upload limit exceeded 5 MB. Upload fewer files or files with smaller size.\n")

      # error if multiple files and zip file
      if(any(grepl(".rar$",inputdata$name))) stop(".rar files are not allowed. Use .zip format.")
      if(any(grepl(".zip$",inputdata$name)) && length(inputdata$name) > 1) stop("Multiple .zip files or .zip files with other files are not allowed.")
      
      #store_general$newwd <- fn_dir(rootwd)
      
      if(any(grepl(".zip$",inputdata$name)) && length(inputdata$name) == 1){
        unzip(inputdata$datapath,exdir=store_general$newwd)
        dpath <- list.files(path=store_general$newwd,full.names=T)
        store_general$basic <- data.frame(name=basename(dpath),
                                 size=file.info(dpath)$size,
                                 type=NA,
                                 datapath=dpath,
                                 format=checkQ(dpath)$type,
                                 fname=gsub("([[:punct:]])|\\s+","_",basename(dpath)),
                                 stringsAsFactors=FALSE)
      }else{
        inputdata$format <- checkQ(inputdata$datapath)$type
        inputdata$fname <- gsub("([[:punct:]])|\\s+","_",inputdata[[1]])
        store_general$basic <- inputdata
      }
      setwd(store_general$newwd)
      cat(paste0("Working directory set to ",store_general$newwd,".\n"))
  })
  
  ## DATA ======================================================================
  
  # OUT: display ---------------------------------------------------------------
  # print values for debugging
  
  output$out_display <- renderPrint({
    print("basic")
    str(store_general$basic)
    print(paste0("qlist length: ",length(store_general$qlist)))
    print("tabulateq")
    str(store_general$tabulateq)
    print("summariseq")
    str(store_general$summariseq)
    print("currwd")
    str(store_general$currwd)
    print("newwd")
    str(store_general$newwd)
    
    print("evanno_check")
    str(store_evanno$evanno_check)
    print("evanno")
    str(store_evanno$evanno)
    
    print("ROOT DIRECTORY")
    print(list.files(rootwd))
    print("ACTIVE DIRECTORY")
    print(list.files(store_general$newwd))
  })
  
  #DT Dom
  #l - length changing input control
  #f - filtering input
  #t - The table!
  #i - Table information summary
  #p - pagination control
  #r - processing display element
  
  # OUT: table_basic -------------------------------------------------------------
  # table showing basic data
  
  output$table_basic <- DT::renderDataTable({
    
    fn_getfilenames()
    
    cat("Printing basic data table ...\n")
    
    #progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))

    inputdata <- store_general$basic
    data.frame(filename=inputdata$fname,format=inputdata$format,size_kb=round(inputdata$size/1024,1))
    },
    selection="none",
    options=list(
    searchHighlight=TRUE,
    pageLength=-1,
    dom="ftr",
    columnDefs=list(list(className="dt-center",targets=c(2,3)),
                  list(className="dt-left",targets=1),
                  list(width="200px",targets="_all"))
    ))
  
  # RFN: fn_readq ----------------------------------------------------------------
  # read uploaded data and save as qlist
  
  fn_readq <- reactive({
    validate(fn_validate(try(store_general$basic),message1="No basic table."))
    req(store_general$basic)
    
    inputdata <- store_general$basic
    
    cat("Running fn_readq() ...\n")
    
    #progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))

    selfile <- inputdata$datapath
    if(length(selfile) > 1)
    {
      chk <- "UNIDENTIFIED"
      chk <- unique(checkQ(selfile)$type)
      fn_filechk1 <- function(chk) if (length(chk) > 1) print("Input contains mixed formats.")
      fn_filechk2 <- function(chk) if (length(chk)==1) {if (chk=="UNIDENTIFIED") print("Unidentified input format.")}

      validate(fn_filechk1(try(chk)))
      validate(fn_filechk2(try(chk)))
    }
    
    store_general$qlist <- readQWa(files=inputdata$datapath,
                            filenames=inputdata$fname,indlabfromfile=TRUE)
    unlink(list.files(path=store_general$newwd,full.names=TRUE))
  })
  
  # RFN: fn_tabulateq -----------------------------------------------------------
  # compute tabulated table from qlist
  
  fn_tabulateq <- reactive({
    validate(fn_validate(try(store_general$qlist),message1="No qlist."))
    #req(store_general$qlist)
    
    cat("Running fn_tabulateq() ...\n")
    
    #progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))
    
    store_general$tabulateq <- tabulateQ(store_general$qlist)
  })
  
  # RFN: fn_summariseq -----------------------------------------------------------
  # compute summarised table from tabulated table
  
  fn_summariseq <- reactive({
    validate(fn_validate(try(store_general$basic),message1="No tabulated table."))
    #req(store_general$tabulateq)
    
    cat("Running fn_summariseq() ...\n")
    
    #progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))
    
    store_general$summariseq <- summariseQ(store_general$tabulateq)
  })
  
  # OUT: table_tabulated -------------------------------------------------------
  # table showing tabulated data
  
  output$table_tabulated <- DT::renderDataTable({
    #req(store_general$qlist)
    
    # progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))
    
    fn_readq()
    fn_tabulateq()
    store_general$tabulateq
  },
  selection="none",
  #extensions="TableTools",
  options=list(
    #dom="T<'clear'>lfrtip",
    #tableTools=list(sSwfPath=copySWF(pdf=TRUE)),
    searchHighlight=TRUE,
    autoWidth=TRUE,
    lengthMenu=list(c(5,10,15,-1),c("5","10","15","All")),
    pageLength=10,
    columnDefs=list(list(className="dt-center",targets="_all"),
                    list(className="dt-left",targets=1))
  )
  )
  
  # OUT: table_summarised -------------------------------------------------------------
  # table showing summarised data
  
  output$table_summarised <- DT::renderDataTable({
    req(store_general$tabulateq)
    
    # progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Processing data...",value=round(runif(1,0.4,0.9),2))
    
    fn_summariseq()
    store_general$summariseq
  },
  selection="none",
  #extensions="TableTools",
  options=list(
    #dom="T<'clear'>lfrtip",
    #tableTools=list(sSwfPath=copySWF(pdf=TRUE)),
    searchHighlight=TRUE,
    autoWidth=TRUE,
    lengthMenu=list(c(5,10,15,-1),c("5","10","15","All")),
    pageLength=10,
    columnDefs=list(list(className="dt-center",targets="_all"))
  )
  )
  
  # UI: ui_downloadtabulated ---------------------------------------------------
  # ui to download tabulated data
  
  output$ui_downloadtabulated <- renderUI({
    req(store_general$tabulateq)
    
    if(!is.null(store_general$tabulateq) && input$tabset_data=="Tabulated data")
    {
      wellPanel(
        h4("Download tabulated data"),
        selectInput("in_tabulatedformat","File format",selectize=TRUE,multiple=FALSE,
                    choices=c("tab-delimited","comma-separated","semicolon-separated","excel")),
        shinyBS::bsTooltip(id="in_tabulatedformat",title="Download format and extension. Tab-delimited file (.txt),Comma-separated value (.csv),Semicolon-separated value (.csv[csv2]),Excel (.xlsx).",
                           placement="right",trigger="hover"),
        downloadButton('btn_tabulated','Download'),
        shinyBS::bsTooltip(id="btn_tabulated",title="Download tabulated data.",placement="bottom",trigger="hover")
      )
    }
  })
  
  # DHL: btn_tabulated ---------------------------------------------------------
  # download handler for tabulated data
  
  output$btn_tabulated <- downloadHandler(
    filename=function() {
      ftype <- switch(input$in_tabulatedformat,"tab-delimited"=".txt","comma-separated"=".csv","semicolon-separated"=".csv","excel"=".xlsx")
      paste0(fn_pophelper(),"_data_tabulated",ftype,sep="")
    },
    content=function(file) {
      if(input$in_tabulatedformat=="tab-delimited") write.table(store_general$tabulateq,file,row.names=F,quote=F)
      if(input$in_tabulatedformat=="comma-separated") write.csv(store_general$tabulateq,file,row.names=F,quote=F)
      if(input$in_tabulatedformat=="semicolon-separated") write.csv2(store_general$tabulateq,file,row.names=F,quote=F)
      if(input$in_tabulatedformat=="excel") write.xlsx2(store_general$tabulateq,file,sheetName="tabulated",row.names=F,quote=F)
    }
  )
  
  # UI: ui_downloadsummarised --------------------------------------------------
  # ui to download summarised data
  
  output$ui_downloadsummarised <- renderUI({
    req(store_general$summariseq)
    
    if(!is.null(store_general$summariseq) && input$tabset_data=="Summarised data")
    {
      wellPanel(
        h4("Download summarised data"),
        selectInput("in_summarisedformat","File format",selectize=TRUE,multiple=FALSE,
                    choices=c("tab-delimited","comma-separated","semicolon-separated","excel")),
        shinyBS::bsTooltip(id="in_summarisedformat",title="Download format and extension. Tab-delimited file (.txt),Comma-separated value (.csv),Semicolon-separated value (.csv[csv2]),Excel (.xlsx).",
                           placement="right",trigger="hover"),
        downloadButton('btn_summarised','Download'),
        shinyBS::bsTooltip(id="btn_summarised",title="Download summarised data.",placement="bottom",trigger="hover")
      )
    }
  })
  
  # DHL: btn_summarised ---------------------------------------------------------
  # download handler for summarised data
  
  output$btn_summarised <- downloadHandler(
    filename=function() {
      ftype <- switch(input$in_summarisedformat,"tab-delimited"=".txt","comma-separated"=".csv","semicolon-separated"=".csv","excel"=".xlsx")
      paste0(fn_pophelper(),"_data_summarised",ftype,sep="")
    },
    content=function(file) {
      if(input$in_summarisedformat=="tab-delimited") write.table(store_general$summariseq,file,row.names=F,quote=F)
      if(input$in_summarisedformat=="comma-separated") write.csv(store_general$summariseq,file,row.names=F,quote=F)
      if(input$in_summarisedformat=="semicolon-separated") write.csv2(store_general$summariseq,file,row.names=F,quote=F)
      if(input$in_summarisedformat=="excel") write.xlsx2(store_general$summariseq,file,sheetName="summarised",row.names=F,quote=F)
    }
  )
  
  ## EVANNO ====================================================================
  
  # UI: ui_evannopage ----------------------------------------------------------
  # ui for whole evanno page
  
  output$ui_evannopage <- renderUI({
    req(store_general$basic)
    #validate(fn_validate(try(store_general$basic),message1="ui_evannopage: 'store_general$basic' is null."))
    
    if(is.null(store_general$qlist)) fn_readq()
    if(is.null(store_general$tabulateq)) fn_tabulateq()
    if(is.null(store_general$summariseq)) fn_summariseq()
    
    validate(fn_validate_equal(length(unique(store_general$basic$format))>1,FALSE,"Input files are mixed formats. Use only STRUCTURE runs."))
    validate(fn_validate_equal(unique(store_general$basic$format)=="STRUCTURE",TRUE,"One or more input files are not in STRUCTURE format."))
    store_evanno$evanno_check <- TRUE
    
    if(store_evanno$evanno_check)
    {
      div(
        tags$br(),
        tabsetPanel(
          tabPanel("Input",
                   fluidRow(
                     column(7,
                            div(
                              fluidRow(
                                column(3,
                                       div(id="div_evannoinput",class="row",
                                           div(class="col-xs-2",style=list("padding-right:5px;margin-top:10.5px;margin-bottom:10.5px"),
                                               icon("info-circle",class="fa-lg")),
                                           div(class="col-xs-10",style=list("padding-left:5px;"),
                                               h4(" Input files")
                                           )),
                                       shinyBS::bsTooltip("div_evannoinput",title="Select runs (rows) below by clicking to include in the Evanno analysis. If no rows are selected,all rows are included by default. To see the results, go to the Output tab.",placement="top",trigger="hover")
                                )
                                #column(11,style="padding-left:0px",
                                #h4("Input files")
                                #)
                              ),
                              DT::dataTableOutput("table_evanno_input")
                            )),
                     column(5,
                            div(
                              div(id="div_kplot",h4("K plot")),
                              div(
                                imageOutput("out_plot_k",width="100%",height="100%")
                              )
                            )
                     )
                   )
          ),
          tabPanel("Output",
                   fluidRow(
                     column(12,
                            tags$br(),
                            h3("Evanno results data"),
                            DT::dataTableOutput("table_evanno_output"),
                            tags$br(),
                            tags$hr(),
                            fluidRow(
                              column(4,
                                     uiOutput("ui_evannoplotoptions"),
                                     uiOutput("ui_evannodownloadoptions")
                              ),
                              column(8,
                                     uiOutput("ui_evanno_plot")
                              )
                            )
                     )
                     
                   )    
          )
        ) 
      )  
    }
    
  })
  
  # OUT: table_evanno_input ------------------------------------------------------
  # displays table to select runs for evanno calculation
  
  output$table_evanno_input <- DT::renderDataTable({
    #req(store_evanno$evanno_check)
    validate(fn_validate(try(store_evanno$evanno_check),message1="table_evanno_input: 'store_evanno$evanno_check' is null."))
    
    store_general$tabulateq[,1:5]
  },
  selection="multiple",
  options=list(
    autoWidth=FALSE,
    columnDefs=list(list(width="16.6%",targets="_all")),
    pageLength=-1,
    dom="t",
    order=list(c(1,"asc")),
    ordering=FALSE)
  )

  # OUT: out_plot_k ---------------------------------------------------------------
  # displays k-plot from summarised data table
  
  output$out_plot_k <- renderImage({
    #req(store_evanno$evanno_check)
    validate(fn_validate(try(store_evanno$evanno_check),message1="out_plot_k: 'store_evanno$evanno_check' is null."))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Drawing plot...",value=0.6)
    
    evindex <- input$table_evanno_input_rows_selected
    if(length(evindex)==0) 
    {
      dfr <- store_general$summariseq
    }else{
      validate(fn_validate_equal(length(evindex)<2,FALSE,"Select two or more runs."))
      dfr <- summariseQ(data=store_general$tabulateq[evindex,])
    }
    
    progress$inc(0.7,message="Drawing plot...")
    
    plotcol <- "grey30"
    plotcol1 <- "steelblue"
    height1 <- 7
    width1 <- 7
    base_size <- 8
    linesize <- base_size*0.05
    
    p <- ggplot2::ggplot(dfr,aes(x=k,y=elpdmean))+
      geom_path(colour=plotcol1,size=linesize,na.rm=TRUE)+
      geom_point(colour=plotcol1,fill=plotcol1,size=base_size*0.3,shape=19,na.rm=TRUE)+
      geom_errorbar(aes(x=k,ymax=elpdmax,ymin=elpdmin,width=0.2),size=linesize,colour=plotcol,na.rm=TRUE)+
      scale_y_continuous(labels=function(x) floor(x))+
      theme_bw(base_size=base_size)+
      labs(x=expression(paste(italic(K))),
           y=expression(paste("Mean L(",italic(K),") " %+-% " SD")))+
      theme(legend.position="none",
            axis.text.y=element_text(angle=90,hjust=0.5,size=base_size*1.1,colour=plotcol),
            axis.text.x=element_text(size=base_size*1.1,colour=plotcol),
            axis.title=element_text(size=base_size+1,colour=plotcol,face="bold"),
            plot.title=element_text(size=base_size+3,hjust=0,colour=plotcol),
            axis.ticks=element_blank(),
            panel.border=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_line(size=base_size*0.04),
            plot.margin=grid::unit(c(0.15,0.15,0.15,0.15),"cm"))
    
    png(filename="kPlot.png",height=height1,width=width1,res=300,units="cm",type="cairo")
    suppressWarnings(print(p))
    dev.off()
    cat("kPlot.png created.\n")
    
    progress$inc(0.9,message="Completed...")
    
    return(list(src="kPlot.png",
                contentType="image/png",
                width=round((7*300)/2.54,0)*0.6,
                height=round((7*300)/2.54,0)*0.6,
                alt="kPlot"))
  },deleteFile=T)
  
  # UI: ui_evanno_plot ------------------------------------------------------------
  # ui for evanno full plot
  
  output$ui_evanno_plot <- renderUI({
    req(store_evanno$evanno)
    #validate(fn_validate(try(store_evanno$evanno),message1="ui_evanno_plot: 'store_evanno$evanno' is null."))
    
    div(
      h3("Evanno results plot"),
      sliderInput("in_epscale","Image preview scale",min=0,max=3,step=0.10,value=1),
      shinyBS::bsTooltip("in_epscale",title="The scale is only to adjust the preview in the browser and does not affect download.",placement="right",trigger="hover"),
      imageOutput("out_plot_evanno",width="100%",height="100%")
    )
  })
  
  # UI: ui_evannoplotoptions ---------------------------------------------------
  # ui for evanno full plot options
  
  output$ui_evannoplotoptions <- renderUI({
    req(store_evanno$evanno)
    #validate(fn_validate(try(store_evanno$evanno),message1="ui_evannoplotoptions: 'store_evanno$evanno' is null."))
    
    div(
      h3("> Evanno plot options"),
      div(id="evannoplotoptions",
          wellPanel(
            div(class="row",
                div(class="col-xs-6",style=list("padding-right: 5px;"),
                    colourpicker::colourInput("in_eptextcol",label="Text/Error-bar colour",value="#505050"),
                    shinyBS::bsTooltip("in_eptextcol",title="Colour of text elements and error bars.",placement="bottom",trigger="hover")
                ),
                div(class="col-xs-6",style=list("padding-left: 5px;"),
                    colourpicker::colourInput("in_eppointcol",label="Point/Line colour",value="steelblue"),
                    shinyBS::bsTooltip("in_eppointcol",title="Colour of point and line.",placement="bottom",trigger="hover")
                )
            ),
            div(class="row",
                div(class="col-xs-6",style=list("padding-right: 5px;"),
                    numericInput("in_eppointsize","Point size",min=0.1,max=10,value=2.5,step=0.1),
                    shinyBS::bsTooltip("in_eppointsize",title="Size of points on the plot. Eg. 0.1-10.0.",placement="bottom",trigger="hover")
                ),
                div(class="col-xs-6",style=list("padding-left: 5px;"),
                    textInput("in_eppointtype","Point type",value="20"),
                    shinyBS::bsTooltip("in_eppointtype",title="Shape of points on the plot. Eg. 1,2,|,+ etc. See Guide.",placement="bottom",trigger="hover")
                )
            ),
            div(class="row",
                div(class="col-xs-6",style=list("padding-right: 5px;"),
                    numericInput("in_eplinesize","Line size",min=0.1,max=10,value=0.3,step=0.1),
                    shinyBS::bsTooltip("in_eplinesize",title="Size of line on the plot. Eg. 0.1-10.0.",placement="bottom",trigger="hover")
                ),
                div(class="col-xs-6",style=list("padding-left: 5px;"),
                    textInput("in_eplinetype","Line type",value="1"),
                    shinyBS::bsTooltip("in_eplinetype",title="Line type. Eg. 1,2,12 etc. See Guide.",placement="bottom",trigger="hover")
                )
            ),
            div(class="row",
                div(class="col-xs-6",style=list("padding-right: 5px;"),
                    numericInput("in_epbasesize","Base size",min=0.1,max=10,value=7,step=0.1),
                    shinyBS::bsTooltip("in_epbasesize",title="Relative size of text elements on the plot. Eg. 0.1-10.0.",placement="bottom",trigger="hover")
                ),
                div(class="col-xs-6",style=list("padding-left: 5px;"),
                    numericInput("in_epebwidth","Error bar width",min=0,max=1,value=0.1,step=0.01),
                    shinyBS::bsTooltip("in_epebwidth",title="Width of error bars. Eg. 0-1.00",placement="bottom",trigger="hover")
                )
            ),
            tags$br(),
            actionButton("btn_reset_epoptions","Reset panel",class="btn-sm btn-warning btn-block")
          )))
  })
  
  # OBS: btn_reset_epoptions ----------------------------------------------------
  # observer for evanno plot options reset
  
  observeEvent(input$btn_reset_epoptions,{
    colourpicker::updateColourInput(session,"in_eptextcol",value="#505050")
    colourpicker::updateColourInput(session,"in_eppointcol",value="steelblue")
    updateNumericInput(session,"in_eppointsize","Point size",min=0.1,max=10,value=2.5,step=0.1)
    updateTextInput(session,"in_eppointtype","Point type",value="20")
    updateNumericInput(session,"in_eplinesize","Line size",min=0.1,max=10,value=0.3,step=0.1)
    updateTextInput(session,"in_eplinetype","Line type",value="1")
    updateNumericInput(session,"in_epbasesize","Base size",min=0.1,max=10,value=7,step=0.1)
    updateNumericInput(session,"in_epebwidth","Error bar width",min=0,max=0.5,value=0.1,step=0.01)
  })
  
  # FN: fn_evanno_calc ----------------------------------------------------------
  # evanno calculation function
  
  fn_evanno_calc <- function(){
    #req(store_general$tabulateq)
    #req(store_evanno$evanno_check)
    validate(fn_validate(try(store_general$tabulateq),message1="fn_evanno_calc: 'store_general$tabulateq' is null."))
    validate(fn_validate(try(store_evanno$evanno_check),message1="fn_evanno_calc: 'store_evanno$evanno_check' is null."))
    
    cat("Running fn_evanno_calc() ...\n")
    
    #progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Evanno method running...",value=0.1)
    
    if(length(input$table_evanno_input_rows_selected)==0)
    {
      dfr <- store_general$summariseq
    }else
    {
      validate(fn_validate_equal(length(input$table_evanno_input_rows_selected)<2,FALSE,"Select two or more runs."))
      dfr <- summariseQ(data=store_general$tabulateq[input$table_evanno_input_rows_selected,,drop=FALSE])
    }
    
    progress$inc(0.3,message="Evanno method running...")
    
    #atleast 3 values of K
    validate(fn_validate_equal(length(dfr$k)<3,FALSE,"Evanno method not computed. Requires at least 3 values of K."))
    #do loci vary
    validate(fn_validate_equal(all(dfr$loci[1]==dfr$loci),TRUE,"Evanno method not computed. Number of loci vary between runs."))
    #do ind vary
    validate(fn_validate_equal(all(dfr$ind[1]==dfr$ind),TRUE,"The Evanno method not computed. Number of individuals vary between runs."))
    #are k values sequential
    is.sequential <- function(x) all(abs(diff(x))==1)
    validate(fn_validate_equal(is.sequential(dfr$k),TRUE,"Evanno method not computed. Requires increasing sequential values of K."))
    #repeats of k<2
    validate(fn_validate_equal(any(dfr$runs<2),FALSE,"Evanno method not computed. Repeats (runs) for some value of K is less than 2."))
    
    
    store_evanno$evanno <- evannoMethodStructureCalculationWa(data=dfr)
  }
  
  # OUT: table_evanno_output -----------------------------------------------------
  # output evanno results table
  
  output$table_evanno_output <- DT::renderDataTable({
    #req(store_evanno$evanno_check)
    validate(fn_validate(try(store_evanno$evanno_check),message1="table_evanno_output: 'store_evanno$evanno_check' is null."))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Evanno method running...",value=0.4)
    
    fn_evanno_calc()
    progress$inc(0.7,message="Evanno method running...")
    store_evanno$evanno
  },
  selection="none",
  #extensions="TableTools",
  options=list(
    #dom="T<'clear'>lfrtip",
    #tableTools=list(sSwfPath=copySWF(pdf=TRUE)),
    autoWidth=TRUE,
    pageLength=-1,
    dom="t",
    columnDefs=list(list(className="dt-center",targets="_all"))
  ))
  
  
  # RFN: fn_evanno_plotparams ---------------------------------------------------
  # evanno full plot parameters
  
  fn_evanno_plotparams <- reactive({
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Evanno method running...",value=0.7)
    
    dfr <- store_evanno$evanno
    
    validate(fn_validate(try(input$in_eptextcol),message1="fn_evanno_plotparams: Argument 'eptextcol' missing."))
    validate(fn_validate(try(as.numeric(input$in_eppointsize)),message1="fn_evanno_plotparams: Argument 'eppointsize' missing."))
    validate(fn_validate(try(input$in_eppointcol),message1="fn_evanno_plotparams: Argument 'eppointcol' missing."))
    validate(fn_validate(try(input$in_eppointtype),message1="fn_evanno_plotparams: Argument 'eppointtype' missing."))
    validate(fn_validate(try(as.numeric(input$in_eplinesize)),message1="fn_evanno_plotparams: Argument 'eplinesize' missing."))
    validate(fn_validate(try(as.numeric(input$in_eplinetype)),message1="fn_evanno_plotparams: Argument 'eplinetype' missing."))
    validate(fn_validate(try(as.numeric(input$in_epebwidth)),message1="fn_evanno_plotparams: Argument 'epebwidth' missing."))
    validate(fn_validate(try(as.numeric(input$in_epbasesize)),message1="fn_evanno_plotparams: Argument 'epbasesize' missing."))
    
    validate(fn_validate(try(as.numeric(input$in_epheight)),message1="fn_evanno_plotparams: Argument 'epheight' missing."))
    validate(fn_validate(try(as.numeric(input$in_epwidth)),message1="fn_evanno_plotparams: Argument 'epwidth' missing."))
    validate(fn_validate(try(as.numeric(input$in_epres)),message1="fn_evanno_plotparams: Argument 'epres' missing."))
    
    eptextcol <- input$in_eptextcol
    eppointsize <- as.numeric(input$in_eppointsize)
    eppointcol <- input$in_eppointcol
    if(is.na(as.numeric(input$in_eppointtype))){eppointtype <- as.character(input$in_eppointtype)}else{eppointtype <- as.numeric(input$in_eppointtype)}
    eplinesize <- as.numeric(input$in_eplinesize)
    
    eplinetype1 <- as.numeric(input$in_eplinetype)
    if(eplinetype1 > 10 | is.na(eplinetype1)) eplinetype1 <- as.character(input$in_eplinetype)
    
    epebwidth <- input$in_epebwidth
    epbasesize <- input$in_epbasesize

    epheight <- as.numeric(input$in_epheight)
    epwidth <- as.numeric(input$in_epwidth)
    epres <- as.numeric(input$in_epres)
    
    return(list(textcol=eptextcol,pointsize=eppointsize,pointcol=eppointcol,pointtype=eppointtype,
                linesize=eplinesize,linetype=eplinetype1,ebwidth=epebwidth,basesize=epbasesize,
                height=epheight,width=epwidth,res=epres))
  })
  
  
  # OUT: out_plot_evanno ----------------------------------------------------------
  # plot full evanno plot
  
  output$out_plot_evanno <- renderImage({
    req(store_evanno$evanno)
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Evanno method running...",value=0.7)
    
    dfr <- store_evanno$evanno
    evp <- fn_evanno_plotparams()
    
    plist <- evannoMethodStructurePlotWa(data=dfr,textcol=evp$textcol,pointsize=evp$pointsize,pointcol=evp$pointcol,
                                         pointtype=evp$pointtype,linesize=evp$linesize,linecol=evp$pointcol,linetype=evp$linetype,
                                         ebcol=evp$textcol,ebwidth=evp$ebwidth,basesize=evp$basesize)
    plen <- length(plist)
    
    png(paste0(store_general$newwd,"/evanno_plot_preview.png"),height=evp$height,width=evp$width,res=evp$res,units="cm",type="cairo-png")
    if (plen==3) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],ncol=2,nrow=2)
    if (plen==4) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)
    dev.off()
    cat(paste0(store_general$newwd,"/evanno_plot_preview.png exported.\n"))
    
    progress$inc(0.9,message="Evanno method running...")
    
    return(list(src=paste0(store_general$newwd,"/evanno_plot_preview.png"),
                contentType="image/png",
                width=round((evp$width*evp$res)/2.54,0)*input$in_epscale,
                height=round((evp$height*evp$res)/2.54,0)*input$in_epscale,
                alt="evanno_plot_preview"))
  },deleteFile=T)
  
  # UI: ui_evannodownloadoptions -----------------------------------------------
  # ui for evanno full plot download options
  
  output$ui_evannodownloadoptions <- renderUI({
    req(store_evanno$evanno)
    #validate(fn_validate(try(store_evanno$evanno),message1="ui_evannodownloadoptions: 'store_evanno$evanno' is null."))
    
    div(
      h3("> Download options"),
      wellPanel(
        h4("Download Evanno plot"),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_epheight","Height (cm)",min=1,max=20,step=1,value=8),
                shinyBS::bsTooltip("in_epheight",title="Height of the figure. Eg. 1-20.",placement="top",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_epwidth","Width (cm)",min=1,max=20,step=1,value=8),
                shinyBS::bsTooltip("in_epwidth",title="Width of the figure. Eg. 1-20.",placement="right",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                selectInput("in_epres","Res",choices=c("200","300","400","500"),selected="200")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                selectInput("in_epformat","File type",choices=c("png","tiff","jpeg","pdf"),selected="png",multiple=FALSE,selectize=TRUE),
                shinyBS::bsTooltip("in_epformat",title="Change in filetype is not shown in preview.",placement="right",trigger="hover")
            )
        ),
        downloadButton("btn_download_plot_evanno","Download Plot"),
        tags$hr(),
        h4("Download Evanno data"),
        selectInput("in_evannoformat","File format",selectize=TRUE,multiple=FALSE,
                    choices=c("tab-delimited","comma-separated","semicolon-separated","excel")),
        shinyBS::bsTooltip("in_evannoformat",title="Download format and extension. Tab-delimited file (.txt),Comma-separated value (.csv),Semicolon-separated value (.csv[csv2]),Excel(.xlsx).",
                           placement="top",trigger="hover"),
        downloadButton("btn_download_text_evanno","Download Data")
      )
    )
  })
  

  
  # FN:  fn_download_plot_evanno -------------------------------------------------
  # download evanno full plot
  
  fn_download_plot_evanno <- function()
  {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Downloading plot...",value=0.1)
    
    fn_evanno_calc()
    dfr <- store_evanno$evanno
    evp <- fn_evanno_plotparams()
    downloadfilename <- paste0(fn_pophelper(),"_plot_evanno")
    
    progress$set(message="Downloading plot...",value=0.6)
    
    fres <- evp$res
    fheight <- evp$height
    fwidth <- evp$width
    
    validate(fn_validate(try(input$in_epformat),message1="Argument 'epformat' missing."))
    fformat <- as.character(input$in_epformat)
    
    if(fformat=="pdf") fheight <- unitConverter(value=fheight,fromunit="cm",tounit="in",dpi=fres)
    if(fformat=="pdf") fwidth <- unitConverter(value=fwidth,fromunit="cm",tounit="in",dpi=fres)
    
    plist <- evannoMethodStructurePlotWa(data=dfr,textcol=evp$textcol,pointsize=evp$pointsize,pointcol=evp$pointcol,
                                         pointtype=evp$pointtype,linesize=evp$linesize,linecol=evp$pointcol,linetype=evp$linetype,
                                         ebcol=evp$textcol,ebwidth=evp$ebwidth,basesize=evp$basesize)
    plen <- length(plist)

    if(fformat=="png") png(paste0(downloadfilename,".png"),height=fheight,width=fwidth,res=fres,units="cm",type="cairo-png")
    if(fformat=="tiff") tiff(paste0(downloadfilename,".tiff"),height=fheight,width=fwidth,res=fres,units="cm",compression="lzw",type="cairo")
    if(fformat=="jpeg") jpeg(paste0(downloadfilename,".jpg"),height=fheight,width=fwidth,res=fres,units="cm",quality=100,type="cairo")
    if(fformat=="pdf") pdf(paste0(downloadfilename,".pdf"),height=fheight,width=fwidth)
    if (plen==3) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],ncol=2,nrow=2)
    if (plen==4) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)
    dev.off()
    if(fformat=="png") cat(paste0(downloadfilename,".png exported.\n"))
    if(fformat=="tiff") cat(paste0(downloadfilename,".tiff exported.\n"))
    if(fformat=="jpeg") cat(paste0(downloadfilename,".jpeg exported.\n"))
    if(fformat=="pdf") cat(paste0(downloadfilename,".pdf exported.\n"))
            
    progress$inc(0.9,message="Downloading plot...")
  }
  
  # FN: fn_download_plot_evanno_name ----------------------------------------------------
  # creates filename for evanno download plot
  
  fn_download_plot_evanno_name <- function()
  {
    downloadfilename <- NULL
    validate(fn_validate(try(input$in_epformat),message1="Argument 'in_epformat' missing."))
    if(input$in_epformat=="png") downloadfilename <- paste0(fn_pophelper(),"_plot_evanno.png")
    if(input$in_epformat=="jpeg") downloadfilename <- paste0(fn_pophelper(),"_plot_evanno.jpg")
    if(input$in_epformat=="tiff") downloadfilename <- paste0(fn_pophelper(),"_plot_evanno.tiff")
    if(input$in_epformat=="pdf") downloadfilename <- paste0(fn_pophelper(),"_plot_evanno.pdf")
    
    return(paste0(downloadfilename))
  }
  
  # DHL: btn_download_plot_evanno ----------------------------------------------------
  # download handler for evannor full plot download
  
  output$btn_download_plot_evanno <- downloadHandler(
    filename=fn_download_plot_evanno_name,
    content=function(file) {
      fn_download_plot_evanno()
      getwd()
      file.copy(fn_download_plot_evanno_name(),file,overwrite=T)
    }
  )
  
  # DHL: btn_download_text_evanno ------------------------------------------------------------
  # download handler for evanno data
  
  output$btn_download_text_evanno <- downloadHandler(
    filename=function() {
      ftype <- switch(input$in_evannoformat,"tab-delimited"=".txt","comma-separated"=".csv","semicolon-separated"=".csv","excel"=".xlsx")
      paste0(fn_pophelper(),"_data_evanno",ftype,sep="")
    },
    content=function(file) {
      if(input$in_evannoformat=="tab-delimited") write.table(store_evanno$evanno,file,row.names=F,quote=F)
      if(input$in_evannoformat=="comma-separated") write.csv(store_evanno$evanno,file,row.names=F,quote=F)
      if(input$in_evannoformat=="semicolon-separated") write.csv2(store_evanno$evanno,file,row.names=F,quote=F)
      if(input$in_evannoformat=="excel") write.xlsx2(store_evanno$evanno,file,sheetName="evanno",row.names=F,quote=F)
    }
  )
  
  ## PLOT ======================================================================
  
  # UI: ui_plot_barplot --------------------------------------------------------
  # ui for barplot
  
  output$ui_plot_barplot <- renderUI({
    #req(store_general$qlist)
    #validate(fn_validate(try(store_plot_helper$qlist),message1="ui_plot_barplot: 'store_plot_helper$qlist' is null."))
    
    #if(is.null(input$in_imgfloat)){imgfloat <- FALSE}else{imgfloat <- input$in_imgfloat}
    
      div(
      conditionalPanel(
        condition="!is.null(store_general$qlist)",
        div(
            sliderInput("in_scale","Image preview scale",min=0,max=3,step=0.10,value=1),
            shinyBS::bsTooltip("in_scale",title="This slider scales the preview in the browser and does not affect download.",placement="right",trigger="hover")
      )),
      imageOutput("out_plot_barplot",width="100%",height="100%")
    )
  })
  
  # UI: ui_plotoptions ---------------------------------------------------------
  # general plot options
  
  output$ui_plotoptions <- renderUI({
    req(store_general$qlist)
    #validate(fn_validate(try(store_general$qlist),message1="ui_plotoptions: 'store_general$qlist' is null."))
    
    div(
      h3("> Plot options"),
      wellPanel(
                h4("Select file(s) to plot:"),
                helpText("The order in which you select is the order in which runs are plotted."),
                DT::dataTableOutput("table_selectrunplot"),
                tags$br(),
                #checkboxInput("in_imgfloat","Float image preview",value=FALSE),
                #actionButton("btn_draw","Draw plot",class="btn-lg btn-block"),
                #tags$br(),
                selectInput("in_clustercol","Colour scheme",choices=colourPalettes(),selectize=TRUE,selected="Rich"),
                shinyBS::bsTooltip("in_clustercol",title="See guide for details on colours.",placement="top",trigger="hover"),
                uiOutput("ui_colorbreweroption"),
                uiOutput("ui_displaycols"),
                uiOutput("ui_align"),
                selectInput("in_sortind","Order individuals",choices=c("None","Label"),selectize=TRUE,multiple=FALSE,selected="None"),
                shinyBS::bsTooltip("in_sortind",title="Order individuals. When using group labels,individuals are ordered within groups.",placement="top",trigger="hover"),
                checkboxInput("in_useindlab","Use individual labels",value=FALSE),
                uiOutput("ui_indlaboptions"),
                checkboxInput("in_showgrplab","Use group labels",value=FALSE),
                uiOutput("ui_grplaboptions")
      )
    )
  })
  
  # OBS: btn_draw ------------------------------------------------
  # observer to change draw button to update
  
  # observeEvent(input$btn_draw>1,{
  #   updateActionButton(session,"btn_draw","Update plot")
  # })
  
  # OUT: table_selectrunplot ---------------------------------------------------
  # table to select runs to plot
  
  output$table_selectrunplot <- DT::renderDataTable({
    req(store_general$qlist)

    dfr <- store_general$tabulateq[,c("file","k")]
    row.names(dfr) <- dfr$file
    dfr
  },
  rownames=FALSE,
  extensions="Scroller",
  selection="multiple",
  options=list(
    ordering=FALSE,
    autoWidth=TRUE,
    pageLength=-1,
    dom="t",
    scrollY=175,
    columnDefs=list(list(className="dt-center",targets=1),
                    list(className="dt-left",targets=0),
                    list(
                      targets=0,
                      render=JS(
                        "function(data,type,row,meta) {",
                        "return type === 'display' && data.length > 20 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0,20) + '...</span>' : data;",
                        "}")))
  ),
  callback=JS('table.page(3).draw(false);')
  )
  
  # UI: ui_align ---------------------------------------------------------------
  # ui for align/merge repeats
  
  output$ui_align <- renderUI({
    req(store_plot_helper$selected_run)
    #validate(fn_validate(try(store_plot_helper$selected_run),message1="ui_align: 'store_plot_helper$selected_run' is null."))
    
    if(length(store_plot_helper$selected_run)>1){
      div(
        selectInput("in_align","Align/Merge runs (within K)",selectize=TRUE,multiple=FALSE,
                    choices=c("None","Align repeats","Merge repeats"),selected="None"),
        shinyBS::bsTooltip("in_align",title="Align or merge runs of equal K. Runs of varying K cannot be aligned or merged.",placement="top",trigger="hover")
      )
    }
      
  })
  
  # UI: ui_indlaboptions -------------------------------------------------------
  # ui for individual label options
  
  output$ui_indlaboptions <- renderUI({
    #req(input$in_useindlab)
    validate(fn_validate(try(input$in_useindlab),message1="ui_indlaboptions: 'input$in_useindlab' is null."))
    
    if(input$in_useindlab) 
    {
      divgrey(
        selectInput("in_indlabtype","Individual label input",choices=c("From input data","Upload file","Paste text"),selected=1,multiple=F,selectize=T),
        uiOutput("ui_indlabnew"),
        uiOutput("ui_indlabwithgrplab"),
        checkboxInput("in_showindlab","Show individual labels",value=TRUE)
      )
    }
  })

  # UI: ui_indlabnew --------------------------------------------------------
  # ui get new individual labels upload/paste
  
  output$ui_indlabnew <- renderUI({
    #req(input$in_indlabtype)
    validate(fn_validate(try(input$in_indlabtype),message1="ui_indlabnew: 'input$in_indlabtype' is null."))
    
    if(input$in_indlabtype=="Upload file")
    {
      fileInput("in_indlabupload","Choose label text file:",multiple=F)
    }else if(input$in_indlabtype=="Paste text"){
      div(
        shinyAce::aceEditor("in_indlabpaste","",mode="text",theme="textmate",readOnly=FALSE,height="100px",fontSize=10),
        shinyBS::bsTooltip("in_indlabpaste",title="Copy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line.",placement="bottom",trigger="hover")
      )
    }
  })
  
  # UI: ui_indlabwithgrplab ----------------------------------------------------
  # ui for indlab with grplab options
  
  output$ui_indlabwithgrplab <- renderUI({
    #req(input$in_useindlab)
    #req(input$in_showgrplab)
    validate(fn_validate(try(input$in_useindlab),message1="ui_indlabwithgrplab: 'input$in_useindlab' is null."))
    validate(fn_validate(try(input$in_showgrplab),message1="ui_indlabwithgrplab: 'input$in_showgrplab' is null."))
    
    if(input$in_showgrplab && input$in_useindlab)
    {
      div(
        checkboxInput("in_indlabwithgrplab","Concatenate ind & grp labels",value=FALSE),
        shinyBS::bsTooltip("in_indlabwithgrplab",title="Concatenate individual labels to group labels (when group labels are in use).",placement="bottom",trigger="hover"),
        uiOutput("ui_indlabsep")
      )
    }
  })
  
  # UI: ui_indlabsep -----------------------------------------------------------
  # ui for individual label separator
  
  output$ui_indlabsep <- renderUI({
    #req(input$in_indlabwithgrplab)
    validate(fn_validate(try(input$in_indlabwithgrplab),message1="ui_indlabsep: 'input$in_indlabwithgrplab' is null."))
    
    if(input$in_indlabwithgrplab)
    {
      div(
        textInput("in_indlabsep","Separator",value=" "),
        shinyBS::bsTooltip("in_indlabsep",title="Separator between individual and group labels.",placement="bottom",trigger="hover")
      )
    }
  })
  
  # UI: ui_grplaboptions -------------------------------------------------------
  # ui for group label options
  
  output$ui_grplaboptions <- renderUI({
    #req(input$in_showgrplab)
    validate(fn_validate(try(input$in_showgrplab),message1="ui_grplaboptions: 'input$in_showgrplab' is null."))
    
    if(input$in_showgrplab)
    {
      divgrey(
        selectInput("in_grplabtype","Group label input",choices=c("Upload file","Paste text"),selected=1,multiple=F,selectize=T),
        uiOutput("ui_grplabnew"),
        uiOutput("ui_selectgrp"),
        uiOutput("ui_subsetgrp"),
        checkboxInput("in_ordergrp","Order group labels",value=FALSE),
        # this tooltip does not work
        #shinyBS::bsTooltip("in_ordergrp",title="Non-contiguous group labels in the 'Selected group label set' will be grouped together and data is ordered by all label sets starting with 'Selected group label set' below.",placement="right",trigger="hover"),
        checkboxInput("in_grpmean","Compute mean over groups",value=FALSE)
        #uiOutput("ui_grplab_filter_checkbox"),
        #uiOutput("ui_grplab_filter"),
        #uiOutput("ui_grplab_order")
      )
    }
  })
  
  # UI: ui_grplabnew -----------------------------------------------------------
  # ui to get new group labels
  
  output$ui_grplabnew <- renderUI({
    #req(input$in_grplabtype)
    validate(fn_validate(try(input$in_grplabtype),message1="ui_grplabnew: 'input$in_grplabtype' is null."))
    
    if(input$in_grplabtype=="Upload file")
    {
      fileInput("in_grplabupload","Choose label text file:",multiple=F)
    }else if(input$in_grplabtype=="Paste text"){
      div(
        shinyAce::aceEditor("in_grplabpaste","",mode="text",theme="textmate",readOnly=FALSE,height="100px",fontSize=10),
        shinyBS::bsTooltip("in_grplabpaste",title="Copy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line.",placement="bottom",trigger="hover")
      )
    }
  })
  
  # UI: ui_selectgrp -----------------------------------------------------------
  # ui for select group labels
  
  output$ui_selectgrp <- renderUI({
    #req(input$in_showgrplab)
    #req(store_plot_helper$grplabtitle)
    validate(fn_validate(try(input$in_showgrplab),message1="ui_selectgrp: 'input$in_showgrplab' is null."))
    validate(fn_validate(try(store_plot_helper$grplabtitle),message1="ui_selectgrp: 'store_plot_helper$grplabtitle' is null."))
    
    div(
      selectInput("in_selectgrp","Active group label title",choices=store_plot_helper$grplabtitle,selectize=TRUE,multiple=FALSE,selected="None"),
      shinyBS::bsTooltip("in_selectgrp",title="Select one group label title to be used for subsetting, ordering, sorting and group mean.",placement="top",trigger="hover")
    )
  })
  
  # UI: ui_subsetgrp -----------------------------------------------------------
  # ui for subset group labels
  
  output$ui_subsetgrp <- renderUI({
    #req(input$in_showgrplab)
    #req(store_plot_helper$grplabtext)
    validate(fn_validate(try(input$in_showgrplab),message1="ui_subsetgrp: 'input$in_showgrplab' is null."))
    validate(fn_validate(try(store_plot_helper$grplabtext),message1="ui_subsetgrp: 'store_plot_helper$grplabtext' is null."))
    
    div(
      selectInput("in_subsetgrp","Subset/Order group label text",choices=store_plot_helper$grplabtext,selectize=TRUE,multiple=TRUE,selected="None"),
      shinyBS::bsTooltip("in_subsetgrp",title="Select group label text to subset, or change order. Use arrow keys to move left/right. Press backspace to remove.",placement="top",trigger="hover")
    )
  })
  
  # UI: ui_grplab_filter_checkbox ----------------------------------------------
  # ui to display group label filter checkbox if group labels are available
  
  # output$ui_grplab_filter_checkbox <- renderUI({
  #   req(store_plot_helper$grplab)
  #   
  #   if(!is.null(store_plot_helper$grplab))
  #   {
  #     checkboxInput("in_grplab_filter","Filter by group labels",value=FALSE)
  #   }
  # })
  
  # UI: ui_grplab_filter -------------------------------------------------------------
  # ui for grplab filter options
  
  # output$ui_grplab_filter <- renderUI({
  #   req(input$in_grplab_filter)
  #   req(store_plot_helper$grplab)
  #   
  #   if(input$in_grplab_filter)
  #   {
  #     fn1 <- function(x) levels(factor(as.character(x)))
  #     grplab_list <- apply(store_plot_helper$grplab,2,fn1)
  #     
  #     plist <- lapply(1:length(grplab_list),function(i) {
  #       div(class="row",
  #           div(class="col-xs-2",style=list("padding-right: 5px;"),
  #               checkboxInput(paste0("in_grplab_filter_use_",i),"Use",value=TRUE)
  #           ),
  #           div(class="col-xs-10",style=list("padding-left: 5px;"),
  #               selectInput(paste0("in_grplab_filter_text_",i),names(grplab_list)[i],
  #                           choices=grplab_list[[i]],selected=grplab_list[[i]],
  #                           selectize=TRUE,multiple=TRUE)
  #           )
  #       )
  #     })
  #     
  #     div(
  #       helpText("Use checkboxes to include/exclude group label sets. Use select box to include/exclude/reorder group label levels."),
  #       do.call(tagList,plist)
  #       )
  #   }
  # 
  # })
  
  # UI: ui_colorbreweroption ---------------------------------------------------
  # ui for colorbrewer
  
  output$ui_colorbreweroption <- renderUI({
    req(input$in_clustercol)
    #validate(fn_validate(try(input$in_clustercol),message1="ui_colorbreweroption: 'input$in_clustercol' is null."))
    
    if(input$in_clustercol == "Colorbrewer")
    {
      div(divgrey(
        selectInput("in_colorbrewerpalette","Select Colorbrewer palette",selectize=TRUE,multiple=FALSE,
                    choices=list("Qualitative (8-12 colours)"=c("Set1","Set2","Set3","Accent","Pastel1","Pastel2","Paired","Dark2"),
                                 "Diverging (11 colours)"=c("Spectral","RdYlGn","RdYlBu","RdGy","RdBu","PuOr","PRGn","PiYG","BrBG"),
                                 "Sequential (9 colours)"=c("BuGn","BuPu","GnBu","OrRd","PuBu","PuRd","RdPu","YlGn","YlGnBu","PuBuGn","YlOrBr","YlOrRd","Blues","Greens","Oranges","Purples","Reds","Greys")),
                    selected="Set1"),
        shinyBS::bsTooltip("in_colorbrewerpalette",title="Colorbrewer palettes have limits on max colours. See Guide.",placement="top",trigger="hover")
      ),
      tags$br())
    }
  })
  
  # UI: ui_displaycols --------------------------------------------------
  # ui for custom colours
  
  output$ui_displaycols <- renderUI({
    req(input$in_clustercol)
    req(store_plot_helper$selected_tabulateq)
    #validate(fn_validate(try(input$in_clustercol),message1="ui_displaycols: 'input$in_clustercol' is null."))
    #validate(fn_validate(try(store_plot_helper$selected_tabulateq),message1="ui_displaycols: 'store_plot_helper$selected_tabulateq' is null."))
    
    maxk <- max(store_plot_helper$selected_tabulateq$k)
    pal <- NA
    
    if(input$in_clustercol == "Colorbrewer")
    {
      validate(fn_validate(try(input$in_colorbrewerpalette),message1="fn_colours: Argument 'colorbrewerpalette' missing."))
      pal <- as.character(input$in_colorbrewerpalette)
    }
    
    dcols <- getColoursWa(maxk,input$in_clustercol,pal)
    validate(fn_validate_colours(try(dcols),message="Number of colours less than K. Choose a different palette."))
    
    plist <- lapply(1:max(store_plot_helper$selected_tabulateq$k),function(i) {
      colourpicker::colourInput(paste0("in_col",i),label=paste0("Cluster ",i),value=dcols[i])
    })
    
    div(div(style="overflow-y:scroll;max-height:270px;background-color:#d4d8d8;padding-right:10px;padding-left:10px;padding-top:20px;padding-bottom:20px;border-radius:3px;",
        do.call(tagList,plist)),
        tags$br())

  })
  
  ## STD PLOT OPTS =============================================================
  
  # UI: ui_stdplotoptions ------------------------------------------------------
  # ui for standard plot input options
  
  output$ui_stdplotoptions <- renderUI({
    req(store_general$qlist)
    #validate(fn_validate(try(store_plot_helper$selected_run),message1="ui_stdplotoptions: 'store_plot_helper$selected_run' is null."))

    div(
      divopenh3("div_stdplotoptionscollapse","> Standard options",
        wellPanel(id="div_stdplotoptions",style="overflow-y:scroll; max-height: 400px",
            divopenh4("div_generalstdoptions","> General options",
                      divgrey(
              div(class="row",
                  div(class="col-xs-6",style=list("padding-right: 5px;"),
                      numericInput("in_barsize","Bar size",min=0,max=1,step=0.02,value=1),
                      shinyBS::bsTooltip("in_barsize",title="Size of bars on the barplot. Eg 0-1.0.",placement="bottom",trigger="hover")
                  ),
                  div(class="col-xs-6",style=list("padding-left: 5px;"),
                      numericInput("in_barbordersize","Bar border size",min=0,max=5,step=0.1,value=0),
                      shinyBS::bsTooltip("in_barbordersize",title="Size of bar border. Eg 0-5.0.",placement="bottom",trigger="hover")
                  )
              ),
              colourpicker::colourInput("in_barbordercolour",label="Bar border colour",value="#FFFFFF"),
              shinyBS::bsTooltip("in_barbordercolour",title="Colour of bar border.",placement="bottom",trigger="hover"),
              checkboxInput("in_showyaxis","Show Y axis",value=FALSE),
              uiOutput("ui_yaxisoptions"),
              uiOutput("ui_panelspaceroption")
              )),
              checkboxInput("in_showsp","Show side panel",value=TRUE),
              uiOutput("ui_sidepaneloptions"),
              checkboxInput("in_showtitle","Show plot title",value=FALSE),
              uiOutput("ui_titleoptions"),
              checkboxInput("in_showsubtitle","Show plot subtitle",value=FALSE),
              uiOutput("ui_subtitleoptions"),
              checkboxInput("in_showlegend","Show cluster legend",value=FALSE),
              uiOutput("ui_clusterlegendoptions")
              #tags$br(),
              #actionButton("btn_reset_stdoptions","Reset panel",class="btn-sm btn-warning btn-block")
            )),
      uiOutput("ui_indlabstdplotoptions"),
      uiOutput("ui_grplabstdplotoptions")
      #uiOutput("ui_downloadplotoptions")
    )
  })
  
  # OBS: btn_reset_stdoptions --------------------------------------------------
  # observer to reset standard plot options
  
  observeEvent(input$btn_reset_stdoptions,{
    updateNumericInput(session,"in_barsize","Bar size",min=0,max=5,step=0.1,value=1)
    updateNumericInput(session,"in_barbordersize","Bar border size",min=0,max=5,step=0.1,value=0)
    colourpicker::updateColourInput(session,"in_barbordercolour",label="Bar border colour",value="#FFFFFF")
    updateCheckboxInput(session,"in_showyaxis","Show Y axis",value=FALSE)
    updateNumericInput(session,"in_ticksize","Tick size",min=0,max=1,step=0.02,value=0.1)
    updateNumericInput(session,"in_ticklength","Tick length",min=0,max=1,step=0.01,value=0.03)
    updateCheckboxInput(session,"in_showsp","Show side panel",value=TRUE)
    updateCheckboxInput(session,"in_showtitle","Show plot title",value=FALSE)
    updateCheckboxInput(session,"in_showsubtitle","Show plot subtitle",value=FALSE)
    updateCheckboxInput(session,"in_showlegend","Show cluster legend",value=FALSE)
    updateSliderInput(session,"in_ps","Panel spacer",min=0.01,max=0.5,step=0.01,value=0.05)
  })
  
  # UI: ui_yaxisoptions ---------------------------------------------------
  # ui for y axis options
  
  output$ui_yaxisoptions <- renderUI({
    #req(input$in_showyaxis)
    validate(fn_validate(try(input$in_showyaxis),message1="ui_yaxisoptions: 'input$in_showyaxis' is null."))
    
    if(input$in_showyaxis)
    {
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              numericInput("in_ticksize","Tick size",min=0,max=1,step=0.02,value=0.1),
              shinyBS::bsTooltip("in_ticksize",title="Size of y-axis ticks.",placement="top",trigger="hover")
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              numericInput("in_ticklength","Tick length",min=0,max=1,step=0.01,value=0.03),
              shinyBS::bsTooltip("in_ticklength",title="Length of y-axis ticks.",placement="top",trigger="hover")
          )
      )
    }
  })
  
  # UI: ui_panelspaceroption ---------------------------------------------------
  # ui for panel spacer for joined plots
  
  output$ui_panelspaceroption <- renderUI({
    req(store_plot_helper$selected_run)
    #validate(fn_validate(try(store_plot_helper$selected_run),message1="ui_panelspaceroption: 'store_plot_helper$selected_run' is null."))

    if(length(store_plot_helper$selected_run)>1)
    {
      div(
        sliderInput("in_ps","Panel spacer",min=0.01,max=0.5,step=0.01,value=0.05),
        shinyBS::bsTooltip("in_ps",title="Panel spacer adjusts space between run panels.",
                           placement="bottom",trigger="hover")
      )
    }
  })
  
  # UI: ui_sidepaneloptions ----------------------------------------------------
  # ui for side panel/strip panel for std plot
  
  output$ui_sidepaneloptions <- renderUI({
    #req(input$in_showsp)
    validate(fn_validate(try(input$in_showsp),message1="ui_sidepaneloptions: 'input$in_showsp' is null."))
    
    if(input$in_showsp)
    {
      divopenh4("div_sidepaneloptions","> Side panel options",
                divgrey(
        textInput("in_splab","Side panel label(s)",value="",placeholder="label1,label2"),
        shinyBS::bsTooltip("in_splab",title="Enter custom side panel labels if required, separated by commas. Labels must be equal to number of plotted runs.",placement="top",trigger="hover"),

        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_splabsize","Text size",min=1,max=8,step=0.2,value=4),
                shinyBS::bsTooltip("in_splabsize",title="Side panel text size.",placement="top",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                selectInput("in_splabpos","Position",selectize=TRUE,multiple=FALSE,choices=c("Right","Left"),selected="Right"),
                shinyBS::bsTooltip("in_splabpos",title="Side panel position.",placement="top",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                colourpicker::colourInput("in_splabcol",label="Colour",value="#505050"),
                shinyBS::bsTooltip("in_splabcol",title="Side panel text colour.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                colourpicker::colourInput("in_spbgcol",label="Bg colour",value="#FFFFFF"),
                shinyBS::bsTooltip("in_spbgcol",title="Side panel background colour.",placement="bottom",trigger="hover")
            )
        ),
        tags$br(),
        actionButton("btn_reset_spoptions","Reset panel",class="btn-sm btn-warning btn-block")
      )
      )
    }
  })
  
  # OBS: btn_reset_spoptions ---------------------------------------------------
  # observer to reset sp panel options
  
  observeEvent(input$btn_reset_spoptions,{
    updateTextInput(session,"in_splab","Side panel label(s)",value="")
    updateNumericInput(session,"in_splabsize","Text size",min=1,max=8,step=0.2,value=4)
    updateSelectizeInput(session,"in_splabpos","Position",choices=c("Right","Left"),selected="Right")
    colourpicker::updateColourInput(session,"in_splabcol",label="Colour",value="#505050")
    colourpicker::updateColourInput(session,"in_spbgcol",label="Bg colour",value="#FFFFFF")
  })
  
  # UI: ui_titleoptions --------------------------------------------------------
  # ui for title options
  
  output$ui_titleoptions <- renderUI({
    #req(input$in_showtitle)
  validate(fn_validate(try(input$in_showtitle),message1="ui_titleoptions: 'input$in_showtitle' is null."))

    if(input$in_showtitle)
    {
      divopenh4("div_titleoptions","> Title options",
                divgrey(
        textInput("in_titlelab","Title label",value="Title"),
        shinyBS::bsTooltip("in_titlelab",title="Input a custom title label.",placement="top",trigger="hover"),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_titlelabsize","Text size",min=1,max=8,step=0.2,value=5),
                shinyBS::bsTooltip("in_titlelabsize",title="Title text size.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_titlelabspacer","Spacer",min=0.4,max=2.4,step=0.2,value=1.4),
                shinyBS::bsTooltip("in_titlelabspacer",title="Distance between title and panels.",placement="bottom",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-4",style=list("padding-right: 5px; width: 34%"),
                numericInput("in_titlelabhjust","H Justify",min=0,max=1,step=0.2,value=0),
                shinyBS::bsTooltip("in_titlelabhjust",title="Title horizontal justification. Value between 0-1.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-4",style=list("padding-right: 5px; padding-left: 5px; width: 33%"),
                numericInput("in_titlelabvjust","V Justify",min=0,max=1,step=0.2,value=0.5),
                shinyBS::bsTooltip("in_titlelabvjust",title="Title vertical justification. Value between 0-1.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-4",style=list("padding-left: 5px; width: 33%"),
                colourpicker::colourInput("in_titlelabcol",label="Text colour",value="#505050"),
                shinyBS::bsTooltip("in_titlelabcol",title="Title text colour.",placement="top",trigger="hover")
            )
        ),
        tags$br(),
        actionButton("btn_reset_titleoptions","Reset panel",class="btn-sm btn-warning btn-block")
      )
      )
    }
  })
  
  # OBS: btn_reset_titleoptions ------------------------------------------------
  # observer to reset title options
  
  observeEvent(input$btn_reset_titleoptions,{
    updateTextInput(session,"in_titlelab","Title label",value="Title")
    updateNumericInput(session,"in_titlelabsize","Text size",min=1,max=8,step=0.2,value=5)
    updateNumericInput(session,"in_titlelabspacer","Spacer",min=0.4,max=2.4,step=0.2,value=1.4)
    updateNumericInput(session,"in_titlelabhjust","H Justify",min=0,max=1,step=0.2,value=0)
    updateNumericInput(session,"in_titlelabvjust","V Justify",min=0,max=1,step=0.2,value=0.5)
    colourpicker::updateColourInput(session,"in_titlelabcol",label="Text colour",value="#505050")
  })
  
  # UI: ui_subtitleoptions -----------------------------------------------------
  # ui for subtitle options
  
  output$ui_subtitleoptions <- renderUI({
    #req(input$in_showsubtitle)
    validate(fn_validate(try(input$in_showsubtitle),message1="ui_subtitleoptions: 'input$in_showsubtitle' is null."))

    if(input$in_showsubtitle)
    {
      divopenh4("div_subtitleoptions","> Subtitle options",
                divgrey(
        textInput("in_subtitlelab","Subtitle label",value="Subtitle"),
        shinyBS::bsTooltip("in_subtitlelab",title="Input a custom subtitle label.",placement="top",trigger="hover"),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_subtitlelabsize","Text size",min=1,max=8,step=0.2,value=4),
                shinyBS::bsTooltip("in_subtitlelabsize",title="Subtitle text size.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_subtitlelabspacer","Spacer",min=0.4,max=2.4,step=0.2,value=1.4),
                shinyBS::bsTooltip("in_subtitlelabspacer",title="Distance between subtitle and panels.",placement="bottom",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-4",style=list("padding-right: 5px; width: 34%"),
                numericInput("in_subtitlelabhjust","H Justify",min=0,max=1,step=0.2,value=0),
                shinyBS::bsTooltip("in_subtitlelabhjust",title="Subtitle horizontal justification. Value between 0-1.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-4",style=list("padding-right: 5px; padding-left: 5px; width: 33%"),
                numericInput("in_subtitlelabvjust","V Justify",min=0,max=1,step=0.2,value=0.5),
                shinyBS::bsTooltip("in_subtitlelabvjust",title="Subtitle vertical justification. Value between 0-1.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-4",style=list("padding-left: 5px; width: 33%"),
                colourpicker::colourInput("in_subtitlelabcol",label="Text colour",value="#505050"),
                shinyBS::bsTooltip("in_subtitlelabcol",title="Subtitle text colour.",placement="top",trigger="hover")
            )
        ),
        tags$br(),
        actionButton("btn_reset_subtitleoptions","Reset panel",class="btn-sm btn-warning btn-block")
      )
      )
    }
  })
  
  # OBS: btn_reset_subtitleoptions ---------------------------------------------
  # observer to reset subtitle options
  
  observeEvent(input$btn_reset_subtitleoptions,{
    updateTextInput(session,"in_subtitlelab","Subtitle label",value="Subtitle")
    updateNumericInput(session,"in_subtitlelabsize","Text size",min=1,max=8,step=0.2,value=4)
    updateNumericInput(session,"in_subtitlelabspacer","Spacer",min=0.4,max=2.4,step=0.2,value=1.4)
    updateNumericInput(session,"in_subtitlelabhjust","Hor position",min=0,max=1,step=0.2,value=0)
    updateNumericInput(session,"in_subtitlelabvjust","Ver position",min=0,max=1,step=0.2,value=0.5)
    colourpicker::updateColourInput(session,"in_subtitlelabcol",label="Text colour",value="#505050")
  })
  
  # UI: ui_clusterlegendoptions ------------------------------------------------
  # ui for clusterlegend options
  
  output$ui_clusterlegendoptions <- renderUI({
    #req(input$in_showlegend)
    validate(fn_validate(try(input$in_showlegend),message1="ui_clusterlegendoptions: 'input$in_showlegend' is null."))

    if(input$in_showlegend)
    {
      divopenh4("div_legendoptions","> Legend options",
                divgrey(
        textInput("in_legendlab","Legend label(s)",value="",placeholder="label1,label2"),
        shinyBS::bsTooltip("in_legendlab",title="Input custom cluster labels here if required, separated by commas. Number of labels must be equal to max K value across all selected runs.",placement="top",trigger="hover"),
        selectInput("in_legendpos","Legend position",selectize=TRUE,multiple=FALSE,choices=c("Right","Left"),selected="Right"),
        shinyBS::bsTooltip("in_legendpos",title="Legend position.",placement="top",trigger="hover"),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_legendtextsize","Legend text size",min=0.2,max=8,step=0.2,value=3),
                shinyBS::bsTooltip("in_legendtextsize",title="Legend text size.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_legendkeysize","Legend key size",min=0.2,max=8,step=0.2,value=2),
                shinyBS::bsTooltip("in_legendkeysize",title="Legend key size.",placement="bottom",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_legendspacing","Legend spacing",min=0,max=8,step=0.2,value=2),
                shinyBS::bsTooltip("in_legendspacing",title="Spacing between legend items.",placement="bottom",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_legendrow","Legend rows",min=1,max=20,step=1,value=NA),
                shinyBS::bsTooltip("in_legendrow",title="Number of rows in legend.",placement="bottom",trigger="hover")
            )
        ),
        tags$br(),
        actionButton("btn_reset_legendoptions","Reset panel",class="btn-sm btn-warning btn-block")
      )
      )
    }
  })
  
  # OBS: btn_reset_legendoptions -----------------------------------------------
  # observer to reset legend options
  
  observeEvent(input$btn_reset_legendoptions,{
    updateTextInput(session,"in_legendlab","Legend label(s)",value="")
    updateSelectizeInput(session,"in_legendpos","Legend position",choices=c("Right","Left"),selected="Right")
    updateNumericInput(session,"in_legendtextsize","Legend text size",min=0.2,max=8,step=0.2,value=3)
    updateNumericInput(session,"in_legendkeysize","Legend key size",min=0.2,max=8,step=0.2,value=2)
    updateNumericInput(session,"in_legendspacing","Legend spacing",min=0,max=8,step=0.2,value=2)
    updateNumericInput(session,"in_legendrow","Legend rows",min=1,max=20,step=1,value=NA)
  })
  
  # UI: ui_grplabstdplotoptions ------------------------------------------------
  # ui for group label options for std plot
  
  output$ui_grplabstdplotoptions <- renderUI({
    #req(input$in_showgrplab)
    validate(fn_validate(try(input$in_showgrplab),message1="ui_grplabstdplotoptions: 'input$in_showgrplab' is null."))

    if(input$in_showgrplab)
    {
      divopenh3("div_grplaboptions","> Group label options",
        wellPanel(style="overflow-y:scroll; max-height: 400px",
                  divopenh4("div_generalgrpoptions","> General options",
                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  numericInput("in_glh","Panel height",min=0,max=2,value=0.5,step=0.1),
                                  shinyBS::bsTooltip("in_glh",title="Adjusts height of the group label panel.",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  numericInput("in_ls","Panel spacer",min=0,max=0.7,value=0,step=0.1),
                                  shinyBS::bsTooltip("in_ls",title="Label spacer adjusts spacing between the label panel and the plot(s) panel(s).",placement="bottom",trigger="hover")
                              )
                          ),
                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  uiOutput("ui_panelratio")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  colourpicker::colourInput("in_grpmarkercol",label="Label marker colour",value="#505050"),
                                  shinyBS::bsTooltip("in_grpmarkercol",title="Colour of the label points & label line.",placement="top",trigger="hover")
                              )
                          )
                          
                  ),
                  divopenh4("div_labeltextoptions","> Text options",
                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  numericInput("in_grplabpos","Text position",min=0,max=1,value=0.5,step=0.1),
                                  shinyBS::bsTooltip("in_grplabpos",title="Position of the text labels. To lower text,increase Label panel height first",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  colourpicker::colourInput("in_grplabtextcol",label="Text colour",value="#505050"),
                                  shinyBS::bsTooltip("in_grplabtextcol",title="Colour of the text labels.",placement="top",trigger="hover")
                              )
                          ),
                          div(class="row",
                              div(class="col-xs-4",style=list("padding-right: 5px; width: 34%"),
                                  numericInput("in_grplabsize","Size",min=1,max=5,value=NA,step=0.1),
                                  shinyBS::bsTooltip("in_grplabsize",title="Size of the text labels. Eg. 1.0-5.0.",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-4",style=list("padding-right: 5px; padding-left: 5px; width: 33%"),
                                  numericInput("in_grplabangle","Angle",min=0,max=180,value=0,step=1),
                                  shinyBS::bsTooltip("in_grplabangle",title="Angle of the text labels. Eg. 0-180.",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-4",style=list("padding-left: 5px; width: 33%"),
                                  numericInput("in_grplabjust","Justify",min=0,max=1,value=0.5,step=0.1),
                                  shinyBS::bsTooltip("in_grplabjust",title="Justification of the text labels. Eg. 0.0-1.0.",placement="bottom",trigger="hover")
                              )
                          )
                  ),
                  divopenh4("div_pointoptions","> Point options",
                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  numericInput("in_pointsize","Point size",min=0.5,max=5,value=NA,step=0.1),
                                  shinyBS::bsTooltip("in_pointsize",title="Size of the label points. Eg. 0.5-5.0.",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  textInput("in_pointtype","Point type",value="|"),
                                  shinyBS::bsTooltip("in_pointtype",title="Point type. Eg. 1,2,|,+ etc. See Guide.",placement="bottom",trigger="hover")
                              )
                          )
                  ),
                  divopenh4("div_lineoptions","> Line options",
                          sliderInput("in_linepos","Line position",min=0,max=1,value=1.0,step=0.1),
                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  numericInput("in_linesize","Line size",min=0.1,max=1,value=NA,step=0.1),
                                  shinyBS::bsTooltip("in_linesize",title="Thickness of the label line. Eg. 0.1-1.0.",placement="bottom",trigger="hover")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  textInput("in_linetype","Line type",value=1),
                                  shinyBS::bsTooltip("in_linetype",title="Label line type. Eg. 1,2,12 etc. See Guide.",placement="bottom",trigger="hover")
                              )
                          )
                  ),
                  divopenh4("div_divideroptions","> Divider options",
                          checkboxInput("in_showdiv","Divider line",value=TRUE),
                          shinyBS::bsTooltip("in_showdiv",title="Vertical lines to visually separate populations.",placement="right",trigger="hover"),
                          selectInput("in_divgrp","Div group label title",choices=store_plot_helper$grplabtitle,selectize=TRUE,multiple=TRUE,selected="None"),
                          shinyBS::bsTooltip("in_divgrp",title="Select one or more group label titles to draw divider lines.",placement="top",trigger="hover"),
                          colourpicker::colourInput("in_divcol",label="Colour",value="#FFFFFF"),
                          shinyBS::bsTooltip("in_divcol",title="Colour of the divider line.",placement="top",trigger="hover"),

                          div(class="row",
                              div(class="col-xs-6",style=list("padding-right: 5px;"),
                                  numericInput("in_divsize","Div size",min=0.1,max=1,value=NA,step=0.1),
                                  shinyBS::bsTooltip("in_divsize",title="Thickness of the divider line. Eg. 0.1-1.0.",placement="top",trigger="hover")
                              ),
                              div(class="col-xs-6",style=list("padding-left: 5px;"),
                                  textInput("in_divtype","Div type",value=21),
                                  shinyBS::bsTooltip("in_divtype",title="Divider line type. Eg. 1,2,12 etc. See Guide.",placement="top",trigger="hover")
                              )
                          )
                  ),
                  actionButton("btn_reset_grplabstdoptions","Reset panel",class="btn-sm btn-warning btn-block")
        )
      )
    }
  })
  
  
  # UI: ui_panelratio ---------------------------------------------------
  # ui for panel ratio (when using grplab)
  
  output$ui_panelratio <- renderUI({
    #req(input$in_showgrplab)
    validate(fn_validate(try(input$in_showgrplab),message1="ui_panelratio: 'input$in_showgrplab' is null."))
    
    div(
      textInput("in_panelratio","Panel ratio",value="3,1"),
      shinyBS::bsTooltip("in_panelratio",title="Ratio between barplot area and group label area. Default sets 3 units of barplot to 1 unit of group label.",placement="bottom",trigger="hover")
    )
  })
  
  # OBS: btn_reset_grplabstdoptions -----------------------------------------------------
  # observer to reset group label std options
  
  observeEvent(input$btn_reset_grplabstdoptions,{
    updateNumericInput(session,"in_glh","Group label panel height",min=0,max=2,value=0.5,step=0.1)
    updateNumericInput(session,"in_ls","Label spacer",min=0,max=0.7,value=0,step=0.1)
    colourpicker::updateColourInput(session,"in_grplabcol",label="Label marker colour",value="#505050")
    updateNumericInput(session,"in_grplabpos","Text position",min=0,max=1,value=0.2,step=0.1)
    colourpicker::updateColourInput(session,"grplabtextcol",label="Text colour",value="#505050")
    updateNumericInput(session,"in_grplabsize","Size",min=1,max=5,value=NA,step=0.1)
    updateNumericInput(session,"in_grplabangle","Angle",min=0,max=180,value=0,step=1)
    updateNumericInput(session,"in_grplabjust","Justify",min=0,max=1,value=0.5,step=0.1)
    updateNumericInput(session,"in_pointsize","Point size",min=0.5,max=5,value=NA,step=0.1)
    updateTextInput(session,"in_pointtype","Point type",value="|")
    updateSliderInput(session,"in_linepos","Line position",min=0,max=1,value=1.0,step=0.1)
    updateNumericInput(session,"in_linesize","Line size",min=0.1,max=1,value=NA,step=0.1)
    updateTextInput(session,"in_linetype","Line type",value=1)
    updateCheckboxInput(session,"in_showdiv","Divider line",value=TRUE)
    updateSelectizeInput(session,"in_divgrp","Div group label title",choices=store_plot_helper$grplabtitle,selected=store_plot_helper$grplabtitle[1])
    colourpicker::updateColourInput(session,"in_divcol",label="Colour",value="#FFFFFF")
    updateNumericInput(session,"in_divsize","Div size",min=0.1,max=1,value=NA,step=0.1)
    updateTextInput(session,"in_divtype","Div type",value=21)
    updateTextInput(session,"in_panelratio","Panel ratio",value="3,1")
  })
  
  # UI: ui_indlabstdplotoptions ------------------------------------------------
  # ui for ind label options for std plot
  
  output$ui_indlabstdplotoptions <- renderUI({
    req(input$in_useindlab)
    #validate(fn_validate(try(input$in_showindlab),message1="ui_indlabstdplotoptions: 'input$in_showindlab' is null."))
    
    if(input$in_useindlab) 
    {
      divopenh3("div_indlaboptions","> Individual label options",
        wellPanel(
                  uiOutput("ui_sharedindlab"),
                  div(class="row",
                      div(class="col-xs-6",style=list("padding-right: 5px;"),
                          numericInput("in_indlabheight","Panel height",min=0,max=2,value=0.5,step=0.1),
                          shinyBS::bsTooltip("in_indlabheight",title="Adjusts height of the individual label panel.",placement="bottom",trigger="hover")
                      ),
                      div(class="col-xs-6",style=list("padding-left: 5px;"),
                          numericInput("in_indlabspacer","Panel spacer",min=0,max=0.7,value=0,step=0.1),
                          shinyBS::bsTooltip("in_indlabspacer",title="Label spacer adjusts spacing between the label panel and the plot(s) panel(s).",placement="bottom",trigger="hover")
                      )
                  ),
                  div(class="row",
                      div(class="col-xs-6",style=list("padding-right: 5px;"),
                          numericInput("in_indlabsize","Label size",min=0.2,max=20,value=NA,step=0.1),
                          shinyBS::bsTooltip("in_indlabsize",title="Label size. A value between 0.2-20.0.",placement="bottom",trigger="hover")
                      ),
                      div(class="col-xs-6",style=list("padding-left: 5px;"),
                          colourpicker::colourInput("in_indlabcol",label="Label colour",value="#505050"),
                          shinyBS::bsTooltip("in_indlabcol",title="Colour of individual labels.",placement="top",trigger="hover")
                      )
                  ),
                  div(class="row",
                      div(class="col-xs-4",style=list("padding-right: 5px; width: 34%"),
                          numericInput("in_indlabvjust","V Justify",min=0,max=1,value=0.5,step=0.1),
                          shinyBS::bsTooltip("in_indlabvjust",title="Vertical justification of the text labels. Value between 0-1.",placement="top",trigger="hover")
                      ),
                      div(class="col-xs-4",style=list("padding-right: 5px; padding-left: 5px; width: 33%"),
                          numericInput("in_indlabhjust","H Justify",min=0,max=1,value=0.5,step=0.1),
                          shinyBS::bsTooltip("in_indlabhjust",title="Horizontal justification of the text labels. Value between 0-1.",placement="top",trigger="hover")
                      ),
                      div(class="col-xs-4",style=list("padding-left: 5px; width: 33%"),
                          numericInput("in_indlabangle","Angle",min=0,max=180,value=90,step=5),
                          shinyBS::bsTooltip("in_indlabangle",title="Angle of the text labels. Value between 0-180.",placement="top",trigger="hover")
                      )
                  ),
                  actionButton("btn_reset_indlabstdoptions","Reset panel",class="btn-sm btn-warning btn-block")
        )
      )
    }
  })
  
  # OBS: btn_reset_indlabstdoptions -----------------------------------------------------
  # observer to reset ind label std options
  
  observeEvent(input$btn_reset_indlabstdoptions,{
    updateCheckboxInput(session,"in_showindlab","Show individual labels",value=TRUE)
    updateCheckboxInput(session,"in_sharedindlab","Common individual labels",value=TRUE)
    updateNumericInput(session,"in_indlabheight","Panel height",min=0,max=2,value=0.5,step=0.1)
    updateNumericInput(session,"in_indlabspacer","Panel spacer",min=0,max=0.7,value=0,step=0.1)
    updateNumericInput(session,"in_indlabsize","Label size",min=0.2,max=5,value=NA,step=0.1)
    colourpicker::updateColourInput(session,"indlabcol",label="Label colour",value="#505050")
    updateNumericInput(session,"in_indlabvjust","V Justify",min=0,max=1,value=0.5,step=0.1)
    updateNumericInput(session,"in_indlabhjust","H Justify",min=0,max=1,value=0.5,step=0.1)
    updateNumericInput(session,"in_indlabangle","Angle",min=0,max=180,value=90,step=5)
  })
  
  # UI: ui_sharedindlab --------------------------------------------------------
  # ui for shared individual labels option
  
  output$ui_sharedindlab <- renderUI({
    req(store_plot_helper$selected_run)
    req(input$in_sortind)
    
    if(length(store_plot_helper$selected_run)>1){
      if(input$in_sortind == "None" || input$in_sortind == "Label"){
        div(
          checkboxInput("in_sharedindlab","Common individual labels",value=TRUE),
          shinyBS::bsTooltip("in_sharedindlab",title="Should individual labels be displayed under each panel separately or a common label under the bottom panel.",placement="right",trigger="hover")
        )
      }else{
        div(
          checkboxInput("in_sharedindlab","Common individual labels",value=FALSE),
          shinyBS::bsTooltip("in_sharedindlab",title="Should individual labels be displayed under each panel separately or a common label under the bottom panel.",placement="right",trigger="hover")
        )
      }
      }
    
  })
  
  # UI: ui_downloadplotoptions -------------------------------------------------
  # ui to download std plots
  
  output$ui_downloadplotoptions <- renderUI({
    req(store_plot_helper$selected_run)
    #validate(fn_validate(try(store_plot_helper$selected_run),message1="ui_downloadplotoptions: 'store_plot_helper$selected_run' is null."))
    #if (input$in_tabset_plot != "Standard Plot") return(NULL)
    
    div(
      h3("> Download options"),
      wellPanel(
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                numericInput("in_height","Height (cm)",min=0.1,max=10.0,step=0.1,value=NA),
                shinyBS::bsTooltip("in_height",title="Height of one run panel in the figure. Eg. 0.1-10.0.",placement="top",trigger="hover")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                numericInput("in_width","Width (cm)",min=1,max=50,step=1,value=NA),
                shinyBS::bsTooltip("in_width",title="Width of the figure. Eg. 1-50.",placement="right",trigger="hover")
            )
        ),
        div(class="row",
            div(class="col-xs-6",style=list("padding-right: 5px;"),
                selectInput("in_res","Res/DPI",choices=c("200","300","400","500"),selected="200")
            ),
            div(class="col-xs-6",style=list("padding-left: 5px;"),
                selectInput("in_format","File type",choices=c("png","tiff","jpeg","pdf"),selected="png",multiple=FALSE,selectize=TRUE),
                shinyBS::bsTooltip("in_format",title="Only applies to download. Change in filetype is not shown in preview.",placement="right",trigger="hover")
            )
        ),
        downloadButton("btn_downloadplotsp","Download Plot")
      )
    )
  })
  
  ## PLOT FN ===================================================================
  
  # RFN: fn_sl_colours ---------------------------------------------------------
  # reactive function to get colours
  
  fn_sl_colours <- reactive({
    
    cat("Running fn_sl_colours() ...\n")
    
    maxk <- max(store_plot_helper$selected_tabulateq$k)
    customcols <- vector("character")
    for(i in 1:maxk)
    {
      customcols <- c(customcols,eval(parse(text=paste0("input$in_col",i))))
    }

    return(customcols)
  })
  
  # RFN: fn_sl_indlab ----------------------------------------------------------
  # reactive function to get individual labels
  
  fn_sl_indlab <- reactive({
    #req(input$in_useindlab)
    validate(fn_validate(try(input$in_useindlab),message1="fn_sl_indlab: 'input$in_useindlab' is null."))
    
    if(input$in_useindlab)
    {
      validate(fn_validate(try(input$in_indlabtype),message1="Argument 'indlabtype' missing."))
      
      if(input$in_indlabtype=="From input data")
      {
        # get ind lab from input data
        indlab <- rownames(store_plot_helper$qlist[[1]])
      }
      
      if(input$in_indlabtype=="Upload file")
      {
        validate(fn_validate(try(input$in_indlabupload),message1="Upload an individual labels text file. \n\nThe file must be tab-delimited or comma-separated."))
        if (is.null(input$in_indlabupload)) stop("Upload an individual labels text file. \n\nThe file must be tab-delimited or comma-separated.")
        
        ext <- input$in_indlabupload[["name"]]
        ext <- substr(ext,regexpr("\\.",ext)[1],regexpr("$",ext)[1])
        if(ext==".txt") indlab <- read.delim(input$in_indlabupload[["datapath"]],header=FALSE,stringsAsFactors=FALSE)[,1]
        if(ext==".csv") indlab <- read.csv(input$in_indlabupload[["datapath"]],header=FALSE,stringsAsFactors=FALSE)[,1]
      }
      
      if(input$in_indlabtype=="Paste text")
      {
        validate(fn_validate(try(input$in_indlabpaste),message3="Paste individual labels. \n\nCopy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line without header."))
        if (nchar(input$in_indlabpaste[1])==0) stop("Paste individual labels. \n\nCopy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line.")
        indlab <- as.character(unlist(strsplit(input$in_indlabpaste,"\n")))
      }
      
      validate(fn_validate_equal(any(duplicated(indlab)),FALSE,paste0("Individual names must be unique and not duplicated. Below are duplicate individual names:\n\n",paste0(indlab[which(duplicated(indlab))],collapse=","))))
      
      validate(fn_validate_equal(length(indlab),try(store_general$tabulateq$ind),message=paste0("Length of individual labels (",length(indlab),") not equal to number of individuals (",store_general$tabulateq$ind,").")))
      
      store_plot_helper$indlab <- indlab
    }
  })
  
  # RFN: fn_sl_grplab ----------------------------------------------------------
  # reactive function to get grplab
  
  fn_sl_grplab <- reactive({
    #req(input$in_showgrplab)
    #req(input$in_grplabtype)
    validate(fn_validate(try(input$in_showgrplab),message1="fn_sl_grplab: 'input$in_showgrplab' is null."))
    validate(fn_validate(try(input$in_grplabtype),message1="fn_sl_grplab: 'input$in_grplabtype' is null."))
    
    if(input$in_showgrplab)
    {
      if(input$in_grplabtype=="Upload file")
      {
        validate(fn_validate(try(input$in_grplabupload),message1="Upload a group label text file. \n\nThe file must be a tab-delimited or comma-separated text file. The file must have a header."))
        
        inputgrpdata <- input$in_grplabupload
        ext <- inputgrpdata[["name"]]
        ext <- substr(ext,regexpr("\\.",ext)[1],regexpr("$",ext)[1])
        if(ext==".txt") grplab <- read.delim(inputgrpdata[["datapath"]],header=TRUE,stringsAsFactors=FALSE)
        if(ext==".csv") grplab <- read.csv(inputgrpdata[["datapath"]],header=TRUE,stringsAsFactors=FALSE)
      }
      
      if(input$in_grplabtype=="Paste text")
      {
        validate(fn_validate(try(input$in_grplabpaste),message3="Paste group labels. \n\nCopy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line without header. Note that only one set of group labels can be input here. For multiple sets of group labels, use 'Upload file' option with headers."))
        if (nchar(input$in_grplabpaste[1])==0) stop("Paste group labels. \n\nCopy-paste labels using keyboard from a spreadsheet or text editor. One column and one label per line without header. Note that only one set of group labels can be input here. For multiple sets of group labels, use 'Upload file' option with headers.")
        grplab <- data.frame(grplab=as.character(unlist(strsplit(input$in_grplabpaste,"\n"))),stringsAsFactors=F)
      }
      
      validate(fn_validate_equal(try(unique(sapply(grplab,length))),try(store_general$tabulateq$ind),message=paste0("Length of group labels (",unique(sapply(grplab,length)),") not equal to number of individuals (",store_general$tabulateq$ind,").")))
      
      store_plot_helper$grplab <- grplab
    }
  })
  
  # OBSEV ----------------------------------------------------------------------
  # update inorder,grporder,grpset
  
  observeEvent(store_plot_helper$sortind,{
    updateSelectizeInput(session,"in_sortind",label="Order individuals",
                         choices=as.character(store_plot_helper$sortind),selected="None")
  })
  
  observeEvent(store_plot_helper$grplabtitle,{
    updateSelectizeInput(session,"in_selectgrp",label="Active group label title",
                         choices=store_plot_helper$grplabtitle,selected=store_plot_helper$grplabtitle[1])
  })
  
  observeEvent(store_plot_helper$grplabtext,{
    updateSelectizeInput(session,"in_subsetgrp",label="Subset/Order group label text",
                         choices=store_plot_helper$grplabtext,selected="None")
  })
  
  observeEvent(store_plot_helper$grplabtext,{
    updateSelectizeInput(session,"in_divgrp",label="Div group label text",
                         choices=store_plot_helper$grplabtitle,selected=store_plot_helper$grplabtitle[1])
  })
  
  # FN: fn_sl_clumpp ----------------------------------------------------------
  # reactive function to get selected qlist
  
  fn_sl_clumpp <- function(qlist=NULL) {
    
    tq <- tabulateQ(qlist,sorttable=F)
    validate(fn_validate_equal(try(sum(diff(tq$ind))),0,message="Cannot Align/Merge runs. Number of individuals differ between selected runs."))
    validate(fn_validate_equal(try(sum(diff(tq$k))),0,message="Cannot Align/Merge runs. Value of K differs between selected runs."))
    k <- tq$k[1]
    
    # if(file.exists(paste0(store_general$newwd,"/clumpp/clumpp_k",k,"-aligned.txt"))) file.remove(paste0(store_general$newwd,"/clumpp-aligned-data.txt"))
    # if(file.exists(paste0(store_general$newwd,"/clumpp/clumpp_k",k,"-merged.txt"))) file.remove(paste0(store_general$newwd,"/clumpp-merged-data.txt"))
    # if(file.exists(paste0(store_general$newwd,"/clumpp/clumpp_k",k,".txt"))) file.remove(paste0(store_general$newwd,"/clumpp-combined-data.txt"))
    # if(file.exists(paste0(store_general$newwd,"/clumpp/clumpp_k",k,"-miscfile.txt"))) file.remove(paste0(store_general$newwd,"/clumpp-misc-file.txt"))
    # if(file.exists(paste0(store_general$newwd,"/clumpp/paramfile"))) file.remove(paste0(store_general$newwd,"/paramfile"))
    
    #if(is.null(store_plot_helper$qlist_aligned)){
      clumppExportWa(qlist=qlist,useexe=TRUE,exd=rootwd,currwd=store_general$newwd)
    #}else{
    #  if(!identical(names(qlist),names(store_plot_helper$qlist_aligned))) clumppExportWa(qlist=qlist,useexe=TRUE,currwd=store_general$newwd)
    #}
    
    #Align repeats
    if(input$in_align=="Align repeats")
    {
      sl_qlist <- readQWa(files=paste0(store_general$newwd,"/clumpp/clumpp_k",k,"-aligned.txt"),
                                                 filenames=paste0("clumpp_k",k,"-aligned"),
                                                 filetype="clumpp",indlabfromfile=FALSE)
      names(sl_qlist) <- paste0(names(qlist),"-Aligned")
      #sl_qlist <- store_plot_helper$qlist_aligned
      #store_plot_helper$selected_tabulateq <- tabulateQ(sl_qlist)
      sl_imgoutput <- "join"
    }
    
    #Merge repeats
    if(input$in_align=="Merge repeats")
    {
      sl_qlist <- readQWa(files=paste0(store_general$newwd,"/clumpp/clumpp_k",k,"-merged.txt"),
                                                filenames=paste0("clumpp_k",k,"-merged"),
                                                filetype="clumpp",indlabfromfile=FALSE)
      names(sl_qlist) <- paste0("K",k,"-Merged")
      #sl_qlist <- store_plot_helper$qlist_merged
      #store_plot_helper$selected_tabulateq <- tabulateQ(sl_qlist)
      sl_imgoutput <- "sep"
    }
    
    return(list("qlist"=sl_qlist,"imgoutput"=sl_imgoutput))
  }
  
  # RFN: fn_sl_core ------------------------------------------------------------
  # generates core parameters
  
  fn_sl_core <- reactive({
    #req(fn_sl_run())
    
    cat("Running fn_sl_core() ...\n")
    
    fn_getfilenames()
    fn_readq()
    fn_tabulateq()
    fn_summariseq()
    
    validate(fn_validate(try(input$table_selectrunplot_rows_selected),message1="Select one or more files to plot."))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Computing core parameters...",value=0.1)
    
    # use indices to find selected run names
    store_plot_helper$selected_run <- store_general$tabulateq[input$table_selectrunplot_rows_selected,]$file
    # find runs from qlist
    store_plot_helper$qlist <- store_general$qlist[match(store_plot_helper$selected_run,names(store_general$qlist))]
    sl_qlist <- store_plot_helper$qlist
    
    # get imgoutput
    if(length(store_plot_helper$selected_run)==1) sl_imgoutput <- "sep"
    if(length(store_plot_helper$selected_run)>1) sl_imgoutput <- "join"
    
    # get indlab
    #validate(fn_validate(try(input$in_useindlab),message1="Argument 'in_useindlab' missing."))
    if(is.null(input$in_useindlab)) {sl_useindlab <- FALSE}else{sl_useindlab <- input$in_useindlab}
    if(sl_useindlab){
      
      # check if number of ind are same across all runs
      if(length(unique(sapply(sl_qlist,nrow)))>1) stop("Selected runs differ in number of individuals.")
      
      fn_sl_indlab()
      
      #validate(fn_validate(try(input$in_showgrplab),message1="Argument 'in_showgrplab' missing."))
      if(is.null(input$in_showgrplab)) {showgrplab <- FALSE}else{showgrplab <- input$in_showgrplab}
      if(showgrplab){
        #validate(fn_validate(try(input$in_indlabwithgrplab),message1="Argument 'in_indlabwithgrplab' missing."))
        if(is.null(input$in_indlabwithgrplab)) {sl_indlabwithgrplab <- FALSE}else{sl_indlabwithgrplab <- input$in_indlabwithgrplab}
        if(sl_indlabwithgrplab){
          #validate(fn_validate(try(input$in_indlabsep),message1="Argument 'in_indlabsep' missing."))
          if(is.null(input$in_indlabsep)) {sl_indlabsep <- " "}else{sl_indlabsep <- input$in_indlabsep}
        }else{
          sl_indlabsep <- " "
        }
      }else{
        sl_indlabwithgrplab <- FALSE
        sl_indlabsep <- " "
      }
      
      #validate(fn_validate(try(input$in_showindlab),message1="Argument 'in_showindlab' missing."))
      if(is.null(input$in_showindlab)) {sl_showindlab <- FALSE}else{sl_showindlab <- input$in_showindlab}
      
    }else{
      sl_indlabwithgrplab <- FALSE
      sl_indlabsep <- " "
      sl_showindlab <- FALSE
    }
    
    progress$inc(0.3,message="Computing core parameters...")
    
    # dimensions
    if(sl_imgoutput=="sep"){
      store_plot_helper$selected_tabulateq <- tabulateQ(sl_qlist,sorttable=FALSE)
    }
    
    # joined plot specific
    if(sl_imgoutput=="join")
    {
      #validate(fn_validate(try(input$in_align),message1="Argument 'in_align' missing."))
      if(is.null(input$in_align)) {wa_align <- "None"}else{wa_align <- input$in_align}
      
      #WITHOUT CLUMPP OPTION
      if(wa_align=="None")
      {
        store_plot_helper$selected_tabulateq <- tabulateQ(sl_qlist,sorttable=FALSE)
      }
      
      #WITH CLUMPP OPTION
      if(wa_align != "None")
      {
        temp <- fn_sl_clumpp(qlist=sl_qlist)
        # copy ind lab from original qlist to clumpp output qlist
        temp$qlist <- lapply(temp$qlist,function(dfr) {rownames(dfr) <- rownames(sl_qlist[[1]]); dfr})
        sl_qlist <- temp$qlist
        sl_imgoutput <- temp$imgoutput
        store_plot_helper$selected_tabulateq <- tabulateQ(sl_qlist,sorttable=FALSE)
      }
    }
    
    progress$inc(0.6,message="Computing core parameters...")
    
    # get colours
    sl_clustercol <- fn_sl_colours()
    
    # update in_sortind
    store_plot_helper$sortind <- c("None","Label","All",paste0("Cluster",1:min(sapply(sl_qlist,ncol))))
    
    # get sortind
    #validate(fn_validate(try(input$in_sortind),message1="Argument 'in_sortind' missing."))
    if(is.null(input$in_sortind)){
      sl_sortind <- NA
    }else{
      sl_sortind <- input$in_sortind
      if("None" %in% sl_sortind) {
        sl_sortind <- NA
      }else{
        sl_sortind <- gsub("^Label$","label",gsub("^All$","all",sl_sortind))
      }
    }

    progress$inc(0.8,message="Computing core parameters...")
    
    # get grplab
    if(is.null(input$in_showgrplab)) {showgrplab <- FALSE}else{showgrplab <- input$in_showgrplab}
    if(showgrplab){
      
      # check if number of ind are same across all runs
      if(length(unique(sapply(sl_qlist,nrow)))>1) stop("Selected runs differ in number of individuals.")
      
      fn_sl_grplab()
      sl_grplab <- store_plot_helper$grplab
      
      # create grplab title (for selectgrp)
      store_plot_helper$grplabtitle <- colnames(store_plot_helper$grplab)
      
      #validate(fn_validate(try(input$in_selectgrp),message1="Argument 'in_selectgrp' missing."))
      if(is.null(input$in_selectgrp)){
        sl_selgrp <- NA
        store_plot_helper$grplabtext <- "None"
      }else{
        if("None" %in% input$in_selectgrp){
          sl_selgrp <- NA
          store_plot_helper$grplabtext <- "None"
        }else{
          sl_selgrp <- input$in_selectgrp
          subsetgrp <- unique(as.character(unlist(store_plot_helper$grplab[,sl_selgrp,drop=FALSE])))
          store_plot_helper$grplabtext <- c("None",subsetgrp)
        }
      }
      
      #validate(fn_validate(try(input$in_ordergrp),message1="Argument 'in_ordergrp' missing."))
      if(is.null(input$in_ordergrp)){sl_ordergrp <- FALSE}else{sl_ordergrp <- input$in_ordergrp}
      #validate(fn_validate(try(input$in_subsetgrp),message1="Argument 'in_subsetgrp' missing."))
      if(is.null(input$in_subsetgrp)){
        sl_subsetgrp <- NA
      }else{
          sl_subsetgrp <- input$in_subsetgrp
          if(("None" %in% sl_subsetgrp) || (sl_subsetgrp == "")) {
            sl_subsetgrp <- NA
          }else{
            validate(fn_validate_subsetgrp(input1=sl_selgrp,input2=store_plot_helper$grplab,input3=sl_ordergrp))
          }
        }
      
      #validate(fn_validate(try(input$in_grpmean),message1="Argument 'in_grpmean' missing."))
      if(is.null(input$in_grpmean)){sl_grpmean <- FALSE}else{sl_grpmean <- input$in_grpmean}
      
    }else
    {
      sl_grplab <- NA
      sl_selgrp <- NA
      sl_ordergrp <- FALSE
      sl_subsetgrp <- NA
      sl_grpmean <- FALSE
    }
    
    progress$inc(0.9,message="Computing core parameters...")
    
    return(list(qlist=sl_qlist,
                imgoutput=sl_imgoutput,
                useindlab=sl_useindlab,
                showindlab=sl_showindlab,
                indlabwithgrplab=sl_indlabwithgrplab,
                indlabsep=sl_indlabsep,
                clustercol=sl_clustercol,
                sortind=sl_sortind,
                grplab=sl_grplab,
                selgrp=sl_selgrp,
                ordergrp=sl_ordergrp,
                subsetgrp=sl_subsetgrp,
                grpmean=sl_grpmean))
  })
  
  # RFN: fn_sl_std ------------------------------------------------------------
  # generates std plot parameters
  
  fn_sl_std <- function(){
    
    cat("Running fn_sl_std() ...\n")
    
    cparams <- fn_sl_core()
    sl_imgoutput <- cparams$imgoutput
    sl_sortind <- cparams$sortind

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Computing std plot parameters...",value=0.1)
    
    #validate(fn_validate(try(input$in_useindlab),message1="Argument 'in_useindlab' missing."))
    if(is.null(input$in_useindlab)){sl_useindlab <- FALSE}else{sl_useindlab <- input$in_useindlab}
    if(sl_useindlab){
      
      #validate(fn_validate(try(input$in_showindlab),message1="Argument 'in_showindlab' missing."))
      if(is.null(input$in_showindlab)){sl_showindlab <- FALSE}else{sl_showindlab <- input$in_showindlab}
      
      if(sl_showindlab)
      {
        #validate(fn_validate(try(input$in_indlabheight),message1="Argument 'in_indlabheight' missing."))
        if(is.null(input$in_indlabheight)){sl_indlabheight <- 0.2}else{sl_indlabheight <- input$in_indlabheight}
        #validate(fn_validate(try(input$in_indlabspacer),message1="Argument 'in_indlabspacer' missing."))
        if(is.null(input$in_indlabspacer)){sl_indlabspacer <- 1.5}else{sl_indlabspacer <- input$in_indlabspacer}
        #validate(fn_validate(try(input$in_indlabsize),message1="Argument 'in_indlabsize' missing."))
        if(is.null(input$in_indlabsize)){sl_indlabsize <- NULL}else{sl_indlabsize <- input$in_indlabsize}
        if(is.na(sl_indlabsize)) sl_indlabsize <- NULL
        #validate(fn_validate(try(input$in_indlabcol),message1="Argument 'in_indlabcol' missing."))
        if(is.null(input$in_indlabcol)){sl_indlabcol <- "grey30"}else{sl_indlabcol <- input$in_indlabcol}
        #validate(fn_validate(try(input$in_indlabvjust),message1="Argument 'in_indlabvjust' missing."))
        if(is.null(input$in_indlabvjust)){sl_indlabvjust <- 0.5}else{sl_indlabvjust <- input$in_indlabvjust}
        #validate(fn_validate(try(input$in_indlabhjust),message1="Argument 'in_indlabhjust' missing."))
        if(is.null(input$in_indlabhjust)){sl_indlabhjust <- 1}else{sl_indlabhjust <- input$in_indlabhjust}
        #validate(fn_validate(try(input$in_indlabangle),message1="Argument 'in_indlabangle' missing."))
        if(is.null(input$in_indlabangle)){sl_indlabangle <- 90}else{sl_indlabangle <- input$in_indlabangle}
        
        if(sl_imgoutput=="join"){
          #validate(fn_validate(try(input$in_sharedindlab),message1="Argument 'in_sharedindlab' missing."))
          if(is.null(input$in_sharedindlab)){sl_sharedindlab <- FALSE}else{sl_sharedindlab <- input$in_sharedindlab}
        }else{
          sl_sharedindlab <- FALSE
        }
      }else{
        sl_sharedindlab <- FALSE
        sl_indlabheight <- 0.2
        sl_indlabspacer <- 1.5
        sl_indlabsize <- NULL
        sl_indlabcol <- "grey30"
        sl_indlabvjust <- 0.5
        sl_indlabhjust <- 1
        sl_indlabangle <- 90
      }
      
    }else{
      sl_showindlab <- FALSE
      sl_sharedindlab <- FALSE
      sl_indlabheight <- 0.2
      sl_indlabspacer <- 1.5
      sl_indlabsize <- NULL
      sl_indlabcol <- "grey30"
      sl_indlabvjust <- 0.5
      sl_indlabhjust <- 1
      sl_indlabangle <- 90
    }
    
    progress$inc(0.2,message="Computing std plot parameters...")
    
    # joined plot specific
    if(sl_imgoutput=="sep") sl_panelspacer <- 0.1
    if(sl_imgoutput=="join")
    {
      #validate(fn_validate(try(input$in_ps),message1="Argument 'in_ps' missing."))
      if(is.null(input$in_ps)) {sl_panelspacer <- 0.1}else{sl_panelspacer <- as.numeric(input$in_ps)}
    }
    
    # check if sharedindlab is on when sorting by all,cluster for joined plots
    if((!is.na(sl_sortind)) && sl_imgoutput=="join"){
      validate(fn_validate_sharedindlab(sharedindlab=sl_sharedindlab,sortind=sl_sortind))
      if(sl_sortind!="label" && sl_sharedindlab) stop("'Common individual labels' must be FALSE (unchecked), when individuals are ordered by 'all' or a cluster.")
    }
    
    # grplab
    if(input$in_showgrplab){
      
      #validate(fn_validate(try(input$in_glh),message1="Argument 'in_glh' missing."))
      if(is.null(input$in_glh)){sl_grplabheight <- NA}else{sl_grplabheight <- input$in_glh}
      #validate(fn_validate(try(input$in_ls),message1="Argument 'in_ls' missing."))
      if(is.null(input$in_ls)){sl_grplabspacer <- 0}else{sl_grplabspacer <- input$in_ls}
      #validate(fn_validate(try(input$in_grpmarkercol),message1="Argument 'in_grpmarkercol' missing."))
      if(is.null(input$in_grpmarkercol)){grpmarkercol <- "grey30"}else{grpmarkercol <- input$in_grpmarkercol}
      #validate(fn_validate(try(input$in_grplabpos),message1="Argument 'in_grplabpos' missing."))
      if(is.null(input$in_grplabpos)){sl_grplabpos <- 0.25}else{sl_grplabpos <- input$in_grplabpos}
      #validate(fn_validate(try(input$in_grplabtextcol),message1="Argument 'in_grplabtextcol' missing."))
      if(is.null(input$in_grplabtextcol)){grplabtextcol <- "grey30"}else{grplabtextcol <- input$in_grplabtextcol}
      
      #validate(fn_validate(try(input$in_pointtype),message1="Argument 'in_pointtype' missing."))
      if(is.null(input$in_pointtype)){
        sl_pointtype <- "|"
      }else{
        sl_pointtype <- suppressWarnings(as.numeric(input$in_pointtype))
        if(is.na(sl_pointtype)) sl_pointtype <- as.character(input$in_pointtype)
      }

      #validate(fn_validate(try(input$in_linepos),message1="Argument 'in_linepos' missing."))
      if(is.null(input$in_linepos)){sl_linepos <- 0.75}else{sl_linepos <- input$in_linepos}
      #validate(fn_validate(try(input$in_linetype),message1="Argument 'in_linetype' missing."))
      if(is.null(input$in_linetype)){
        sl_linetype <- 1
      }else{
        sl_linetype <- suppressWarnings(as.numeric(input$in_linetype))
        if(sl_linetype > 10 | is.na(sl_linetype)) sl_linetype <- as.character(input$in_linetype)
      }

      #validate(fn_validate(try(input$in_showdiv),message1="Argument 'in_showdiv' missing."))
      if(is.null(input$in_showdiv)){sl_showdiv <- TRUE}else{sl_showdiv <- input$in_showdiv}
      if(sl_showdiv)
      {
        #validate(fn_validate(try(input$in_divcol),message1="Argument 'in_divcol' missing."))
        if(is.null(input$in_divcol)){sl_divcol <- "white"}else{sl_divcol <- input$in_divcol}
        #validate(fn_validate(try(input$in_divsize),message1="Argument 'in_divsize' missing."))
        if(is.null(input$in_divsize)){sl_divsize <- NA}else{sl_divsize <- input$in_divsize}
        #validate(fn_validate(try(input$in_divtype),message1="Argument 'in_divtype' missing."))
        if(is.null(input$in_divtype)){
          sl_divtype <- "21"
        }else{
          sl_divtype <- suppressWarnings(as.numeric(input$in_divtype))
          if(sl_divtype > 10 | is.na(sl_divtype)) sl_divtype <- as.character(input$in_divtype)
        }

        #validate(fn_validate(try(input$in_divgrp),message1="Argument 'in_divgrp' missing."))
        if(is.null(input$in_divgrp)) {sl_divgrp <- "None"}else{sl_divgrp <- input$in_divgrp}
        if("None" %in% sl_divgrp){
          sl_divgrp <- NA
        }
        
      }else{
        sl_divgrp <- NA
        sl_divcol <- "white"
        sl_divsize <- NA
        sl_divtype <- "21"
      }
      
      #validate(fn_validate(try(input$in_grplabsize),message1="Argument 'in_grplabsize' missing."))
      if(is.null(input$in_grplabsize)){sl_grplabsize <- NA}else{sl_grplabsize <- input$in_grplabsize}
      #validate(fn_validate(try(input$in_grplabangle),message1="Argument 'in_grplabangle' missing."))
      if(is.null(input$in_grplabangle)){sl_grplabangle <- NA}else{sl_grplabangle <- input$in_grplabangle}
      #validate(fn_validate(try(input$in_grplabjust),message1="Argument 'in_grplabjust' missing."))
      if(is.null(input$in_grplabjust)){sl_grplabjust <- NA}else{sl_grplabjust <- input$in_grplabjust}
      #validate(fn_validate(try(input$in_pointsize),message1="Argument 'in_pointsize' missing."))
      if(is.null(input$in_pointsize)){sl_pointsize <- NA}else{sl_pointsize <- input$in_pointsize}
      #validate(fn_validate(try(input$in_linesize),message1="Argument 'in_linesize' missing."))
      if(is.null(input$in_linesize)){sl_linesize <- NA}else{sl_linesize <- input$in_linesize}
      
      ppar <- getPlotParams(grplab=cparams$grplab,plotnum=length(store_plot_helper$selected_run),
                                        grplabsize=sl_grplabsize,grplabangle=sl_grplabangle,
                                        grplabjust=sl_grplabjust,pointsize=sl_pointsize,
                                        linesize=sl_linesize)
      
      sl_grplabsize <- ppar$grplabsize
      sl_grplabangle <- ppar$grplabangle
      sl_grplabjust <- ppar$grplabjust
      sl_pointsize <- ppar$pointsize
      sl_linesize <- ppar$linesize
      
      #validate(fn_validate(try(input$in_panelratio),message1="Argument 'in_panelratio' missing."))
      if(is.null(input$in_panelratio)){
        sl_panelratio <- c(3,1)
      }else{
        sl_panelratio <- as.numeric(unlist(strsplit(input$in_panelratio,",")))
      }
      
    }else
    {
      sl_grplabspacer <- 0
      sl_grplabheight <- NA
      grpmarkercol <- "grey30"
      sl_grplabpos <- 0.25
      grplabtextcol <- "grey30"
      sl_grplabsize <- NA
      sl_grplabangle <- NA
      sl_grplabjust <- NA
      sl_pointsize <- NA
      sl_pointtype <- "|"
      sl_linepos <- 0.75
      sl_linesize <- NA
      sl_linetype <- 1
      sl_showdiv <- TRUE
      sl_divgrp <- NA
      sl_divcol <- "white"
      sl_divsize <- NA
      sl_divtype <- "21"
      sl_panelratio <- c(3,1)
    }
    
    progress$inc(0.6,message="Computing std plot parameters...")
    
    # side panel
    #validate(fn_validate(try(input$in_showsp),message1="Argument 'in_showsp' missing."))
    if(is.null(input$in_showsp)){sl_showsp <- TRUE}else{sl_showsp <- input$in_showsp}
    if(sl_showsp)
    {
      #validate(fn_validate(try(input$in_splabsize),message1="Argument 'in_splabsize' missing."))
      if(is.null(input$in_splabsize)){sl_splabsize <- NULL}else{sl_splabsize <- input$in_splabsize}
      #validate(fn_validate(try(input$in_splabpos),message1="Argument 'in_splabpos' missing."))
      if(is.null(input$in_splabpos)){sl_sppos <- "right"}else{sl_sppos <- tolower(as.character(input$in_splabpos))}
      #validate(fn_validate(try(input$in_splabcol),message1="Argument 'in_splabcol' missing."))
      if(is.null(input$in_splabcol)){sl_splabcol <- "grey30"}else{sl_splabcol <- input$in_splabcol}
      #validate(fn_validate(try(input$in_spbgcol),message1="Argument 'in_spbgcol' missing."))
      if(is.null(input$in_spbgcol)){sl_spbgcol <- NA}else{sl_spbgcol <- input$in_spbgcol}
      
      #validate(fn_validate(try(input$in_splab),message1="Argument 'in_splab' missing."))
      sl_splab <- input$in_splab
      if(is.null(sl_splab)){
        sl_splab <- NA
      }else{
        if(sl_splab=="")
        {
          sl_splab <- NA
        }else{
          sl_splab <- as.character(unlist(strsplit(as.character(input$in_splab),",")))
          validate(fn_validate_equal(length(sl_splab),length(cparams$qlist),"Number of side panel labels are not equal to the number of plotted runs."))
        }
      }

      
    }else{
      sl_splab <- NA
      sl_sppos <- "right"
      sl_splabsize <- NULL
      sl_splabcol <- "grey30"
      sl_spbgcol <- NA
    }
    
    progress$inc(0.7,message="Computing std plot parameters...")
    
    # title
    #validate(fn_validate(try(input$in_showtitle),message1="Argument 'in_showtitle' missing."))
    if(is.null(input$in_showtitle)){sl_showtitle <- FALSE}else{sl_showtitle <- input$in_showtitle}
    if(sl_showtitle)
    {
      #validate(fn_validate(try(input$in_titlelabsize),message1="Argument 'in_titlelabsize' missing."))
      if(is.null(input$in_titlelabsize)){sl_titlesize <- NULL}else{sl_titlesize <- input$in_titlelabsize}
      #validate(fn_validate(try(input$in_titlelabspacer),message1="Argument 'in_titlelabspacer' missing."))
      if(is.null(input$in_titlelabspacer)){sl_titlespacer <- 1.4}else{sl_titlespacer <- input$in_titlelabspacer}
      #validate(fn_validate(try(input$in_titlelabhjust),message1="Argument 'in_titlelabhjust' missing."))
      if(is.null(input$in_titlelabhjust)){sl_titlehjust <- 0}else{sl_titlehjust <- input$in_titlelabhjust}
      #validate(fn_validate(try(input$in_titlelabvjust),message1="Argument 'in_titlelabvjust' missing."))
      if(is.null(input$in_titlelabvjust)){sl_titlevjust <- 0.5}else{sl_titlevjust <- input$in_titlelabvjust}
      #validate(fn_validate(try(input$in_titlelabcol),message1="Argument 'in_titlelabcol' missing."))
      if(is.null(input$in_titlelabcol)){sl_titlecol <- "grey30"}else{sl_titlecol <- input$in_titlelabcol}
      
      #validate(fn_validate(try(input$in_titlelab),message1="Argument 'titlelab' missing."))
      if(is.null(input$in_titlelab)){sl_titlelab <- FALSE}else{sl_titlelab <- input$in_titlelab}
      
    }else{
      sl_titlelab <- NA
      sl_titlesize <- NULL
      sl_titlespacer <- 1.4
      sl_titlehjust <- 0
      sl_titlevjust <- 0.5
      sl_titlecol <- "grey30"
    }
    
    progress$inc(0.8,message="Computing std plot parameters...")
    
    # subtitle
    #validate(fn_validate(try(input$in_showsubtitle),message1="Argument 'in_showsubtitle' missing."))
    if(is.null(input$in_showsubtitle)){sl_showsubtitle <- FALSE}else{sl_showsubtitle <- input$in_showsubtitle}
    if(sl_showsubtitle)
    {
      #validate(fn_validate(try(input$in_subtitlelabsize),message1="Argument 'in_subtitlelabsize' missing."))
      if(is.null(input$in_subtitlelabsize)){sl_subtitlesize <- NULL}else{sl_subtitlesize <- input$in_subtitlelabsize}
      #validate(fn_validate(try(input$in_subtitlelabspacer),message1="Argument 'in_subtitlelabspacer' missing."))
      if(is.null(input$in_subtitlelabspacer)){sl_subtitlespacer <- 1.4}else{sl_subtitlespacer <- input$in_subtitlelabspacer}
      #validate(fn_validate(try(input$in_subtitlelabhjust),message1="Argument 'in_subtitlelabhjust' missing."))
      if(is.null(input$in_subtitlelabhjust)){sl_subtitlehjust <- 0}else{sl_subtitlehjust <- input$in_subtitlelabhjust}
      #validate(fn_validate(try(input$in_subtitlelabvjust),message1="Argument 'in_subtitlelabvjust' missing."))
      if(is.null(input$in_subtitlelabvjust)){sl_subtitlevjust <- 0.5}else{sl_subtitlevjust <- input$in_subtitlelabvjust}
      #validate(fn_validate(try(input$in_subtitlelabcol),message1="Argument 'in_subtitlelabcol' missing."))
      if(is.null(input$in_subtitlelabcol)){sl_subtitlecol <- "grey30"}else{sl_subtitlecol <- input$in_subtitlelabcol}
      
      #validate(fn_validate(try(input$in_subtitlelab),message1="Argument 'subtitlelab' missing."))
      if(is.null(input$in_subtitlelab)){sl_subtitlelab <- FALSE}else{sl_subtitlelab <- input$in_subtitlelab}
      
    }else{
      sl_subtitlelab <- NA
      sl_subtitlesize <- NULL
      sl_subtitlespacer <- 1.4
      sl_subtitlehjust <- 0
      sl_subtitlevjust <- 0.5
      sl_subtitlecol <- "grey30"
    }
    
    # legend
    #validate(fn_validate(try(input$in_showlegend),message1="Argument 'in_showlegend' missing."))
    if(is.null(input$in_showlegend)){sl_showlegend <- FALSE}else{sl_showlegend <- input$in_showlegend}
    if(sl_showlegend){
      #validate(fn_validate(try(input$in_legendpos),message1="Argument 'in_legendpos' missing."))
      if(is.null(input$in_legendpos)){sl_legendpos <- "right"}else{sl_legendpos <- tolower(input$in_legendpos)}
      #validate(fn_validate(try(input$in_legendtextsize),message1="Argument 'in_legendtextsize' missing."))
      if(is.null(input$in_legendtextsize)){sl_legendtextsize <- 3}else{sl_legendtextsize <- input$in_legendtextsize}
      #validate(fn_validate(try(input$in_legendkeysize),message1="Argument 'in_legendkeysize' missing."))
      if(is.null(input$in_legendkeysize)){sl_legendkeysize <- 2}else{sl_legendkeysize <- input$in_legendkeysize}
      #validate(fn_validate(try(input$in_legendspacing),message1="Argument 'in_legendspacing' missing."))
      if(is.null(input$in_legendspacing)){sl_legendspacing <- 2}else{sl_legendspacing <- input$in_legendspacing}

      #validate(fn_validate(try(input$in_legendrow),message1="Argument 'in_legendrow' missing."))
      if(is.null(input$in_legendrow)){sl_legendrow <- NA}else{sl_legendrow <- input$in_legendrow}
      if(is.na(sl_legendrow)) sl_legendrow <- NULL
      
      #validate(fn_validate(try(input$in_legendlab),message1="Argument 'in_legendlab' missing."))
      sl_legendlab <- input$in_legendlab
      if(is.null(sl_legendlab)){
        sl_legendlab <- NA
      }else{
        if(sl_legendlab=="")
        {
          sl_legendlab <- NA
        }else{
          sl_legendlab <- as.character(unlist(strsplit(as.character(input$in_legendlab),",")))
          validate(fn_validate_equal(length(sl_legendlab),max(store_plot_helper$selected_tabulateq$k),"Number of legend labels are not equal to the max number of clusters."))
        }
      }
    }else{
      sl_legendlab <- NA
      sl_legendpos <- "right"
      sl_legendtextsize <- 3
      sl_legendkeysize <- 2
      sl_legendspacing <- 2 
      sl_legendrow <- NA
    }
    
    progress$inc(0.9,message="Computing std plot parameters...")
    
    # bar and y axis
    #validate(fn_validate(try(input$in_barsize),message1="Argument 'in_barsize' missing."))
    if(is.null(input$in_barsize)){sl_barsize <- 1}else{sl_barsize <- input$in_barsize}
    #validate(fn_validate(try(input$in_barbordersize),message1="Argument 'in_barbordersize' missing."))
    if(is.null(input$in_barbordersize)){sl_barbordersize <- 0}else{sl_barbordersize <- input$in_barbordersize}
    #validate(fn_validate(try(input$in_barbordercolour),message1="Argument 'in_barbordercolour' missing."))
    if(is.null(input$in_barbordercolour)){sl_barbordercolour <- "black"}else{sl_barbordercolour <- input$in_barbordercolour}
    #validate(fn_validate(try(input$in_showyaxis),message1="Argument 'in_showyaxis' missing."))
    if(is.null(input$in_showyaxis)){sl_showyaxis <- FALSE}else{sl_showyaxis <- input$in_showyaxis}
    if(sl_showyaxis){
      #validate(fn_validate(try(input$in_ticksize),message1="Argument 'in_ticksize' missing."))
      if(is.null(input$in_ticksize)){sl_ticksize <- 0.1}else{sl_ticksize <- input$in_ticksize}
      #validate(fn_validate(try(input$in_ticklength),message1="Argument 'in_ticklength' missing."))
      if(is.null(input$in_ticklength)){sl_ticklength <- 0.03}else{sl_ticklength <- input$in_ticklength}
      sl_showticks <- TRUE
    }else{
      sl_ticksize <- 0.1
      sl_ticklength <- 0.03
      sl_showticks <- FALSE
    }
    
    return(list(
      panelspacer=sl_panelspacer,
      showindlab=sl_showindlab,
      sharedindlab=sl_sharedindlab,
      indlabheight=sl_indlabheight,
      indlabspacer=sl_indlabspacer,
      indlabsize=sl_indlabsize,
      indlabcol=sl_indlabcol,
      indlabvjust=sl_indlabvjust,
      indlabhjust=sl_indlabhjust,
      indlabangle=sl_indlabangle,
      grplabspacer=sl_grplabspacer,
      grplabheight=sl_grplabheight,
      grplabpos=sl_grplabpos,
      grplabcol=grplabtextcol,
      grplabsize=sl_grplabsize,
      grplabangle=sl_grplabangle,
      grplabjust=sl_grplabjust,
      pointcol=grpmarkercol,
      pointsize=sl_pointsize,
      pointtype=sl_pointtype,
      linecol=grpmarkercol,
      linepos=sl_linepos,
      linesize=sl_linesize,
      linetype=sl_linetype,
      showdiv=sl_showdiv,
      divgrp=sl_divgrp,
      divcol=sl_divcol,
      divsize=sl_divsize,
      divtype=sl_divtype,
      panelratio=sl_panelratio,
      showsp=sl_showsp,
      sppos=sl_sppos,
      splab=sl_splab,
      splabsize=sl_splabsize,
      #splabangle=NULL,
      splabcol=sl_splabcol,
      #splabface="plain",
      spbgcol=sl_spbgcol,
      showtitle=sl_showtitle,
      titlelab=sl_titlelab,
      titlehjust=sl_titlehjust,
      titlevjust=sl_titlevjust,
      titlesize=sl_titlesize,
      titlecol=sl_titlecol,
      #titleface="plain",
      titlespacer=sl_titlespacer,
      showsubtitle=sl_showsubtitle,
      subtitlelab=sl_subtitlelab,
      subtitlehjust=sl_subtitlehjust,
      subtitlevjust=sl_subtitlevjust,
      subtitlesize=sl_subtitlesize,
      subtitlecol=sl_subtitlecol,
      #subtitleface="plain",
      subtitlespacer=sl_subtitlespacer,
      showlegend=sl_showlegend,
      legendlab=sl_legendlab,
      legendpos=sl_legendpos,
      legendkeysize=sl_legendkeysize,
      legendtextsize=sl_legendtextsize,
      legendspacing=sl_legendspacing,
      legendrow=sl_legendrow,
      barsize=sl_barsize,
      barbordersize=sl_barbordersize,
      barbordercolour=sl_barbordercolour,
      showyaxis=sl_showyaxis,
      showticks=sl_showticks,
      ticksize=sl_ticksize,
      ticklength=sl_ticklength
    ))
  }
  
  
  # ER: er_core ----------------------------------------------------------------
  # event reactive for draw button
  
  er_core <- eventReactive(input$btn_draw, {
    fn_sl_core()
  })
  
  # ER: er_std ----------------------------------------------------------------
  # event reactive for draw button
  
  er_std <- eventReactive(input$btn_draw, {
    fn_sl_std()
  })
  
  # OUT: out_plot_barplot --------------------------------------------------------------
  # single line barplot output image
  
  output$out_plot_barplot <- renderImage({

    sl_outputfilename <- paste0(fn_pophelper(),"_barplot",sep="")
    #if(is.null(store_general$qlist)) fn_sl_core()
    cparams <- fn_sl_core()
    params <- fn_sl_std()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Drawing plot...",value=0.2)
    
    if(is.null(input$in_height)) {height <- NA}else{height <- input$in_height}
    if(is.null(input$in_width)) {width <- NA}else{width <- input$in_width}
    if(is.null(input$in_res)) {res <- 200}else{res <- as.integer(input$in_res)}
    
    plotQ(qlist=cparams$qlist,imgoutput=cparams$imgoutput,
          outputfilename=sl_outputfilename,clustercol=cparams$clustercol,sortind=cparams$sortind,
          grplab=cparams$grplab,selgrp=cparams$selgrp,ordergrp=cparams$ordergrp,subsetgrp=cparams$subsetgrp,
          grpmean=cparams$grpmean,panelspacer=params$panelspacer,showsp=params$showsp,
          sppos=params$sppos,splab=params$splab,splabsize=params$splabsize,splabcol=params$splabcol,
          spbgcol=params$spbgcol,showtitle=params$showtitle,titlelab=params$titlelab,
          titlehjust=params$titlehjust,titlevjust=params$titlevjust,titlesize=params$titlesize,
          titlecol=params$titlecol,titlespacer=params$titlespacer,showsubtitle=params$showsubtitle,
          subtitlelab=params$subtitlelab,subtitlehjust=params$subtitlehjust,
          subtitlevjust=params$subtitlevjust,subtitlesize=params$subtitlesize,
          subtitlecol=params$subtitlecol,subtitlespacer=params$subtitlespacer,
          grplabspacer=params$grplabspacer,grplabheight=params$grplabheight,
          grplabpos=params$grplabpos,grplabsize=params$grplabsize,
          grplabangle=params$grplabangle,grplabjust=params$grplabjust,
          grplabcol=params$grplabcol,showindlab=params$showindlab,sharedindlab=params$sharedindlab,
          useindlab=cparams$useindlab,indlabwithgrplab=cparams$indlabwithgrplab,
          indlabspacer=params$indlabspacer,indlabheight=params$indlabheight,
          indlabsep=cparams$indlabsep,indlabsize=params$indlabsize,indlabangle=params$indlabangle,
          indlabvjust=params$indlabvjust,indlabhjust=params$indlabhjust,indlabcol=params$indlabcol,
          pointsize=params$pointsize,pointcol=params$pointcol,pointtype=params$pointtype,
          linepos=params$linepos,linesize=params$linesize,linetype=params$linetype,
          linecol=params$linecol,showdiv=params$showdiv,
          divgrp=params$divgrp,divcol=params$divcol,divtype=params$divtype,
          divsize=params$divsize,showlegend=params$showlegend,legendlab=params$legendlab,
          legendpos=params$legendpos,legendkeysize=params$legendkeysize,
          legendtextsize=params$legendtextsize,legendspacing=params$legendspacing,
          legendrow=params$legendrow,barsize=params$barsize,
          barbordersize=params$barbordersize,barbordercolour=params$barbordercolour,
          showyaxis=params$showyaxis,showticks=params$showticks,ticksize=params$ticksize,
          ticklength=params$ticklength,panelratio=params$panelratio,
          imgtype="png",height=height,width=width,dpi=res)

    progress$set(message="Drawing plot...",value=0.8)
    
    if(!any(is.na(cparams$grplab))) {labs <- nrow(cparams$grplab)}else{labs <- 0}
    dims <- getDim(ind=max(sapply(cparams$qlist,nrow)),height=height,
                       width=width,dpi=res,units="cm",
                       imgtype="png",grplabheight=params$grplabheight,
                       labs=labs,plotnum=length(cparams$qlist),
                       showindlab=params$showindlab,sharedindlab=params$sharedindlab)
    
    progress$set(message="Drawing plot...",value=0.9)
    
    return(list(src=paste0(sl_outputfilename,".png"),
                contentType="image/png",
                width=round(((dims$width*as.integer(input$in_res))/2.54)*input$in_scale,0),
                height=round((((dims$height+dims$grplabheight)*as.integer(input$in_res))/2.54)*input$in_scale,0),
                alt="standard_barplot"))
    
  },deleteFile=TRUE)
  
  # FN: fn_downloadplotname ----------------------------------------------------
  # creates filename for download plot
  
  fn_downloadplotname <- function()
  {
    downloadfilename <- NULL
    validate(fn_validate(try(input$in_format),message1="Argument 'in_format' missing."))
    if(input$in_format=="png") downloadfilename <- paste0(fn_pophelper(),"_barplot.png")
    if(input$in_format=="jpeg") downloadfilename <- paste0(fn_pophelper(),"_barplot.jpg")
    if(input$in_format=="tiff") downloadfilename <- paste0(fn_pophelper(),"_barplot.tiff")
    if(input$in_format=="pdf") downloadfilename <- paste0(fn_pophelper(),"_barplot.pdf")
    
    return(downloadfilename)
  }
  
  # FN: fn_downloadplot --------------------------------------------------------
  # download single line barplot
  
  fn_downloadplot <- function(){
    
    cparams <- fn_sl_core()
    params <- fn_sl_std()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Downloading plot...",value=0.6)
    
    sl_outputfilename <- gsub(".pdf$","",gsub(".tiff$","",gsub(".jpg$","",gsub(".png$","",fn_downloadplotname()))))

    plotQ(qlist=cparams$qlist,imgoutput=cparams$imgoutput,
                     outputfilename=sl_outputfilename,clustercol=cparams$clustercol,sortind=cparams$sortind,
                     grplab=cparams$grplab,selgrp=cparams$selgrp,ordergrp=cparams$ordergrp,subsetgrp=cparams$subsetgrp,
                     grpmean=cparams$grpmean,panelspacer=params$panelspacer,showsp=params$showsp,
                     sppos=params$sppos,splab=params$splab,splabsize=params$splabsize,splabcol=params$splabcol,
                     spbgcol=params$spbgcol,showtitle=params$showtitle,titlelab=params$titlelab,
                     titlehjust=params$titlehjust,titlevjust=params$titlevjust,titlesize=params$titlesize,
                     titlecol=params$titlecol,titlespacer=params$titlespacer,showsubtitle=params$showsubtitle,
                     subtitlelab=params$subtitlelab,subtitlehjust=params$subtitlehjust,
                     subtitlevjust=params$subtitlevjust,subtitlesize=params$subtitlesize,
                     subtitlecol=params$subtitlecol,subtitlespacer=params$subtitlespacer,
                     grplabspacer=params$grplabspacer,grplabheight=params$grplabheight,
                     grplabpos=params$grplabpos,grplabsize=params$grplabsize,
                     grplabangle=params$grplabangle,grplabjust=params$grplabjust,
                     grplabcol=params$grplabcol,showindlab=params$showindlab,sharedindlab=params$sharedindlab,
                     useindlab=cparams$useindlab,indlabwithgrplab=cparams$indlabwithgrplab,
                     indlabspacer=params$indlabspacer,indlabheight=params$indlabheight,
                     indlabsep=cparams$indlabsep,indlabsize=params$indlabsize,indlabangle=params$indlabangle,
                     indlabvjust=params$indlabvjust,indlabhjust=params$indlabhjust,indlabcol=params$indlabcol,
                     pointsize=params$pointsize,pointcol=params$pointcol,pointtype=params$pointtype,
                     linepos=params$linepos,linesize=params$linesize,linetype=params$linetype,
                     linecol=params$linecol,showdiv=params$showdiv,
                     divgrp=params$divgrp,divcol=params$divcol,divtype=params$divtype,
                     divsize=params$divsize,showlegend=params$showlegend,legendlab=params$legendlab,
                     legendpos=params$legendpos,legendkeysize=params$legendkeysize,
                     legendtextsize=params$legendtextsize,legendspacing=params$legendspacing,
                     legendrow=params$legendrow,barsize=params$barsize,
                     barbordersize=params$barbordersize,barbordercolour=params$barbordercolour,
                     showyaxis=params$showyaxis,showticks=params$showticks,ticksize=params$ticksize,
                     ticklength=params$ticklength,panelratio=params$panelratio,
                     imgtype=input$in_format,height=input$in_height,
                     width=input$in_width,dpi=as.integer(input$in_res))
    
    progress$inc(0.9,message="Downloading plot...")
  }
  
  # DHL: btn_downloadplotsp ----------------------------------------------------
  # download handler for downloading single line barplot
  
  output$btn_downloadplotsp <- downloadHandler(
    filename=fn_downloadplotname,
    content=function(file) {
      fn_downloadplot()
      file.copy(paste0(fn_downloadplotname()),file,overwrite=T)
    }
  )
  
  ## INTERACTIVE PLOT ==========================================================
  
  # UI: intplotoptions -----------------------------------------------
  output$ui_intplotoptions <- renderUI({
    req(store_general$qlist)
  
    div(
      div(
        h3("> Interactive options"),
        wellPanel(
          sliderInput("ip_height","Plot height (px)",min=100,max=1000,step=2,value=280),
          sliderInput("ip_width","Plot width (px)",min=200,max=1600,step=2,value=800),
          sliderInput("ip_border","Bar border size",min=0,max=2,step=0.1,value=0),
          sliderInput("ip_grpwidth","Bar gap",min=0,max=1,step=0.1,value=0),
          checkboxInput("ip_legend","Show legend",value=TRUE),
          checkboxInput("ip_credit","Show caption",value=TRUE),
          checkboxInput("ip_title","Show title",value=FALSE)
        ))
    )
  })
  
  # FN: fn_sl_int --------------------------------------------------------------
  # plots interactive plots
  
  fn_sl_int <- function(coredata){
    
    cat("Running fn_sl_int() ...\n")
    
    dfa <- intPlotQ(dfr=coredata[["dfr"]],sortind=coredata[["sortind"]],
                    grplab=coredata[["grplab"]],selgrp=coredata[["selgrp"]],
                    ordergrp=coredata[["ordergrp"]],subsetgrp=coredata[["subsetgrp"]],
                    grpmean=coredata[["grpmean"]],useindlab=coredata[["useindlab"]],
                    indlabwithgrplab=coredata[["indlabwithgrplab"]],indlabsep=coredata[["indlabsep"]])

    hc <- dfa$df %>% dplyr::mutate(x=as.factor(x)) %>%
      hchart(.,"column",hcaes(x="x",y="y",group="group")) %>%
      hc_xAxis(title=list(text=NULL),allowDecimals=FALSE,categories=dfa$df$ind) %>%
      hc_yAxis(title=list(text=NULL),max=1) %>%
      hc_colors(substr(coredata[["clustercol"]],1,7)) %>%
      hc_legend(enabled=coredata[["legend"]],align='right',verticalAlign='top',
                layout='horizontal',floating=FALSE) %>%
      hc_plotOptions(column=list(animation=FALSE,stacking="normal",pointPadding=0,
                                 groupPadding=coredata[["grpwidth"]],borderWidth=coredata[["border"]])) %>%
      hc_chart(zoomType='x',panKey="shift",panning=TRUE) %>%
      hc_size(height=coredata[["height"]],width=coredata[["width"]]) %>%
      hc_credits(enabled=coredata[["credit"]],text=coredata[["name"]],href="") %>%
      hc_exporting(enabled=TRUE,url="https://export.highcharts.com",
                   fallbackToExportServer=TRUE,buttons=list(contextButton=list(
                     align='left',verticalAlign='top'))) %>%
      hc_tooltip(borderWidth=1,followPointer=TRUE,followTouchMove=TRUE,shared=FALSE,
                 headerFormat="",
                 pointFormat="{point.popup}")
    
    if(coredata[["title"]]) hc <- hc_title(hc,text=coredata[["name"]],align="left",x=35,y=20)
    if(!coredata[["showindlab"]]) hc <- hc_xAxis(hc,title=list(text=NULL),allowDecimals=FALSE,visible=FALSE)
    
    return(column(width=12,hc))
  }
  
  # FN: fn_sl_int_params --------------------------------------------------------------
  # int plot params
  
  fn_sl_int_params <- reactive({
    
    cat("Running fn_sl_params() ...\n")
    
    #validate(fn_validate(try(input$ip_height),message1="Argument 'ip_height' missing."))
    if(is.null(input$ip_height)) {ip_height <- 280}else{ip_height <- input$ip_height}
    #validate(fn_validate(try(input$ip_width),message1="Argument 'ip_width' missing."))
    if(is.null(input$ip_width)) {ip_width <- 800}else{ip_width <- input$ip_width}
    #validate(fn_validate(try(input$ip_border),message1="Argument 'ip_border' missing."))
    if(is.null(input$ip_border)) {ip_border <- 0}else{ip_border <- input$ip_border}
    #validate(fn_validate(try(input$ip_grpwidth),message1="Argument 'ip_grpwidth' missing."))
    if(is.null(input$ip_grpwidth)) {ip_grpwidth <- 0}else{ip_grpwidth <- input$ip_grpwidth}
    #validate(fn_validate(try(input$ip_legend),message1="Argument 'ip_legend' missing."))
    if(is.null(input$ip_legend)) {ip_legend <- FALSE}else{ip_legend <- input$ip_legend}
    #validate(fn_validate(try(input$ip_credit),message1="Argument 'ip_credit' missing."))
    if(is.null(input$ip_credit)) {ip_credit <- TRUE}else{ip_credit <- input$ip_credit}
    #validate(fn_validate(try(input$ip_title),message1="Argument 'ip_title' missing."))
    if(is.null(input$ip_title)) {ip_title <- FALSE}else{ip_title <- input$ip_title}
    
    return(list(height=ip_height,width=ip_width,border=ip_border,
         grpwidth=ip_grpwidth,legend=ip_legend,credit=ip_credit,
         title=ip_title))
  })
  
  # ER: er_int ----------------------------------------------------------------
  # event reactive for draw button
  
  er_int <- eventReactive(input$btn_draw, {
    fn_sl_int_params()
  })
  
  # OUT: out_hcontainer --------------------------------------------------------
  # interactive barplot container
  
  output$out_hcontainer <- renderUI({
    
    cparams <- fn_sl_core()
    iparams <- fn_sl_int_params()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Drawing plot...",value=0.2)
    
    clist <- list(length=length(cparams$qlist))
    for(i in 1:length(cparams$qlist))
    {
      tlist <- list()
      tlist$dfr <- cparams$qlist[[i]]
      tlist$name <- names(cparams$qlist)[i]
      tlist$sortind <- cparams$sortind
      tlist$grplab <- cparams$grplab
      tlist$selgrp <- cparams$selgrp
      tlist$ordergrp <- cparams$ordergrp
      tlist$subsetgrp <- cparams$subsetgrp
      tlist$grpmean <- cparams$grpmean
      tlist$useindlab <- cparams$useindlab
      tlist$showindlab <- cparams$showindlab
      tlist$indlabwithgrplab <- cparams$indlabwithgrplab
      tlist$indlabsep <- cparams$indlabsep
      tlist$clustercol <- cparams$clustercol
      
      tlist$height <- iparams$height
      tlist$width <- iparams$width
      tlist$border <- iparams$border
      tlist$grpwidth <- iparams$grpwidth
      tlist$legend <- iparams$legend
      tlist$credit <- iparams$credit
      tlist$title <- iparams$title
      
      clist[[i]] <- tlist
    }
    
    progress$set(message="Drawing plot...",value=0.3)
    charts <- lapply(clist,fn_sl_int)
    progress$set(message="Drawing plot...",value=0.7)
    #if(is.null(input$in_imgfloat)){imgfloat <- FALSE}else{imgfloat <- input$in_imgfloat}
    do.call(tagList,charts)
  })
  
  # OSE ------------------------------------------------------------------------
  # delete user directory when session ends
  
  session$onSessionEnded(function() {
    cat("Removing user directory ...\n")
    setwd(rootwd)
    if(dir.exists(isolate(store_general$newwd))){
      unlink(isolate(store_general$newwd),recursive=TRUE)
    }
  })
  
  # OUT: out_display_plot ------------------------------------------------------
  # print values for debugging plots
  
  output$out_display_plot <- renderPrint({
    cat(paste0("selected runs: ",length(input$table_selectrunplot_rows_selected),"\n"))
    cat(paste0("in_scale: ",input$in_scale,"\n"))
    cat(paste0("in_clustercol: ",input$in_clustercol,"\n"))
    cat(paste0("in_colorbrewerpalette: ",input$in_colorbrewerpalette,"\n"))
    cat(paste0("in_align: ",input$in_align,"\n"))
    cat(paste0("in_sortind: ",input$in_sortind,"\n"))
    cat("\nINDLAB\n")
    cat(paste0("in_useindlab: ",input$in_useindlab,"\n"))
    cat(paste0("in_showindlab: ",input$in_showindlab,"\n"))
    cat(paste0("in_indlabtype: ",input$in_indlabtype,"\n"))
    cat(paste0("in_indlabupload: ",input$in_indlabupload,"\n"))
    cat(paste0("in_indlabpaste: ",input$in_indlabpaste,"\n"))
    cat(paste0("in_indlabwithgrplab: ",input$in_indlabwithgrplab,"\n"))
    cat(paste0("in_indlabsep: ",input$in_indlabsep,"\n"))
    cat("\nINDLAB STD PLOT\n")
    cat(paste0("in_indlabheight: ",input$in_indlabheight,"\n"))
    cat(paste0("in_indlabspacer: ",input$in_indlabspacer,"\n"))
    cat(paste0("in_indlabsize: ",input$in_indlabsize,"\n"))
    cat(paste0("in_indlabcol: ",input$in_indlabcol,"\n"))
    cat(paste0("in_indlabvjust: ",input$in_indlabvjust,"\n"))
    cat(paste0("in_indlabhjust: ",input$in_indlabhjust,"\n"))
    cat(paste0("in_indlabangle: ",input$in_indlabangle,"\n"))
    cat("\nGRPLAB\n")
    cat(paste0("in_showgrplab: ",input$in_showgrplab,"\n"))
    cat(paste0("in_grplabtype: ",input$in_grplabtype,"\n"))
    cat(paste0("in_grplabupload: ",input$in_grplabupload,"\n"))
    cat(paste0("in_grplabpaste: ",input$in_grplabpaste,"\n"))
    cat(paste0("in_ordergrp: ",input$in_ordergrp,"\n"))
    cat(paste0("in_grpmean: ",input$in_grpmean,"\n"))
    cat(paste0("in_selectgrp: ",input$in_selectgrp,"\n"))
    cat(paste0("in_subsetgrp: ",input$in_subsetgrp,"\n"))
    cat("\nGRPLAB STD PLOT\n")
    cat(paste0("in_glh: ",input$in_glh,"\n"))
    cat(paste0("in_ls: ",input$in_ls,"\n"))
    cat(paste0("in_grplabcol: ",input$in_grplabcol,"\n"))
    cat(paste0("in_grplabpos: ",input$in_grplabpos,"\n"))
    cat(paste0("in_grplabtextcol: ",input$in_grplabtextcol,"\n"))
    cat(paste0("in_grplabsize: ",input$in_grplabsize,"\n"))
    cat(paste0("in_grplabangle: ",input$in_grplabangle,"\n"))
    cat(paste0("in_grplabjust: ",input$in_grplabjust,"\n"))
    cat(paste0("in_pointsize: ",input$in_pointsize,"\n"))
    cat(paste0("in_pointtype: ",input$in_pointtype,"\n"))
    cat(paste0("in_linepos: ",input$in_linepos,"\n"))
    cat(paste0("in_linesize: ",input$in_linesize,"\n"))
    cat(paste0("in_linetype: ",input$in_linetype,"\n"))
    cat("\nDIV\n")
    cat(paste0("in_showdiv: ",input$in_showdiv,"\n"))
    cat(paste0("in_divcol: ",input$in_divcol,"\n"))
    cat(paste0("in_divsize: ",input$in_divsize,"\n"))
    cat(paste0("in_divtype: ",input$in_divtype,"\n"))
    cat(paste0("in_ps: ",input$in_ps,"\n"))
    cat("\nSIDE PANEL\n")
    cat(paste0("in_showsp: ",input$in_showsp,"\n"))
    cat(paste0("in_splab: ",input$in_splab,"\n"))
    cat(paste0("in_splabsize: ",input$in_splabsize,"\n"))
    cat(paste0("in_splabpos: ",input$in_splabpos,"\n"))
    cat(paste0("in_splabcol: ",input$in_splabcol,"\n"))
    cat(paste0("in_spbgcol: ",input$in_spbgcol,"\n"))
    cat("\nTITLE\n")
    cat(paste0("in_showtitle: ",input$in_showtitle,"\n"))
    cat(paste0("in_titlelab: ",input$in_titlelab,"\n"))
    cat(paste0("in_titlelabsize: ",input$in_titlelabsize,"\n"))
    cat(paste0("in_titlelabspacer: ",input$in_titlelabspacer,"\n"))
    cat(paste0("in_titlelabhjust: ",input$in_titlelabhjust,"\n"))
    cat(paste0("in_titlelabvjust: ",input$in_titlelabvjust,"\n"))
    cat(paste0("in_titlelabcol: ",input$in_titlelabcol,"\n"))
    cat("\nSUBTITLE\n")
    cat(paste0("in_showsubtitle: ",input$in_showsubtitle,"\n"))
    cat(paste0("in_subtitlelab: ",input$in_subtitlelab,"\n"))
    cat(paste0("in_subtitlelabsize: ",input$in_subtitlelabsize,"\n"))
    cat(paste0("in_subtitlelabspacer: ",input$in_subtitlelabspacer,"\n"))
    cat(paste0("in_subtitlelabhjust: ",input$in_subtitlelabhjust,"\n"))
    cat(paste0("in_subtitlelabvjust: ",input$in_subtitlelabvjust,"\n"))
    cat(paste0("in_subtitlelabcol: ",input$in_subtitlelabcol,"\n"))
    cat("\nLEGEND\n")
    cat(paste0("in_showlegend: ",input$in_showlegend,"\n"))
    cat(paste0("in_legendlab: ",input$in_legendlab,"\n"))
    cat(paste0("in_legendpos: ",input$in_legendpos,"\n"))
    cat(paste0("in_legendtextsize: ",input$in_legendtextsize,"\n"))
    cat(paste0("in_legendkeysize: ",input$in_legendkeysize,"\n"))
    cat("\nDOWNLOAD PLOT\n")
    cat(paste0("in_height: ",input$in_height,"\n"))
    cat(paste0("in_width: ",input$in_width,"\n"))
    cat(paste0("in_res: ",input$in_res,"\n"))
    cat(paste0("in_format: ",input$in_format,"\n"))
    cat("\nSTORE PLOT HELPER\n")
    cat(paste0("selected_run: ",paste0(store_plot_helper$selected_run,collapse=","),"\n"))
    cat("qlist\n")
    print(str(store_plot_helper$qlist))
    cat("selected_tabulateq\n")
    print(head(store_plot_helper$selected_selected_tabulateq))
    cat(paste0("fn_sl_indlab:",paste0(store_plot_helper$indlab,collapse=","),"\n"))
    cat("grplab\n")
    print(head(store_plot_helper$grplab))
    #cat(paste0("kvec: ",paste0(store_plot_helper$kvec,collapse=","),"\n"))
    cat(paste0("grplabtitle: ",paste0(store_plot_helper$grplabtitle,collapse=","),"\n"))
    cat(paste0("grplabtext: ",paste0(store_plot_helper$grplabtext,collapse=","),"\n"))
    cat(paste0("sortind: ",paste0(store_plot_helper$sortind,collapse=","),"\n"))
    cat("\nFN_SL_CORE\n")
    print(fn_sl_core())
    cat("\nFN_SL_STD\n")
    print(fn_sl_std())
  })
    
  ## INT PLOT OPTS =============================================================
})