# POPHELPER SHINY
# FUNCTIONS.R
# Roy Mathew Francis
# v2.0.0
# 13-Aug-2017

# > sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United Kingdom.1252 
# [2] LC_CTYPE=English_United Kingdom.1252   
# [3] LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.1252    
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils    
# [6] datasets  methods   base     
# 
# other attached packages:
#   [1] shinythemes_1.1.1  Cairo_1.5-9        xlsx_0.6.1        
# [4] rJava_0.9-10       shinyBS_0.61       viridisLite_0.3.0 
# [7] colourpicker_1.0   shinyAce_0.3.1     DT_0.4            
# [10] rlist_0.4.6.1      gtable_0.2.0       gridExtra_2.3     
# [13] markdown_0.8       fields_9.6         maps_3.3.0        
# [16] spam_2.2-0         dotCall64_1.0-0    shiny_1.1.0       
# [19] RColorBrewer_1.1-2 highcharter_0.5.0  dplyr_0.7.6       
# [22] tidyr_0.8.1        stringr_1.3.1      ggplot2_3.0.0     
# [25] captioner_2.2.3    bookdown_0.7       knitr_1.20        
# 
# loaded via a namespace (and not attached):
#   [1] jsonlite_1.5      assertthat_0.2.0  TTR_0.23-3       
# [4] xlsxjars_0.6.1    yaml_2.2.0        pillar_1.3.0     
# [7] backports_1.1.2   lattice_0.20-35   glue_1.3.0       
# [10] digest_0.6.15     promises_1.0.1    colorspace_1.3-2 
# [13] htmltools_0.3.6   httpuv_1.4.5      plyr_1.8.4       
# [16] pkgconfig_2.0.1   broom_0.5.0       purrr_0.2.5      
# [19] xtable_1.8-2      scales_1.0.0      later_0.7.3      
# [22] tibble_1.4.2      withr_2.1.2       sourcetools_0.1.7
# [25] lazyeval_0.2.1    quantmod_0.4-13   magrittr_1.5     
# [28] crayon_1.3.4      mime_0.5          evaluate_0.11    
# [31] nlme_3.1-137      xts_0.11-0        rsconnect_0.8.8  
# [34] tools_3.5.1       data.table_1.11.4 munsell_0.5.0    
# [37] bindrcpp_0.2.2    compiler_3.5.1    rlang_0.2.1      
# [40] htmlwidgets_1.2   crosstalk_1.0.0   igraph_1.2.2     
# [43] miniUI_0.1.1.1    rmarkdown_1.10    curl_3.2         
# [46] R6_2.2.2          zoo_1.8-3         lubridate_1.7.4  
# [49] bindr_0.1.1       rprojroot_1.3-2   stringi_1.2.4    
# [52] Rcpp_0.12.18      tidyselect_0.2.4  xfun_0.3    

#load or install required libraries
library("shiny")
library("fields") #tim.colors()
library("ggplot2") #ggplot
library("markdown") #help page
library("tidyr") #gather
library("gridExtra")
library("gtable") #gtable_add_cols
library("RColorBrewer") #brewer.pal()
library("DT") #dataTable
library("shinyAce") #aceEditor
library("colourpicker") #colourInput
library("viridisLite") #viridis colours
library("shinyBS") #hover tips
library("highcharter") #int plot
library("dplyr") #int plot
library("rJava") #excel download
library("xlsx") #excel download
library("Cairo")
library("shinythemes")
#library("extrafont") #fonts

# mainfont <- "Ubuntu"
# try <- try(if(!mainfont %in% extrafont::fonts()) font_import(pattern=mainfont, prompt = F))
# if(class(try)=="try-error")
# {
#   fontfamily <- "sans"
# }else{
#   if(!mainfont %in% extrafont::fonts()) font_import(pattern=mainfont, prompt = F)
#   if(mainfont %in% extrafont::fonts()) {fontfamily <- mainfont}else{fontfamily <- "Ubuntu"}
# }

fontfamily <- "sans"
options(shiny.deprecation.messages=TRUE)


#pophelper version
fn_pophelper <- function()
{
  return("pophelperShiny_v2.0.0") 
}

#pophelper version
fn_update <- function()
{
  return("13-Aug-2018") 
}

# fn_dir
fn_dir <- function(currwd=NULL)
{
  if(is.null(currwd)) currwd <- getwd()

  #Create working directory
  newwd <- paste0(currwd,"/",paste0(format(Sys.time(),"%Y%m%d%H%M%S"),sample(1:1000,1)))
  dir.create(newwd)

  return(newwd)
}

#validation
fn_validate <- function(input,message1,message2,message3) 
{
  
  if(missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing",message1))
  if(gcheck == 1)
  {
    m1 <- sub("Argument ","",message1)
    m1 <- sub(" missing.","",m1)
  }
  
  if (all(is.null(input))) {
    if(missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if(all(is.na(input)))
    {
      if(missing(message2))
      {
        if(gcheck==1) message2 <- paste0("Argument ",m1," is NA.",sep="")
        if(gcheck!=1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if(all(nchar(input) == 0))
    {
      if(missing(message3))
      {
        if(gcheck==1) message3 <- paste0("Argument ",m1," is empty.",sep="")
        if(gcheck!=1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# Validate equality between two input values
# Prints message if input1 is not equal to input2
#
fn_validate_equal <- function(input1,input2,message)
{
  if(all(input1 != input2)) print(message)
}

# Validate colour return
# Checks if input is a high_k (low number of colours)
#
fn_validate_colours <- function(input1,message)
{
  if(length(input1)==1) {
    if(input1 == "high_k") print(message)
  }
}

#validate structure for evanno
fn_validate_evanno <- function(input1)
{
  un <- unique(checkQ(input1$datapath)$type)
  if(length(un)>1) return("One or more input files are not in STRUCTURE format.")
  if(length(un)==1 && un!="STRUCTURE") return("One or more input files are not in STRUCTURE format.")
}

#Validateevannocalc
fn_validate_evannocalc <- function(input1)
{
  if(is.null(input1)) return(FALSE)
}

#Validate grplab
fn_validate_grplab <- function(grplab)
{
  if(!is.list(grplab)) print("Group labels are not list datatype.")
  if(is.null(names(grplab))) print("Group label list element(s) are not named.")
  if(any(duplicated(names(grplab)))) print("Duplicate label titles found in group labels.")
  if(!any(sapply(grplab,is.character))) print("Group labels contains one or more elements that are not character datatype.")
  if(length(unique(sapply(grplab,length)))>1) print("Group labels contains labels that are of unequal length.")
  if(!any(sapply(grplab,length)>1)) print("Group labels contains labels of length < 2.")
  if(any(sapply(grplab,is.na))) print("Group labels contains one or more NAs.")
}

# validate sharedindlab
fn_validate_sharedindlab <- function(sharedindlab,sortind)
{
  if(sortind!="label" && sharedindlab) print("'Common individual labels' must be FALSE (unchecked), when individuals are ordered by 'all' or a cluster.")
}

# Validate subset group
# Checks if subset group is contiguous and if ordergrp is checked
#' @param input1 selgrp title character
#' @param input2 grplab data.frame
#' @param input3 ordergrp
#' 
fn_validate_subsetgrp <- function(input1,input2,input3)
{
  rlegrp <- rle(as.vector(unlist(input2[,input1,drop=FALSE])))
  if(any(duplicated(rlegrp$values)) && (!input3)) print("Selected active group label set contains non-contiguous labels. Check 'Order group labels' to reorder all group label sets alphabetically.")
}

#colourPalettes
colourPalettes <- function()
{
  return(list("Function Based" = c("Inferno","Magma","Plasma","Viridis","Rich","Tim","Muted","Teal","Funky","Merry","Rainbow"), 
              "Pre Defined" = c("Standard","Strong","Wong","Krzywinski","Morris","Ocean five","Keeled","Vintage","Retro","Colorbrewer")))
}

#getColoursWa
getColoursWa <- function(k,scheme,palette)
{
  if(missing(k)) stop("getColoursWa: Argument 'k' is missing.")
  if (!is.numeric(k)) stop("getColoursWa: Argument 'k' is not numeric. ")
  if(missing(scheme)) stop("getColoursWa: Argument 'k' is missing.")
  
  #' @description Splits input vector into equal length and returns at n equal intervals
  #' @param x A character or numeric vector
  #' @param n A number denoting length
  #' 
  cslice <- function(x,n)
  {
    if(n > length(x)) stop("n must be <= length of x.")
    x1 <- 1:length(x)
    x2 <- split(x1, sort(x1%%n))
    x3 <- as.integer(sapply(x2,function(x) x[1]))
    return(x[x3])
  }

  col1 <- NULL
  #modified rich.colors() fn from gplots package
  r.colors <- function(n)
  {
    x <- seq(0, 1, length = n)
    r <- 1/(1 + exp(20 - 35 * x))
    g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
    b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
    rgb.m <- matrix(c(r, g, b), ncol = 3, dimnames = list(NULL, c("red", "green", "blue")))
    col <- mapply(rgb, r, g, b, 1)
    return(col)
  }
  
  if(scheme == "Strong")
  {
    col1 <- c("#11A4C8","#63C2C5","#1D4F9F",
              "#0C516D","#2A2771","#396D35",
              "#80C342","#725DA8","#B62025",
              "#ED2224","#ED1943","#ED3995",
              "#7E277C","#F7EC16","#F8941E",
              "#8C2A1C","#808080")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Standard")
  {
    col1 <- c("#1D72F5","#DF0101","#77CE61", 
              "#FF9326","#A945FF","#0089B2",
              "#FDF060","#FFA6B2","#BFF217",
              "#60D5FD","#CC1577","#F2B950",
              "#7FB21D","#EC496F","#326397",
              "#B26314","#027368","#A4A4A4",
              "#610B5E")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Ocean five")
  {
    col1 <- c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Keeled")
  {
    col1 <- c("#48B098", "#91CB62", "#FFEE3B", "#FB9013", "#FF3C28")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Vintage")
  {
    col1 <- c("#400F13", "#027368", "#A3BF3F", "#F2B950", "#D93A2B")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }

  if(scheme == "Retro")
  {
    col1 <- c("#01948E","#A9C4E2","#E23560","#01A7B3","#FDA963","#323665","#EC687D")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Wong")
  {
    col1 <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#006699","#D55E00","#CC79A7")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Krzywinski")
  {
    col1 <- c("#006E82","#8214A0","#005AC8","#00A0FA","#FA78FA","#14D2DC","#AA0A3C","#FA7850","#0AB45A","#F0F032","#A0FA82","#FAE6BE")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Morris")
  {
    col1 <- c("#4D94CC","#34648A","#8B658A","#9ACD32","#CC95CC","#9ACD32","#8B3A39","#CD6601","#CC5C5B","#8A4500")
    if (k <= length(col1)) return(col1[1:k])
    if (k > length(col1)) {return("high_k")}
  }
  
  if(scheme == "Muted")
  {
    col1 <- c("#46BDDD","#82DDCE","#F5F06A","#F5CC6A","#F57E6A")
    cr <- colorRampPalette(col1,bias=1,space="rgb",interpolate="spline")
    return(cr(k))
  }
  
  if(scheme == "Teal")
  {
    col1 <- c("#CFF09E","#A8DBA8","#79BD9A","#3B8686","#0B486B")
    cr <- colorRampPalette(col1,bias=1,space="rgb",interpolate="spline")
    return(cr(k))
  }
  
  if(scheme == "Merry")
  {
    col1 <- c("#5BC0EB","#FDE74C","#9BC53D","#E55934","#FA7921")
    cr <- colorRampPalette(col1,bias=1,space="rgb",interpolate="spline")
    return(cr(k))
  }
  
  if(scheme == "Funky")
  {
    col1 <- c("#A6CEE3","#3F8EAA","#79C360","#E52829","#FDB762","#ED8F47","#9471B4")
    cr <- colorRampPalette(col1,bias=1,space="rgb",interpolate="spline")
    return(cr(k))
  }
  
  if(scheme == "Viridis") return(viridisLite::viridis(n=k))
  if(scheme == "Inferno") return(viridisLite::inferno(n=k))
  if(scheme == "Magma") return(viridisLite::magma(n=k))
  if(scheme == "Plasma") return(viridisLite::plasma(n=k))
  if(scheme == "Rich") return(r.colors(k))
  if(scheme == "Tim") return(fields::tim.colors(k))
  if(scheme == "Rainbow") return(rainbow(k))
  if(scheme == "Colorbrewer") 
  {
    ninecols <- c("Set1","Pastel1","BuGn","BuPu","GnBu","OrRd","PuBu","PuRd","RdPu","YlGn","PuBuGn","YlGnBu","YlOrBr","YlOrRd","Blues","Greens","Oranges","Purples","Reds","Greys")
    if(any(palette %in% ninecols)) {
      if(k>9) {
        return("high_k")
      }else{
          return(RColorBrewer::brewer.pal(k,palette))
      }
    }
    
    eightcols <- c("Set2","Accent","Pastel2","Dark2")
    if(any(palette %in% eightcols)) {
      if(k>8) {
        return("high_k")
      }else{
          return(RColorBrewer::brewer.pal(k,palette))
      }
    }
    
    elevencols <- c("Spectral","RdYlGn","RdYlBu","RdGy","RdBu","PuOr","PRGn","PiYG","BrBG")
    if(any(palette %in% elevencols)) {
      if(k>11) {
        return("high_k")
        }else{
            return(RColorBrewer::brewer.pal(k,palette))
        }
      }
    
    twelvecols <- c("Set3","Paired")
    if(any(palette %in% twelvecols)) {
      if(k>12) {
        return("high_k")
        }else{
          return(RColorBrewer::brewer.pal(k,palette))
        }
      }
  }
}

# divopenh3: collapsable panel uncollapsed
divopenh3 <- function(id=NA,title=NA,...)
{
  div(HTML(paste0("<h3 data-toggle='collapse' data-target='#",id,"'>",title,"</h3>",sep="")),
      div(id=id,class='collapse in',...)
  )
}

# divopenh4: collapsable panel uncollapsed
divopenh4 <- function(id=NA,title=NA,...)
{
  div(HTML(paste0("<h4 data-toggle='collapse' data-target='#",id,"'>",title,"</h4>",sep="")),
    div(id=id,class='collapse in',...)
  )
}

# divclosedh4: collapsable panel collapsed
divclosedh4 <- function(id=NA,title=NA,...)
{
  div(style="padding-right:5px;padding-left:5px;",
    HTML(paste0("<h4 data-toggle='collapse' data-target='#",id,"'>",title,"</h4>",sep="")),
    div(id=id,class='collapse',...)
  )
}

# divgrey: gray div
divgrey <- function(...)
{
  div(style="background-color:#d4d8d8;padding:5px;padding-right:10px;padding-left:10px;border-radius:3px;",...)
}

# evannoMethodStructureCalculationWa -------------------------------------------

evannoMethodStructureCalculationWa <- function(data=NULL)
{
  #does df data contain any data
  if (length(data) == 0) stop("No input data.")
  if (is.null(data)) stop("No input data.")
  #make sure dataframe
  data <- as.data.frame(data)
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  cold <- colnames(data)
  
  #is column loci available
  if (!"loci" %in% cold) stop("Column loci not available.")
  #is column ind available
  if (!"ind" %in% cold) stop("Column ind not available.")
  #is column k available
  if (!"k" %in% cold) stop("Column k not available.")
  #is column runs available
  if (!"runs" %in% cold) stop("Column runs not available.")
  #is column elpdmean available
  if (!"elpdmean" %in% cold) stop("Column elpdmean not available.")
  #is column elpdsd available
  if (!"elpdsd" %in% cold) stop("Column elpdsd not available.")
  #is column minelpd available
  if (!"elpdmin" %in% cold) stop("Column elpdmin not available.")
  #is column maxelpd available
  if (!"elpdmax" %in% cold) stop("Column elpdmax not available.")
  
  #atleast 3 values of K
  if (length(data$k) < 3) stop("Evanno method not computed. Requires at least 3 values of K.")
  #do loci vary
  if (all(data$loci[1] == data$loci) != TRUE) stop("Evanno method not computed. Number of loci vary between runs.")
  #do ind vary
  if (all(data$ind[1] == data$ind) != TRUE) stop("The Evanno method not computed. Number of individuals vary between runs.")
  #are k values sequential
  is.sequential <- function(x) all(abs(diff(x)) == 1)
  if (!is.sequential(data$k)) stop("Evanno method not computed. Requires increasing sequential values of K.")
  #repeats of k<2
  if (any(data$runs < 2)) stop("Evanno method not computed. Repeats (runs) for some value of K is less than 2.")
  
  #convert dataframe to list
  datal <- as.list(data)
  
  #Loop to get first derivative of l(K) and its sd
  drv1 <- vector(length = nrow(data)-1, mode = "numeric")
  drv1sd <- vector(length = nrow(data)-1, mode = "numeric")
  i <- 1
  len1 <- length(datal$elpdmean)
  while (i < len1)
  {
    drv1[i] <- round(datal$elpdmean[i+1]-datal$elpdmean[i],3)
    drv1sd[i] <- round(abs(datal$elpdsd[i+1]-datal$elpdsd[i]),3)
    i = i+1
  }
  
  #Loop to get second derivative of l(K) and its sd
  drv2 <- vector(length = nrow(data)-2, mode = "numeric")
  drv2sd <- vector(length = nrow(data)-2, mode = "numeric")
  i <- 1
  len1 <- length(drv1)
  while (i < len1)
  {
    drv2[i] <- round(abs(drv1[i+1]-drv1[i]),3)
    drv2sd[i] <- round(abs(drv1sd[i+1]-drv1sd[i]),3)
    i = i+1
  }
  
  #add NA to SD vector 1 and 2
  drv1sdf <- c(NA, drv1sd)
  drv2sdf <- c(NA, drv2sd, NA)
  
  datal$drv1 <- c(NA, drv1)
  datal$drv1max <- round((datal$drv1+drv1sdf),3)
  datal$drv1min <- round((datal$drv1-drv1sdf),3)
  datal$drv2 <- c(NA, drv2, NA)
  datal$drv2max <- round((datal$drv2+drv2sdf),3)
  datal$drv2min <- round((datal$drv2-drv2sdf),3)
  datal$drv3 <- round(abs(datal$drv2)/datal$elpdsd,3)
  #datal$BestK <- ""
  #bestpos <- (1:length(datal$drv3))[(datal$drv3) == max(datal$drv3, na.rm = TRUE)]
  #bestpos <- bestpos[!is.na(bestpos)]
  #datal$BestK[bestpos] <- "*"
  
  data <- data.frame(datal)
  rm(datal)
  colnames(data)[9:15] <- c("lnk1","lnk1max","lnk1min","lnk2","lnk2max","lnk2min","deltaK")
  
  #return table
  return(data)
}

# evannoMethodStructurePlotWa --------------------------------------------------

evannoMethodStructurePlotWa <- function(data=NULL,textcol="grey30",
                                        pointsize=1.5,pointcol="steelblue",pointtype=20,
                                        linesize=0.2,linecol="steelblue",linetype=1,
                                        ebcol="grey30",ebwidth=0.15,basesize=5)
{
  #does df data contain any data
  if (length(data) == 0) stop("No input data.")
  if (is.null(data)) stop("No input data.")
  #make sure dataframe
  data <- as.data.frame(data)
  cold <- colnames(data)
  
  #if(!all(c("lnk1","lnk1max","lnk1min","lnk2","lnk2max","lnk2min","deltaK") %in% cold)) stop("evannoMethodStructurePlotWa: Missing columns.")
  
  #linesize <- base_size*0.04
  #pointsize <- base_size*0.3
  gridsize <- basesize*0.03
  
  #create plots list
  plist <- vector("list")
  
  #plot1
  plist[[1]] <- ggplot2::ggplot(data, aes(x=k, y=elpdmean))+
    geom_path(colour=linecol, size=linesize, linetype=linetype, na.rm=TRUE)+
    geom_point(colour=pointcol,fill=pointcol, size=pointsize, shape=pointtype, na.rm=TRUE)+
    geom_errorbar(aes(x=k, ymax=elpdmax, ymin=elpdmin,width=ebwidth),size=linesize,colour=ebcol,na.rm=TRUE)+
    labs(x=expression(paste(italic(K))), y=expression(paste("Mean L(", italic(K), ") " %+-% " SD")),title="A")+
    theme_bw(base_size = basesize)
  
  #plot 2
  plist[[2]] <- ggplot2::ggplot(data, aes(x=k, y=lnk1))+
    geom_path(colour=linecol, size=linesize, linetype=linetype, na.rm=TRUE)+
    geom_point(colour=pointcol, fill=pointcol, size=pointsize, shape=pointtype, na.rm=TRUE)+
    geom_errorbar(aes(x=k, ymax=lnk1max, ymin=lnk1min, width=ebwidth),size=linesize, colour=ebcol,na.rm=TRUE)+
    labs(x=expression(paste(italic(K))), y=expression(paste("L'(", italic(K), ") " %+-% " SD")), title="B")+
    theme_bw(base_size = basesize)
  
  #plot 3
  plist[[3]] <- ggplot2::ggplot(data, aes(x=k, y=lnk2))+
    geom_path(colour=linecol, size=linesize, linetype=linetype, na.rm=TRUE)+
    geom_point(colour=pointcol, fill=pointcol, size=pointsize, shape=pointtype, na.rm=TRUE)+
    geom_errorbar(aes(x=k, ymax=lnk2max, ymin=lnk2min, width=ebwidth),size=linesize, colour=ebcol, na.rm=TRUE)+
    labs(x=expression(paste(italic(K))), y=expression(paste("|L\"(", italic(K), ")| " %+-% " SD")), title="C")+
    theme_bw(base_size = basesize)
  
  #plot 4
  if(is.finite(sum(data$drv3, na.rm=TRUE)))
  {
    plist[[4]] <- ggplot2::ggplot(data, aes(x=k, y=deltaK))+
      geom_path(colour=linecol, size=linesize, linetype=linetype, na.rm=TRUE)+
      geom_point(colour=pointcol, fill=pointcol, size=pointsize, shape=pointtype, na.rm=TRUE)+
      #labs(x=expression(paste(italic(K))), y=expression(paste(Delta,italic(K))), title="D")+
      labs(x=expression(paste(italic(K))), y=expression(paste(Delta,"(", italic(K),")")), title="D")+
      theme_bw(base_size = basesize)
  }
  
  plen <- length(plist)
  for (r in 1:plen)
  {
    plist[[r]] <- plist[[r]] + theme(legend.position="none",
                                     axis.text.y=element_text(angle=90,hjust=0.5,colour=textcol),
                                     axis.text.x=element_text(colour=textcol),
                                     axis.title=element_text(colour=textcol,face="bold"),
                                     plot.title=element_text(hjust=0, colour=textcol),
                                     panel.border=element_blank(),
                                     axis.ticks=element_blank(),
                                     panel.grid.minor=element_blank(),
                                     panel.grid.major=element_line(size=gridsize),
                                     plot.margin=grid::unit(c(0.1,0.1,0.1,0.1),"cm"))
  }
  
  return(plist)
}

# intPlotQ ---------------------------------------------------------------------

#' @title intPlotQ
#' @description Processes qlist data.frame for interactive plot
#' @param dfr A qlist data.frame
#' @param sortind A character, one of 'label','all' or a cluster.
#' @param grplab A group label data.frame
#' @param selgrp A character label one of grplab title
#' @param ordergrp A logical indicating if the data must be ordered by grplab
#' @param subsetgrp A character denoting a label text one of selgrp
#' @param grpmean A logical
#' @param useindlab A logical
#' @param useindlabwithgrplab A logical
#' @param indlabsep A character denoting separator between grplab and indlab
#' 
intPlotQ <- function(dfr,sortind=NA,grplab=NA,selgrp=NA,ordergrp=F,subsetgrp=NA,grpmean=F,useindlab=T,indlabwithgrplab=T,indlabsep=" ")
{
  # check grplabels
  if(!all(is.na(grplab)))
  {
    verifyGrplab(grplab)
    grplablen <- ncol(grplab)
    grplabloop <- grplab
    grplabcheck <- TRUE
  }else{
    grplabcheck <- FALSE
  }

  # add rownames
  if(!useindlab) row.names(dfr) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dfr))),collapse=""),nchar(nrow(dfr)),"d"),1:nrow(dfr))
  corder <- 1:nrow(dfr)
  
  #ordering grps
  if(grplabcheck)
  {
    templist <- grpLabels(dframe=dfr,grplab=grplabloop,selgrp=selgrp,
                                      subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,
                                      indlabwithgrplab=indlabwithgrplab,indlabsep=indlabsep,
                                      corder=corder)
    dfr <- templist$dframe
    grplabloop <- templist$grplab
    corder <- templist$corder
    rm(templist)
    
    # sorting individuals
    if(!is.na(sortind))
    {
      templist <- sortInd(dframe=dfr,grplab=grplabloop,selgrp=selgrp,ordergrp=ordergrp,
                                      sortind=sortind,corder=corder)
      dfr <- templist$dframe
      grplabloop <- templist$grplab
      corder <- templist$corder
      rm(templist)
    }
  }else{
    # sorting individuals
    if(!is.na(sortind))
    {
      templist <- sortInd(dframe=dfr,sortind=sortind,corder=corder)
      dfr <- templist$dframe
      corder <- templist$corder
      rm(templist)
    }
  }

  #rownames(grplabloop) <- rownames(dfr)
  #dfr <- merge(dfr,grplabloop,by=0)
  #rownames(dfr) <- dfr$Row.names
  #dfr$Row.names <- NULL
  
  dfr$ind <- as.character(rownames(dfr))
  #dfr$run <- factor(rep(1,length.out=nrow(dfr)))
  dfr$x <- seq(from=1,to=nrow(dfr))
  #dfr$corder <- corder

  if(grplabcheck)
  {
    fun1 <- function(x) paste0(paste0("<b>",colnames(grplabloop),":</b> "),x,collapse="<br>")
    dfr$popup <- paste0(paste0("<b>Cur Pos:</b> ",dfr$x,"<br><b>Orig Pos:</b> ",
                               corder,"<br><b>ID:</b> ",dfr$ind,"<br>"),
                        apply(grplabloop,MARGIN=1,fun1))
  }else{
    dfr$popup <- paste0("<b>Cur Pos:</b> ",dfr$x,"<br><b>Orig Pos:</b> ",
                        corder,"<br><b>ID:</b> ",dfr$ind)
  }
  
  df2 <- dfr %>% tidyr::gather(key="group",value="y",-x,-popup,-ind)
  df2$popup <- paste0(df2$popup,paste0("<br><b>Cluster:</b> ",df2$group,"<br><b>Value:</b> ",df2$y))
  
  return(list(df=df2,ind=dfr$ind))
}

# readQWa ------------------------------------------------------------------------

#' @title Convert run files (q-matrices) to qlist.
#' @description Takes one or more STRUCTURE, TESS, BAPS, numeric delimited run files or 
#' CLUMPP format files and converts them to a qlist (list of dataframes).
#' @param files A character or character vector of one or more files.
#' @param filenames A character vector of filenames.
#' @param filetype A character indicating input filetype. Options are 'auto','structure','tess','baps',
#' 'basic' or 'clumpp'. See details.
#' @param indlabfromfile A logical indicating if individual labels must be read 
#' from input file and used as row names for resulting dataframe. Spaces in 
#' labels may be replaced with _. Currently only applicable to STRUCTURE runs files.
#' @return A list of lists with dataframes is returned. List items are named by input filenames.
#' File extensions such as '.txt','.csv','.tsv' and '.meanQ' are removed from filename.
#' In case filenames are missing or not available, lists are named sample1, sample2 etc.
#' For STRUCTURE runs, if individual labels are present in the run file and \code{indlabfromfile=T}, 
#' they are added to the dataframe as row names. Structure metadata including loci, 
#' burnin, reps, elpd, mvll, and vll is added as attributes to each dataframe.
#' For CLUMPP files, multiple runs within one file are suffixed by -1, -2 etc.
#' @details
#' STRUCTURE, TESS and BAPS run files have unique layout and format (See vignette). BASIC files can be Admixture run files, 
#' fastStructure meanQ files or any tab-delimited, space-delimited or comma-delimited tabular data 
#' without a header. CLUMPP files can be COMBINED, ALIGNED or 
#' MERGED files. COMBINED files are generated from \code{clumppExport}. ALIGNED and 
#' MERGED files are generated by CLUMPP.
#' 
readQWa <- function(files=NULL,filenames=NULL,filetype="auto",indlabfromfile=FALSE)
{
  if(is.null(files) || (length(files)==0)) stop("readQWa: No input files.")
  if(is.null(filenames) || (length(filenames)==0)) stop("readQWa: No input filenames.")
  if(!is.character(files)) stop("readQWa: Argument 'files' is not a character datatype.")
  flen <- length(files)
  
  len <- length(files)
  dlist <- vector("list")
  for (i in 1:len)
  {
    # check file
    if(filetype=="auto") 
    {
      chk <- tolower(checkQ(files[i])$type)
      
      if(chk %in% c("structure","tess","baps","basic","clumpp")) 
      {
        if(chk=="structure") dfr <- readQStructureWa(files[i],filenames[i],indlabfromfile=indlabfromfile)
        if(chk=="tess") dfr <- readQTessWa(files[i],filenames[i])
        if(chk=="basic") dfr <- readQBasicWa(files[i],filenames[i])
        if(chk=="clumpp") dfr <- readQClumppWa(files[i],filenames[i])
        if(chk=="baps") dfr <- readQBapsWa(files[i],filenames[i])
        dlist <- append(dlist,dfr)
      }else{
        warning(paste0("readQWa: Input file ",files[i]," was not identified as a STRUCTURE, TESS, BAPS, BASIC or CLUMPP filetype. Specify 'filetype' manually or check input."))
      }
    }else{
      if(filetype=="structure") dfr <- readQStructureWa(files[i],filenames[i],indlabfromfile=indlabfromfile)
      if(filetype=="tess") dfr <- readQTessWa(files[i],filenames[i])
      if(filetype=="basic") dfr <- readQBasicWa(files[i],filenames[i])
      if(filetype=="clumpp") dfr <- readQClumppWa(files[i],filenames[i])
      if(filetype=="baps") dfr <- readQBapsWa(files[i],filenames[i])
      dlist <- append(dlist,dfr)
    }
  }
  return(dlist)
}

# readQStructureWa ------------------------------------------------------------

#' @title Convert STRUCTURE run files to qlist.
#' @description Takes one or more STRUCTURE run files and converts them to a list of dataframes.
#' @param files A character or character vector of one or more STRUCTURE run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param filenames A character vector of filenames.
#' @param indlabfromfile A logical indicating if individual labels must be read from input file and used as row names for resulting dataframe. Spaces in labels may be replaced with _.
#' @return A list of lists with dataframes is returned. If individual labels are 
#' present in the STRUCTURE file, they are added to the dataframe as row names. Structure
#' metadata including loci, burnin, reps, elpd, mvll, and vll is added as attributes 
#' to each dataframe. List items are named by input filenames.
#' 
readQStructureWa <- function(files=NULL,filenames=NULL,indlabfromfile=FALSE)
{
  if(is.null(files) | (length(files)==0)) stop("readQStructureWa: No input files.")
  if(is.null(filenames) | (length(filenames)==0)) stop("readQStructureWa: No input filenames.")
  if(!is.character(filenames)) stop("readQStructureWa: Argument 'filenames' is not a character datatype.")
  if(length(files) != length(filenames)) stop("readQStructureWa: Length of files and filenames are not equal.")
  
  #number of files selected
  flen <- length(files)
  
  #check file
  if(any(checkQ(files)$type != "STRUCTURE")) warning("readQStructureWa: Input may contain incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    fname <- filenames[i]
    file1 <- readLines(as.character(files[i]), warn=FALSE)
    
    #find individuals and get number of individuals
    ind <- as.numeric(as.character(base::gsub("\\D", "", grep("\\d individuals", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if(is.na(ind)) cat(paste0("Number of individuals is NA in file: ", fname,"\n"))
    
    #get value of k & error check
    k <- as.numeric(as.character(base::gsub("\\D", "", grep("\\d populations assumed", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if(is.na(k)) cat(paste0("Value of K is NA in file: ", fname,"\n"))
    
    #get number of loci & error check
    loci <- as.numeric(base::gsub("\\D", "", grep("\\d loci", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if(is.na(loci)) cat(paste0("Number of Loci is NA in file: ", files[i], "\n"))
    
    #get burn-in value & error check
    burnin <- as.numeric(base::gsub("\\D", "", grep("\\d Burn-in period", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if(is.na(burnin)) cat(paste0("Burn-in value is NA in file: ", files[i], "\n"))
    
    #get burn-in value & error check
    reps <- as.numeric(base::gsub("\\D", "", grep("\\d Reps", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if(is.na(reps)) cat(paste0("Reps value is NA in file: ", files[i], "\n"))
    
    #get est ln prob of data & error check
    elpd <- as.numeric(base::gsub("=", "", base::gsub("Estimated Ln Prob of Data", "", grep("Estimated Ln Prob of Data", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if(is.na(elpd)) cat(paste0("Estimated Ln Prob of Data is NA in file: ", files[i], "\n"))
    
    #get mn value of ln likelihood & error check
    mvll <- as.numeric(base::gsub("=", "", base::gsub("Mean value of ln likelihood", "", grep("Mean value of ln likelihood", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if(is.na(mvll)) cat(paste0("Mean value of ln likelihood is NA in file: ", files[i], "\n"))
    
    #get Variance of ln likelihood else NA
    vll <- as.numeric(base::gsub("=", "", base::gsub("Variance of ln likelihood", "", grep("Variance of ln likelihood", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if(is.na(vll)) cat(paste0("Variance of ln likelihood is NA in file: ", files[i], "\n"))
    
    file1 <- file1[grep(".+\\(\\d+\\).+\\:.+",file1)]
    if(length(file1)==0)
    {
      cstart <- base::charmatch("Inferred ancestry of individuals", file1)
      cend <- base::charmatch("Estimated Allele Frequencies in each", file1)
      file1 <- file1[(cstart+2):(cend-1)]
    }
    
    file_a <- file1[file1 != ""]
    rm(file1)
    
    #error check
    tc_file_a <- textConnection(file_a)
    file_b <- read.delim(tc_file_a,header=F,sep="",stringsAsFactors=F)
    close(tc_file_a)
    
    suppressWarnings(
      errorcheck <- try(
        file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F],
        silent=T)
    )
    rm(file_b)
    
    if(class(errorcheck)=="try-error")
    {
      #using manual substring
      file_a <- base::gsub("\\([0-9.,]+\\)","",file_a)
      file_b <- base::gsub(":  ", "", substr(file_a, base::regexpr(":\\W+\\d\\.\\d+", file_a), base::nchar(file_a)-1))
      file_b <- base::sub("\\s+$","",base::sub("^\\s+","",file_b))
      rm(file_a)
      file_c <- as.vector(as.numeric(as.character(unlist(base::strsplit(file_b, " ")))))
      rm(file_b)
      dframe <- as.data.frame(matrix(file_c, nrow=ind, byrow=TRUE),stringsAsFactors=FALSE)
    }else{
      #using textconnection
      tc_file_a <- textConnection(file_a)
      file_b <- read.delim(tc_file_a,header=F,sep="",stringsAsFactors=F)
      close(tc_file_a)
      dframe <- file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F]
    }
    
    dframe <- as.data.frame(sapply(dframe, as.numeric),stringsAsFactors=FALSE)
    colnames(dframe) <- paste0("Cluster", 1:ncol(dframe))
    row.names(dframe) <- 1:nrow(dframe)
    #row.names(dframe) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dframe))),collapse=""),nchar(nrow(dframe)),"d"),1:nrow(dframe))
    
    #labels
    if(indlabfromfile)
    {
      labeldf <- file_b[,(grep("[0-9]",file_b[1,])[1]+1):(grep("[(]",file_b[1,])[1]-1),drop=FALSE]
      
      if(ncol(labeldf) > 1) labeldf <- data.frame(V2=do.call(paste, c(labeldf, sep="_")),stringsAsFactors=FALSE)
      if(nrow(labeldf)==nrow(dframe))
      {
        if(any(duplicated(labeldf[,1]))) 
        {
          warning(paste0("readQStructureWa: Individual names in file ",fname," not used due to presence of duplicate names."))
        }else{
          row.names(dframe) <- as.character(labeldf[,1])
        }
      }else{
        warning(paste0("readQStructureWa: Individual names in file ",fname," not used due to incorrect length."))
      }
    }
    
    attr(dframe, "ind") <- nrow(dframe)
    attr(dframe, "k") <- ncol(dframe)
    attr(dframe, "loci") <- loci
    attr(dframe, "burnin") <- burnin
    attr(dframe, "reps") <- reps
    attr(dframe, "elpd") <- elpd
    attr(dframe, "mvll") <- mvll
    attr(dframe, "vll") <- vll
    
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  
  fnames <- sub(".txt","",filenames)
  names(dlist) <- fnames
  return(dlist)
}

# readQTessWa -----------------------------------------------------------------

#' @title Convert TESS cluster files to qlist.
#' @description Takes one or more TESS cluster run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more TESS cluster run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param filenames A character vector of filenames.
#' @return A list of lists with dataframes is returned. List items are named by input filename.
#' @details Use collectRunsTess() to collect TESS runs into one directory.
#' @examples 
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' # create a qlist
#' readQTess(tfiles)
#' @export
#'
readQTessWa <- function(files=NULL,filenames=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQTessWa: No input files.")
  if(is.null(filenames) | (length(filenames)==0)) stop("readQTessWa: No input filenames.")
  if(!is.character(filenames)) stop("readQTessWa: Argument 'filenames' is not a character datatype.")
  if(length(files) != length(filenames)) stop("readQTessWa: Length of files and filenames are not equal.")
  
  # number of files selected
  flen <- length(files)
  
  # check file
  if(any(checkQ(files)$type != "TESS")) error("readQTessWa: Input may contain incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    # read whole file in
    file1 <- readLines(files[i],warn=FALSE)
    
    # extract the cluster table part
    file1 <- file1[3:c(grep("Estimated Allele Frequencies",file1)-1)]
    
    # remove empty lines
    file1 <- file1[file1 != ""]
    
    # create a text connection
    tc_file1 <- textConnection(file1)
    
    # read as a table
    file2 <- read.delim(tc_file1,header=F,sep="\t",stringsAsFactors=FALSE)
    
    # close text connection
    close(tc_file1)
    
    # choose columns 2 to numofcols-2
    dframe <- file2[,2:(ncol(file2)-2)]
    
    # remove temporary files
    rm(file1,file2)
    
    # convert all columns to numeric
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    
    # add column names
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # add attributes
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    # add to list
    dlist[[i]] <- dframe
  }
  
  # add file names as qlist names
  fnames <- sub(".txt","",filenames)
  names(dlist) <- fnames
  
  return(dlist)
}

# readQBasicWa ---------------------------------------------------------------

#' @title Convert delimited text files to qlist.
#' @description Takes one or more delimited numeric text files and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more delimited text files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param filenames A character vector of filenames.
#' @return A list of lists with dataframes is returned. List items are named by input filename.
#' @details Input files can be Admixture run files, fastStructure meanQ files. 
#' or any tab-delimited, space-delimited or comma-delimited tabular data without header.
#' 
readQBasicWa <- function(files=NULL, filenames=NULL)
{
  if(is.null(files) | (length(files)==0)) stop("readQBasicWa: No input files.")
  if(is.null(filenames) | (length(filenames)==0)) stop("readQBasicWa: No input filenames.")
  if(!is.character(filenames)) stop("readQBasicWa: Argument 'filenames' is not a character datatype.")
  if(length(files) != length(filenames)) stop("readQBasicWa: Length of files and filenames are not equal.")
  #number of files selected
  flen <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep=""))
  
  #check file
  chk <- checkQ(files)
  if(any(chk$type != "BASIC")) stop("readQBasicWa: Incorrect input format.")
  if(any(is.na(chk$subtype))) stop("readQBasicWa: Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    # Test space-delim, tab-delim or comma-delim files here
    if(chk$subtype[i]=="SPACE") dframe <- read.delim(files[i], header=F, sep="", dec=".", stringsAsFactors=FALSE)
    if(chk$subtype[i]=="TAB") dframe <- read.delim(files[i], header=F, sep="\t", dec=".", stringsAsFactors=FALSE)
    if(chk$subtype[i]=="COMMA") dframe <- read.delim(files[i], header=F, sep=",", dec=".", stringsAsFactors=FALSE)
    
    if(!all(sapply(dframe, is.numeric))) stop("readQBasicWa: One or more columns are not numeric.")
    colnames(dframe) <- paste0("Cluster", 1:ncol(dframe))
    
    attr(dframe, "ind") <- nrow(dframe)
    attr(dframe, "k") <- ncol(dframe)
    
    dlist[[i]] <- dframe
  }
  
  filenames <- sub(".txt","",filenames)
  filenames <- sub(".tsv","",filenames)
  filenames <- sub(".csv","",filenames)
  filenames <- sub(".meanQ","",filenames)
  
  names(dlist) <- filenames
  return(dlist)
}

# readQClumppWa ---------------------------------------------------------------

#' @title Convert CLUMPP format numeric text files to qlist.
#' @description Takes one or more CLUMPP format numeric text files and converts
#' them to a list of dataframes.
#' @param files A character or character vector of one or more COMBINED, ALIGNED or 
#' MERGED files. COMBINED files are generated from \code{clumppExport}. ALIGNED and 
#' MERGED files are generated by CLUMPP. Use \code{choose.files(multi=TRUE)} to 
#' select interactively.
#' @param filenames A character vector of filenames.
#' @return A list of lists with dataframes is returned. Each list item is named by 
#' input filename. Multiple runs within one file are suffixed by -1, -2 etc.
#' 
readQClumppWa <- function(files=NULL,filenames=NULL)
{
  if(is.null(files) | (length(files)==0)) stop("readQClumppWa: No input files.")
  if(is.null(filenames) | (length(filenames)==0)) stop("readQClumppWa: No input filenames.")
  if(!is.character(filenames)) stop("readQClumppWa: Argument 'filenames' is not a character datatype.")
  if(length(files) != length(filenames)) stop("readQClumppWa: Length of files and filenames are not equal.")
  
  #number of files selected
  flen <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep=""))
  
  #check file
  chk <- checkQ(files)
  if(any(chk$type != "CLUMPP")) stop("readQClumpp: Incorrect input format.")
  
  i <- 1
  k <- 1
  dlist <- vector("list")
  snames <- vector()
  len1 <- length(files)
  for (i in 1:len1)
  {
    fname <- base::gsub(".txt", "", filenames[i])
    
    df1 <- read.table(files[i],header=F, sep="", dec=".", quote="",stringsAsFactors=FALSE)
    if(class(df1)!="data.frame") stop("readQClumppWa: Read error. Check input format.")
    
    df1[,1] <- factor(df1[ ,1])
    indlev <- levels(df1[,1])
    Ind <- as.numeric(as.character(length(indlev)))
    tempb <- as.numeric(nrow(df1))
    numruns <- as.numeric(tempb/Ind)
    numk <- ncol(df1) - 2
    
    df2 <- data.frame(Num=factor(rep(1:numruns, 1, each=Ind)), 
                      Ind=factor(rep(1:Ind, numruns)), 
                      df1[, 2:(numk+1)],stringsAsFactors=FALSE)
    colnames(df2)[3:ncol(df2)] <- paste0("Cluster",1:(ncol(df2)-2))
    
    for(j in 1:numruns)
    {
      dframe <- subset(df2,df2$Num==j)
      dframe$Num <- NULL
      dframe$Ind <- NULL
      snames <- c(snames,paste0(fname,"-",j))
      
      if(!all(sapply(dframe, is.numeric))) stop("readQClumppWa: One or more columns are not numeric.")
      
      attr(dframe, "ind") <- nrow(dframe)
      attr(dframe, "k") <- ncol(dframe)
      
      dlist[[k]] <- dframe
      k <- k+1
    }
  }
  
  snames <- sub(".txt","",snames)
  snames <- sub(".tsv","",snames)
  snames <- sub(".csv","",snames)
  snames <- sub(".meanQ","",snames)
  
  names(dlist) <- snames
  return(dlist)
}

# readQBapsWa -----------------------------------------------------------------

#' @title Convert BAPS cluster files to qlist.
#' @description Takes one or more BAPS cluster run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more BAPS cluster run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param filenames A character vector of filenames.
#' @return A list of lists with dataframes is returned. List items are named by input filename.
#'
readQBapsWa <- function(files=NULL,filenames=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQBapsWa: No input files.")
  if(is.null(filenames) | (length(filenames)==0)) stop("readQBapsWa: No input filenames.")
  if(!is.character(filenames)) stop("readQBapsWa: Argument 'filenames' is not a character datatype.")
  if(length(files) != length(filenames)) stop("readQBapsWa: Length of files and filenames are not equal.")
  
  # number of files selected
  flen <- length(files)
  
  # check if file type is BAPS
  if(any(checkQ(files)$type != "BAPS")) error("readQBapsWa: Input may be in incorrect format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    # read in all lines from file
    file1 <- readLines(files[i],warn=FALSE)
    
    # extract the cluster table part
    file1 <- file1[grep("^1:",file1):length(file1)]
    
    # read table using delimiter : and use column V2
    tc_file1 <- textConnection(file1)
    file2 <- read.delim(tc_file1,sep=":",header=F,stringsAsFactors=F)$V2
    
    # read table using delimiter space
    tc_file2 <- textConnection(file2)
    dframe <- read.delim(tc_file2,sep="",header=F,stringsAsFactors=F)
    
    # close text connections
    close(tc_file1,tc_file2)
    
    # remove temporary objects
    rm(file1,file2)
    
    # convert all columns to numeric
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    
    # create valid column names
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # attach attributes to dataframe
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    # place dataframe in a list
    dlist[[i]] <- dframe
  }
  
  # remove .txt in all file names
  fnames <- sub(".txt","",filenames)
  # label qlist objects with file names
  names(dlist) <- fnames
  
  return(dlist)
}

# clumppExportWa -----------------------------------------------------------------

#' @title Generate CLUMPP output from a qlist
#' @description Takes a qlist and combines several repeats for each K into a 
#' single file along with a parameter file suitable for input to CLUMPP. The two 
#' output files are organised into folders by K. CLUMPP is executed automatically 
#' when \code{useexe=T}, else the CLUMPP executable file can be copied to the 
#' output directories and run to reorder the clusters for each K.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param prefix A character prefix for folder names. By default, set to 'pop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental). May not work on all OS and versions.
#' @param exd Path to executables directory
#' @param currwd Path to working directory where clumpp files are to be exported.
#' @return The combined file and paramfile are written into respective folders 
#' named by K.
#' 
clumppExportWa <- function(qlist=NULL,prefix=NA,parammode=NA,paramrep=NA,useexe=FALSE,exd=NA,currwd=NA)
{
  # check input
  is.qlist(qlist)
  
  if(is.na(prefix)) prefix <- "clumpp"
  prefix <- paste0(prefix,"_k")
  if(!is.logical(useexe)) stop("clumppExport: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  # get tabulated runs
  df1 <- tabulateQ(qlist)
  df2 <- summariseQ(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  if(is.null(names(qlist))) names(qlist) <- paste0("sample",1:length(qlist))
  
  # k val duplicated
  if(any(duplicated(df2l$k))) stop("clumppExport: Repeating values of K found.")
  # do ind vary?
  if(!all(df2l$ind[1]==df2l$ind)) warning("clumppExport: Number of individuals vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length=runs)
    for (f in 1:runs)
    {
      sel <- which(names(qlist)==as.character(df1l$file[p]))
      dframe1 <- qlist[[sel]]
      
      # generate df
      dframe3 <- as.matrix(data.frame(V1=paste0(1:ind,":"),dframe1,last=as.character(rep(1,ind)),stringsAsFactors=FALSE))
      
      # add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p=p+1
    }
    
    if(runs > 1 && k > 1)
    {
      if(is.na(currwd)) currwd <- getwd()
      if(as.numeric(file.access(currwd,2))==-1) stop(paste0("clumppExport: Directory ",currwd," has no write permission."))
      
      if(!dir.exists(paste0(currwd,"/clumpp"))) dir.create(paste0(currwd,"/clumpp"))
      setwd(paste0(currwd,"/clumpp"))
      cat(paste0("Folder created: ",basename(getwd()),"\n"))  
      out <- paste0(prefix,k,".txt")
      
      ## file output block
      
      # make 2 line space
      spacer <- matrix(rep("  ",(k+2)*2),nrow=2)
      
      # write file
      write(t(format(ldata[[1]],nsmall=15)),paste(out),ncolumns=k+2)
      for (i in 2:length(ldata))
      {
        write(t(spacer),paste(out),ncolumns=k+2,append=TRUE)
        write(t(format(ldata[[i]],nsmall=15)),append=TRUE,paste(out),ncolumns=k+2)
      }
      cat(paste0(out),"exported.\n")
      
      ## paramfile section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if(T1 <= 100000000)
      {
        if(is.na(parammode)) parammode <- 2
        if(is.na(paramrep)) paramrep <- 20
      }else{
        if(is.na(parammode)) parammode <- 3
        if(is.na(paramrep)) paramrep <- 500
      }
      out1 <- base::gsub(".txt","",out)
      params <- c("DATATYPE 1 ",
                  "INDFILE NOTNEEDED.indfile ",
                  paste0("POPFILE ",out," "),
                  paste0("OUTFILE ",out1,"-merged.txt "),
                  paste0("MISCFILE ",out1,"-miscfile.txt "),
                  paste0("K ",k," "),
                  paste0("C ",ind," "),
                  paste0("R ",length(ldata)," "),
                  paste0("M ",parammode," "),
                  "W 0 ",
                  "S 2 ",
                  "GREEDY_OPTION 2 ",
                  paste0("REPEATS ",paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params,"paramfile")
      cat(paste0("paramfile exported.\n"))
      
      # autorun clumpp executable
      if(useexe)
      {

        if(tolower(Sys.info()[['sysname']])=="windows")
        {
          file.copy(paste0(exd,"/clumpp_windows_1.1.2b.exe"),".")
          tryCatch(system("clumpp_windows_1.1.2b.exe"),error=function(c) paste0("CLUMPP execution failed."))
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(tolower(Sys.info()[['sysname']])=="darwin")
        {
          file.copy(paste0(exd,"/clumpp_mac_1.1.2b"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          tryCatch(system("./clumpp_mac_1.1.2b"),error=function(c) paste0("CLUMPP execution failed."))
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if((tolower(Sys.info()[['sysname']])=="linux") && (tolower(Sys.info()[['machine']])=="x86_32"))
        {
          file.copy(paste0(exd,"/clumpp_linux_1.1.2b_32bit"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          tryCatch(system("./clumpp_linux_1.1.2b_32bit"),error=function(c) paste0("CLUMPP execution failed."))
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if((tolower(Sys.info()[['sysname']])=="linux") && (tolower(Sys.info()[['machine']])=="x86_64"))
        {
          file.copy(paste0(exd,"/clumpp_linux_1.1.2b_64bit"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          tryCatch(system("./clumpp_linux_1.1.2b_64bit"),error = function(c) paste0("CLUMPP execution failed."))
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }

      }
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if(k==1) message(paste0(prefix,k," not exported. K less than 2.\n"))
      if(runs < 2) message(paste0(prefix,k," not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

# summariseQ -------------------------------------------------------------------
## custom summariseQ() that can accept one run

#' @title Summarise a tabulated dataframe
#' @description Creates a summary table from a tabulated dataframe of two or 
#' more runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateQ()}. 
#' Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported 
#' as a tab-delimited text file in the working directory.
#' @return Returns a dataframe with all values of K sorted by K. The table has 
#' 3 columns namely value of K, number of runs for each K and number of 
#' individuals.
#' If the input file is derived from STRUCTURE runs, the table is sorted by loci 
#' as well. Other columns include elpdmean, elpdsd, elpdmin and elpdmax.
#' 
summariseQ <- summarizeQ <- function(data=NULL,writetable=FALSE)
{
  # does df data contain any data?
  if(is.null(data) || length(data)==0) stop("summariseQ: No input files.")
  if(!is.logical(writetable)) stop("summariseQ: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  
  # make sure dataframe
  if(class(data) != "data.frame") stop("summariseQ: Input is not a dataframe.")
  # convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  # is column k available?
  if(length(grep("k",colnames(data)))==0) stop("summariseQ: Column k not available.")
  # is column ind available?
  if(length(grep("ind",colnames(data)))==0) stop("summariseQ: Column ind not available.")
  
  # check
  #if(nrow(data) < 2) stop("summariseQ: At least 2 runs are required for this function.")
  
  if(all(c("k","ind","loci","elpd") %in% colnames(data)))
  {
    dframe1 <- stats::aggregate(elpd ~ loci + ind + k,data=data,length)
    colnames(dframe1)[4] <- "runs"
    dframe2 <- aggregate(elpd ~ loci + ind + k,data=data,FUN=function(x) c(elpdmean =mean(x,na.rm=TRUE),elpdsd=sd(x,na.rm=TRUE),elpdmin=min(x,na.rm=TRUE),elpdmax=max(x,na.rm=TRUE)))[,-c(1:3)]
    dframe1 <- cbind(dframe1,dframe2)
  }else{
    dframe1 <- stats::aggregate(file ~ ind + k,data=data[,c("file","k","ind")],length)
    colnames(dframe1)[3] <- "runs"
  }
  
  # write table if opted
  if(writetable) write.table(dframe1,"summariseQ.txt",quote=FALSE,row.names=FALSE,sep="\t",dec=".")
  
  return(dframe1)
}
