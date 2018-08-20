# pophelperShiny 2.0.0

`pophelperShiny` compliments the R package `pophelper` by providing a graphical user interface around the fuctions. This app is used to analyse and visualise population structure from programs such as STRUCTURE, TESS, TESS3, BAPS and numeric delimited formats such as ADMIXTURE or fastSTRUCTURE. The app can be used to read run files to R, tabulate runs, summarise runs, estimate *K* using the Evanno method, align clusters within K using CLUMPP and generate barplot figures.  

![A preview of the app.](./images/preview.jpg)

## Usage

The app can be installed as a regular R package and run locally. See instructions below. The app can also be accessed online at [pophelper.com](http://www.pophelper.com). 

## Installation  

You need to have the latest [JAVA JDK and JRE](http://www.oracle.com/technetwork/java/javase/downloads/index.html) installed on your system. Mac and Linux users will need to have Cairo graphics installed separately. You need to have R (> 3.5.0) statistical package installed. [R](https://www.r-project.org/) is open-source and freely available to download for Windows, Mac and other OS. Then, basically run the code below. This installs the dependency packages, then it installs `pophelperShiny` from `github` using the `devtools` package.  

```coffee
# install dependencies and devtools
install.packages(c("Cairo","colourpicker","dplyr","DT","fields","ggplot2",
                   "gridExtra","gtable","highcharter","markdown","RColorBrewer",
                   "rJava","shiny","shinyAce","shinyBS","shinyjs","shinythemes",
                   "tidyr","viridisLite","xlsx","devtools"),dependencies=T)

# install the package from GitHub
devtools::install_github('royfrancis/pophelperShiny')

# load library for use
library(pophelperShiny)

# launch app
runPophelper()
```

If using the RStudio browser, click on 'Open in Browser' to open in a system browser. This allows to download files. Note that this app includes binary executable CLUMPP for aligning run within K. This may not work on all OS and versions.

`pophelperShiny` has been currently tested on the following systems: 

+ Windows 10, R 3.5.0  
+ Windows 7 64bit, R 3.5.1  
+ Mac High Sierra 10.13.1, R 3.5.1  
+ CentOS Linux release 7.5.1804 , R 3.5.0  (App does not close gracefully)
+ Ubuntu Linux 16.04 LTS, R 3.5.1  

### Disclaimer

The `pophelperShiny` R package is offered free and without warranty of any kind, either expressed or implied. I will not be held liable to you for any damage arising out of the use, modification or inability to use this program. Please make sure you verify all your results.  

### Contact

If you have an comments, suggestions, corrections or ideas on ways to improve or extend this package, feel free to contact me. Submit an issue on the [Github issues page](https://github.com/royfrancis/pophelperShiny/issues).  

2018 | Roy M Francis  
