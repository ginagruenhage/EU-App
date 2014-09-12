# server.R

require(plyr)
require(stringr)
library(rworldmap)
library(cmdsr)
require(ggvis)
require(ggplot2)
require(devtools)

require("ggplot2")
    require("reshape2")
    require("plyr")
    require("yaImpute")
    require("audio")
    require("scales")
    #require("ppMeasures")
    require("stringr")
    require("Matrix")
    require("fda")
    #require("unit")
    require("vegan")
    require("mnormt")
    require("grid")
    require("igraph")
    require("mgcv")
    require("Rcpp")
    require("RcppArmadillo")
    require("animation")
    require("sna")
    require("devtools")
    require("roxygen2")
    require('MASS')
    require('smacof')
    require('knitr')
    require('shiny')
    require('ggvis')
source("/home/gina/EigeneDateien/Work/PHD/utility_functions/utils.R")
    source("/home/gina/EigeneDateien/Work/PHD/utility_functions/aliases.R")
source("helpers.R")

info.df <- read.table("data/df.csv")
Ds <- compute.dmat(info.df)

shinyServer(function(input, output) {

  dataInput <- reactive({
    DistL <- compute.Dlist(Ds, T = input$bins, convex = input$convex)
    return(DistL)
  })

  res <- reactive({
    res <- cmds(dataInput(), k=2, l = input$lambda, init = input$init)
    return(res)
  })

  embed <- reactive({
    embed <- make.df(res(),info.df)
    })

  limits <- reactive({
    compute.limits(embed())
    })

  alpha.val <- reactive({
    compute.alpha(input$bins,input$alpha)
  })
  
  vis <- reactive({
    plot.timestep(embed(),alpha.val(),limits(),input$check.pen)
  })
  vis %>% bind_shiny("myplot",controls_id="myplot_ui")
                    
  output$text <- renderPrint({
    #print(summary.cmds(res()))
    print(summary.cmds(res())$Error)    
  })

  
})
