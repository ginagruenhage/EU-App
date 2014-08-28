# server.R

require(plyr)
require(stringr)
library(rworldmap)
library(cmdsr)
library(googleVis)

source("helpers.R")

info.df <- read.table("data/df.csv")
Ds <- compute.dmat(info.df)

shinyServer(function(input, output) {

  dataInput <- reactive({
    DistL <- compute.Dlist(Ds, T = input$bins+1, convex = input$convex)
    return(DistL)
  })

  res <- reactive({
    res <- cmds(dataInput(), k=2, l = input$lambda, init = input$init)
    return(res)
  })

  embed <- reactive({
    embed <- make.df(res(),info.df)
    embed <- mutate(embed, time = alpha*(input$bins + 1))
    return(embed)
    })

  limits <- reactive({
    compute.limits(embed())
    })

  alpha <- reactive({
    compute.alpha(input$bins,input$alpha)
  })

  output$plot <- renderGvis({
    gvisMotionChart(subset(embed(),select = c("iso","time","cmds.x1","cmds.x2")), idvar = "iso", timevar = "time", xvar = "cmds.x1", yvar = "cmds.x2", options = list(showSidePanel = FALSE))
  })

  output$text <- renderPrint({
    print(summary.cmds(res())$Error)
  })

  
})
