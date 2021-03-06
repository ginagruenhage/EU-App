# server.R

require(plyr)
require(stringr)
library(rworldmap)
library(cmdsr)

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

  alpha <- reactive({
    compute.alpha(input$bins,input$alpha)
  })
  
  output$plot <- renderPlot({   
    plot.timestep(embed(),alpha(),limits())
  })

  output$text <- renderPrint({
    print(summary.cmds(res())$Error)
  })

  
})
