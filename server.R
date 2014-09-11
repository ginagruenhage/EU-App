# server.R

require(plyr)
require(stringr)
library(rworldmap)
library(cmdsr)
library(ggvis)

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
