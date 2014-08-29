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
  
  ## dataInput <- reactive(function(){
  ##   #x <- subset(embed(), alpha < alpha.val() + 1e-5 & alpha > alpha.val() - 1e-5)
  ##   x <- embed()
  ##   x <- x[,select = c("cmds.x1","cmds.x2","alpha","iso")]
  ##   names(x) <- c("x","y","timevar","idvar")
  ##   x
  ## })

  ## vis <- reactive({
  ##   embed() %>% ggvis(~cmds.x1, ~cmds.x2, size = 5) %>% layer_points() 
  ## })
  vis <- reactive({
    plot.timestep.ggvis(embed(),alpha.val(),limits())
  })
  vis %>% bind_shiny("myplot",controls_id="myplot_ui")
  ## gv <- reactive({
  ##   dataInput() %>% ggvis(~cmds.x1, ~cmds.x2, size = 5) %>%
  ##   layer_points()
  ##   })

  #output$controls <- renderControls(gv)
  #ovserve_ggvis(gv,"my_plot",session)
                  
  output$text <- renderPrint({
    print(summary.cmds(res())$Error)
  })

  
})
