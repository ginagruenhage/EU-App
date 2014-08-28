library(shiny)

shinyUI(fluidPage(
          titlePanel("Comparison of EU countries"),
          
          sidebarLayout(
            sidebarPanel(

              h3("About"),
              p("In this application of cMDS we compare member countries of the European Union. The data consists of demographic and economic variables, for example life expectancy and income per capita. Two distance matrices were built based on the two feature sets. These two matrices are then weighted to build a continuous weighted metric between EU countries. The weighting is either linear or convex. The corresponding 2d embedding is shown in the main panel. The euclidean distances approximate the original distances. You can view the embedding for one weighting level at time. You can scroll through the weight variable via the bottom slider. The plot is updated at once when you change any of the settings."),
              br(),
              helpText("Choose the number of bins for the weighting:"),
              
              numericInput("bins", label = "Bin Number", value = 10),
              
              helpText("Choose the smoothing parameter lambda:"),
              
              numericInput("lambda", label = "Lambda", value = 1),

              helpText("Choose the initialization method:"),

              selectInput("init", label = "Initialization Method", choices = list("average" = "average", "smacof" = "smacof", "random" = "random"), selected = "average"),
              
              helpText("Choose weighting method:"),
              
              checkboxInput("convex", label = "Convex Weighting", value = FALSE),

              br(),
              hr(),

              helpText("Choose the weight for which you want to plot the embedding:"),

              sliderInput("alpha", "Economic weight:", min = 0, max = 1, value = 0)
              ),
            
            mainPanel(
              h3("cMDS embedding"),
              htmlOutput("plot"),
              br(),
              p("The total cost of the embedding:"),
              verbatimTextOutput("text"),
              h3("About cMDS"),
              p("For this application we use cMDS with the R package",code("cmdsr"),code("cmdsr"),"provides tools to compute and analyze continuous embeddings that are computed using cMDS. Continuous embeddings are a useful way to visualize data that has inherent continuous parameters, such as time or experimental control variables. It is also particularly useful to visualize the effects of changing distance functions. An example could be a family of distance functions that is parameterized by a hyperparameter, such as scale. Applying different distance functions from this family to a dataset might lead to significantly different output. Various effects of such changing distances on data can be visualized using cMDS. 
You can track the development of",code("cmdsr"),"at",a("https://github.com/ginagruenhage/cmdsr",href = "https://github.com/ginagruenhage/cmdsr"),".",code("cmdsr"),"is easy to install using the",code("devtools")," package:",code("devtools::install_github('cmdsr','ginagruenhage')"),"."),
              br(),
              p("The cMDS algorithm is described in: Gina Gruenhage & Simon Barthelme, Visualizing the effects of a changing distance using continuous embeddings, ",a("arxiv.org/abs/1311.1911",href = "arxiv.org/abs/1311.1911"),".")
             
              )
            )
          )
        )
