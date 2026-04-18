
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

left_sidebar <- sidebar(
  numericInput("learning",
               "Learning Slope %",
               100,
               0,
               100,
               1),
  numericInput("rate",
               "Rate Slope %",
               100,
               0,
               100,
               1),
  numericInput("t1",
               "T1 Cost",
               1,
               0,
               NA,
               1)
)

page_sidebar(
  plotOutput("curve_plot"),
  sidebar = left_sidebar,
  title = "LRC Comparison"
)
