
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

set_theme(theme_grey(base_size = 15))

left_sidebar <- sidebar(
  selectInput("y_axis",
              "y-axis",
              c("LTC", "LAC"),
              "LTC"),
  numericInput("learning",
               "Learning Slope %",
               100,
               0,
               100,
               1,
               updateOn = "blur"),
  numericInput("rate",
               "Rate Slope %",
               100,
               0,
               100,
               1,
               updateOn = "blur"),
  numericInput("t1",
               "T1 Cost",
               1,
               0,
               NA,
               1,
               updateOn = "blur"),
  numericInput("n_lots",
               "Number of Lots",
               10,
               1,
               NA,
               1,
               updateOn = "blur"),
  numericInput("max_qty",
               "Maximum Quantity",
               100,
               1,
               NA,
               1,
               updateOn = "blur")
)

page_sidebar(
  layout_columns(
    plotOutput("curve_plot"),
    plotOutput("qty_plot",
               click = "qty_click"),
    col_widths = c(12, 12)
  ),
  sidebar = left_sidebar,
  title = "LRC Comparison"
)
