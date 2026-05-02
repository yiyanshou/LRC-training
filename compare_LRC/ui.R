
library(shiny)
library(bslib)

theme <- bs_theme(preset = "shiny") |>
  bs_add_rules(".wide-popover {max-width: 75%;}")

left_sidebar <- sidebar(
  selectInput("y_axis",
              "y-axis",
              c("LTC", "LAC"),
              "LTC"),
  numericInput("learning_slope",
               "Learning Slope %",
               100,
               0,
               100,
               1,
               updateOn = "blur"),
  numericInput("rate_slope",
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
               10,
               1,
               NA,
               1,
               updateOn = "blur"),
  actionButton("reset",
               "Reset")
)

help_btn <- popover(icon("circle-info"),
                    includeHTML("LRC_comparison_info.html"),
                    id = "help",
                    options = list(customClass = "wide-popover"))

page_sidebar(
  plotOutput("curve_plot"),
  plotOutput("qty_plot",
             click = "qty_click"),
  theme = theme,
  sidebar = left_sidebar,
  title = tags$span("LRC Comparison",
                    HTML("&nbsp &nbsp"),
                    help_btn),
  window_title = "LRC Comparison"
)
