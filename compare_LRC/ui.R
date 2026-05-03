
library(shiny)
library(bslib)

theme <- bs_theme(preset = "shiny") |>
  
  bs_add_rules(".wide-popover {max-width: 85%;}") |>
  
  bs_add_rules(".zoom-box {display: flex;
               flex-direction: column;
               justify-content: center;
               align-items: center;
               height: 100%;
               width: 25px;
               margin-left: 20px}") |>
  
  bs_add_rules(".zoom-icon {font-size: 25px}") |>
  
  bs_add_rules(".zoom-btn {flex: none;
               height: 25px;
               width: 25px;
               margin: 10px 0px 10px 0px}") |>
  
  bs_add_rules(".plot-box {display: flex;
               height: 50%;
               width: 100%}") |>
  
  bs_add_rules(".main {display: flex;
               flex-direction: column;
               height: 100%;
               width: 100%}")

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
               "Reset"),
  verbatimTextOutput("debug")
)

help_btn <- popover(icon("circle-info"),
                    includeHTML("LRC_comparison_info.html"),
                    id = "help",
                    options = list(customClass = "wide-popover"))

zoom_in_btn <- actionLink("zoom_in",
                          "",
                          icon = icon("magnifying-glass-plus",
                                      class = "zoom-icon"),
                          class = "zoom-btn")

zoom_out_btn <- actionLink("zoom_out",
                           "",
                           icon = icon("magnifying-glass-minus",
                                       class = "zoom-icon"),
                           class = "zoom-btn")

zoom_ctrls <- tags$div(zoom_in_btn,
                       zoom_out_btn,
                       class = "zoom-box")

top_panel <- tags$div(plotOutput("curve_plot",
                                 brush = brushOpts("curve_brush",
                                                   resetOnNew = T),
                                 height = "100%"),
                      zoom_ctrls,
                      class = "plot-box")

bottom_panel <- tags$div(plotOutput("qty_plot",
                                    click = "qty_click",
                                    height = "100%"),
                         tags$div(class = "zoom-box"),
                         class = "plot-box")

main_content <- tags$div(top_panel,
                         bottom_panel,
                         class = "main")

page_sidebar(
  main_content,
  theme = theme,
  sidebar = left_sidebar,
  title = tags$span("LRC Comparison",
                    HTML("&nbsp &nbsp"),
                    help_btn),
  window_title = "LRC Comparison"
)
