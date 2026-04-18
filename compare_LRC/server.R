
# Define server logic
function(input, output, session) {
  output$curve_plot <- renderPlot({
    learning <- log(input$learning/100, 2)
    rate <- log(input$rate/100, 2)
    t1 <- input$t1
    
    qty <- c(10, 20, 30, 30, 10)
    seq <- stream_to_seq(qty)
    
    lrc_data <- seq %>%
      mutate(UT = ut_ltc(First,
                         Last,
                         t1,
                         learning,
                         rate),
             CAD = cad_ltc(First,
                           Last,
                           t1,
                           learning,
                           rate),
             CAI = cai_ltc(First,
                           Last,
                           t1,
                           learning,
                           rate),
             Lot = row_number()) %>%
      tidyr::pivot_longer(c(UT, CAD, CAI),
                          names_to = "Model",
                          values_to = "LTC")
    
    ggplot(lrc_data,
           aes(x = Lot, y = LTC, color = Model)) +
      geom_point() +
      geom_line()
  })
}
