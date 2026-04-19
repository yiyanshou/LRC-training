
# Define server logic
function(input, output, session) {
  # Initialize quantity stream
  qty_stream <- reactiveVal(tibble(Lot = numeric(0),
                                   Qty = numeric(0)))
  
  # Add or remove lots from quantity stream
  observe({
    old_stream <- qty_stream()
    n_new <- input$n_lots - nrow(old_stream)
    if (n_new > 0) {
      new_rows <- tibble(Lot = seq_len(n_new) + max(old_stream$Lot, 0),
                         Qty = rep(1, n_new))
      new_stream <- rbind(old_stream, new_rows)
      qty_stream(new_stream)
    } else {
      qty_stream(head(old_stream, input$n_lots))
    }
  }) %>%
    bindEvent(input$n_lots)
  
  # Update quantity plot
  output$qty_plot <- renderPlot({
      ggplot(qty_stream(),
             aes(x = Lot, y = Qty)) +
      geom_point() +
      ylim(0, input$max_qty)
  }) %>%
    bindEvent(qty_stream(),
              input$max_qty)
  
  # Convert learning and rate slopes to parameters
  learning <- reactive({log(input$learning/100, 2)}) %>%
    bindEvent(input$learning)
  rate <- reactive({log(input$rate/100, 2)}) %>%
    bindEvent(input$rate)
  
  # Convert quantity stream to unit sequencing
  unit_seq <- reactive({
    qty_stream() %>%
      mutate(stream_to_seq(Qty))
  }) %>%
    bindEvent(qty_stream())
  
  # Update LRC plot
  output$curve_plot <- renderPlot({
    lrc_data <- unit_seq() %>%
      mutate(UT = ut_ltc(First,
                         Last,
                         input$t1,
                         learning(),
                         rate()),
             CAD = cad_ltc(First,
                           Last,
                           input$t1,
                           learning(),
                           rate()),
             CAI = cai_ltc(First,
                           Last,
                           input$t1,
                           learning(),
                           rate())) %>%
      tidyr::pivot_longer(c(UT, CAD, CAI),
                          names_to = "Model",
                          values_to = "LTC")
    
    ggplot(lrc_data,
           aes(x = Lot, y = LTC, color = Model, shape = Model)) +
      geom_point() +
      geom_line()
  }) %>%
    bindEvent(learning(),
              rate(),
              input$t1,
              unit_seq())
  
  last_click <- reactiveVal()
  
  observe({
    req(input$qty_click)
    last_click(input$qty_click)
  })
  
  output$click_pos <- renderPrint({
    last_click()
  })
}
