
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
  gg_qty <- reactive({
    dummy_data <- tibble(Lot = 1,
                         Qty = 0,
                         Model = c("CAD", "CAI", "UT"))
    
    ggplot(dummy_data,
           aes(x = Lot, y = Qty, color = Model)) +
      geom_point() +
      geom_point(data = qty_stream(),
                 color = "black") +
      ylim(0, input$max_qty) +
      scale_color_manual(values = rep("transparent", 3)) +
      theme(legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(color = "transparent",
                                      fill = "transparent"))
  }) %>%
    bindEvent(qty_stream(),
              input$max_qty)
  
  output$qty_plot <- renderPlot({gg_qty()})
  
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
  gg_curve <- reactive({
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
      geom_line() +
      labs(x = NULL)
  }) %>%
    bindEvent(learning(),
              rate(),
              input$t1,
              unit_seq())
  
  output$curve_plot <- renderPlot({gg_curve()})
  
  # Interactive plotting quantity stream adjustment
  observe({
    req(input$qty_click)
    click_x <- round(input$qty_click$x)
    click_y <- round(input$qty_click$y)
    req(1 <= click_x,
        click_x <= input$n_lots,
        0 <= click_y)
    
    new_stream <- qty_stream()
    new_stream[[click_x, "Qty"]] <- click_y
    qty_stream(new_stream)
  }) %>%
    bindEvent(input$qty_click)
  
}
