
# Define server logic
function(input, output, session) {
  # Set ordering of models
  model_enum <- c("Unit Theory", "CUMAV-Direct", "CUMAV-Iterative")
  
  # Set color palette and point shapes for curve plot
  palette <- c("red", "blue", "green")
  point_shapes <- c(0, 1, 2)
  
  # Initialize quantity stream
  qty_stream <- reactiveVal(data.frame(Lot = numeric(0),
                                       Qty = numeric(0)))
  
  # Add or remove lots from quantity stream
  observe({
    req(input$n_lots >= 0)
    old_stream <- qty_stream()
    n_new <- input$n_lots - nrow(old_stream)
    if (n_new > 0) {
      new_rows <- data.frame(Lot = seq_len(n_new) + max(old_stream$Lot, 0),
                             Qty = rep(1, n_new))
      new_stream <- rbind(old_stream, new_rows)
      qty_stream(new_stream)
    } else {
      qty_stream(old_stream[seq_len(input$n_lots), ])
    }
  }) |>
    bindEvent(input$n_lots)
  
  # Update quantity plot
  gg_qty <- reactive({
    dummy_data <- data.frame(Lot = 1,
                             Qty = 0,
                             Model = c("CAD", "CAI", "UT"))
    
    with(
      qty_stream(),
      {
        plot(x = Lot,
             y = Qty,
             ylim = c(0, input$max_qty),
             type = "b",
             xlab = NA)
        title(xlab = "Lot",
              line = 2.1)
      }
    )
  }) |>
    bindEvent(qty_stream(),
              input$max_qty)
  
  output$qty_plot <- renderPlot({
    validate(need(input$n_lots > 0,
                  "The number of lots must be at least 1."))
    par(mar = c(3.1, 4.1, 0.1, 2.1),
        xpd = NA,
        cex = 1.3,
        las = 1)
    gg_qty()
  })
  
  # Convert learning and rate slopes to parameters
  learning <- reactive({log(input$learning/100, 2)}) |>
    bindEvent(input$learning)
  rate <- reactive({log(input$rate/100, 2)}) |>
    bindEvent(input$rate)
  
  # Convert quantity stream to unit sequencing
  unit_seq <- reactive({
    cbind(qty_stream(),
          stream_to_seq(qty_stream()$Qty))
  }) |>
    bindEvent(qty_stream())
  
  # Update LRC plot
  gg_curve <- reactive({
    lac <- input$y_axis == "LAC"
    
    ut <- NULL
    cad <- NULL
    cai <- NULL
    
    with(
      unit_seq(),
      {
        ut <<- data.frame(
          Lot = Lot,
          Model = factor("Unit Theory", model_enum),
          Cost = ut_ltc(First,
                        Last,
                        input$t1,
                        learning(),
                        rate(),
                        lac)
        )
        
        cad <<- data.frame(
          Lot = Lot,
          Model = factor("CUMAV-Direct", model_enum),
          Cost = cad_ltc(First,
                         Last,
                         input$t1,
                         learning(),
                         rate(),
                         lac)
        )
        
        cai <<- data.frame(
          Lot = Lot,
          Model = factor("CUMAV-Iterative", model_enum),
          Cost = cai_ltc(First,
                         Last,
                         input$t1,
                         learning(),
                         rate(),
                         lac)
        )
      })
    
    ymin <- min(ut$Cost, cad$Cost, cai$Cost)
    ymax <- max(ut$Cost, cad$Cost, cai$Cost)
    
    plot(x = ut$Lot,
         y = ut$Cost,
         col = palette[ut$Model],
         pch = point_shapes[ut$Model],
         type = "b",
         ylim = c(ymin, ymax),
         ylab = input$y_axis,
         xlab = NA)
    lines(x = cad$Lot,
          y = cad$Cost,
          col = palette[cad$Model],
          pch = point_shapes[cad$Model],
          type = "b")
    lines(x = cai$Lot,
          y = cai$Cost,
          col = palette[cai$Model],
          pch = point_shapes[cai$Model],
          type = "b")
    legend("top",
           legend = paste0(model_enum, "    "),
           col = palette,
           pch = point_shapes,
           lty = 1,
           horiz = T,
           bty = "n",
           text.width = NA,
           inset = -.2)
  }) |>
    bindEvent(learning(),
              rate(),
              input$t1,
              input$y_axis,
              unit_seq())
  
  output$curve_plot <- renderPlot({
    validate(need(input$n_lots > 1,
                  "The number of lots must be at least 2."))
    validate(need(input$learning > 0,
                  "The learning slope must be nonnegative."))
    validate(need(input$rate > 0,
                  "The rate slope must be nonnegative."))
    
    par(mar = c(0.5, 4.1, 2.7, 2.1),
        cex = 1.3,
        xpd = NA,
        las = 1)
    gg_curve()
  })
  
  # Interactive plotting quantity stream adjustment
  observe({
    req(input$qty_click)
    click_x <- round(input$qty_click$x)
    click_y <- round(input$qty_click$y)
    req(1 <= click_x,
        click_x <= input$n_lots,
        1 <= click_y)
    
    new_stream <- qty_stream()
    new_stream[[click_x, "Qty"]] <- click_y
    qty_stream(new_stream)
  }) |>
    bindEvent(input$qty_click)
  
  # Reset button
  observe({
    updateNumericInput(inputId = "learning",
                       value = 100)
    updateNumericInput(inputId = "rate",
                       value = 100)
    updateNumericInput(inputId = "t1",
                       value = 1)
    updateNumericInput(inputId = "n_lots",
                       value = 10)
    updateNumericInput(inputId = "max_qty",
                       value = 10)
    
    data.frame(Lot = seq_len(input$n_lots),
               Qty = rep(1, input$n_lots)) |>
      qty_stream()
  }) |>
    bindEvent(input$reset)
  
}
