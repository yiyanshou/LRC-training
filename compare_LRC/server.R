
# Define server logic
function(input, output, session) {
  # Set ordering of models
  model_enum <- c("Unit Theory", "CUMAV-Direct", "CUMAV-Iterative")
  
  # Set color palette and point shapes for curve plot
  palette <- c("red", "blue", "green")
  point_shapes <- c(0, 1, 2)
  
  # Initialize quantity stream and curve plot limits
  qty_stream <- reactiveVal(data.frame(Lot = numeric(0),
                                       Qty = numeric(0)))
  
  zoom_limits <- reactiveValues(ymin = NA_real_,
                                ymax = NA_real_,
                                xmin = NA_real_,
                                xmax = NA_real_)
  
  # Open info popover on load
  toggle_popover("help")
  
  # Round number of lots input to nearest integer
  n_lots <- reactive({round(input$n_lots)})
  
  # Add or remove lots from quantity stream
  observe({
    req(n_lots() > 0)
    old_stream <- qty_stream()
    n_new <- n_lots() - nrow(old_stream)
    if (n_new > 0) {
      new_rows <- data.frame(Lot = seq_len(n_new) + max(old_stream$Lot, 0),
                             Qty = rep(1, n_new))
      new_stream <- rbind(old_stream, new_rows)
      qty_stream(new_stream)
    } else {
      qty_stream(old_stream[seq_len(n_lots()), ])
    }
  }) |>
    bindEvent(n_lots())
  
  # Update max quantity
  observe({
    qty <- qty_stream()
    qty$Qty <- pmin(qty$Qty,
                    input$max_qty)
    qty_stream(qty)
  }) |>
    bindEvent(input$max_qty)
  
  # Update quantity plot
  qty_plot <- reactive({
    with(
      qty_stream(),
      {
        xmin <- if(is.na(zoom_limits$xmin)) {
          min(Lot)
        } else {
          zoom_limits$xmin
        }
        xmax <- if(is.na(zoom_limits$xmax)) {
          max(Lot)
        } else {
          zoom_limits$xmax
        }
        
        plot(x = Lot,
             y = Qty,
             ylim = c(0, input$max_qty),
             xlim = c(xmin, xmax),
             type = "b",
             xlab = NA)
        title(xlab = "Lot",
              line = 2.1)
      }
    )
  }) |>
    bindEvent(qty_stream(),
              input$max_qty,
              zoom_limits$ymin,
              zoom_limits$ymax,
              zoom_limits$xmin,
              zoom_limits$xmax)
  
  output$qty_plot <- renderPlot({
    validate(need(n_lots() >= 2,
                  "The number of lots must be at least 2."))
    
    par(mar = c(3.1, 4.1, 0.1, 0),
        cex = 1.3,
        las = 1)
    qty_plot()
  })
  
  # Convert learning and rate slopes to parameters
  learning <- reactive({log(input$learning_slope/100, 2)}) |>
    bindEvent(input$learning_slope)
  rate <- reactive({log(input$rate_slope/100, 2)}) |>
    bindEvent(input$rate_slope)
  
  # Convert quantity stream to unit sequencing
  unit_seq <- reactive({
    cbind(qty_stream(),
          stream_to_seq(qty_stream()$Qty))
  }) |>
    bindEvent(qty_stream())
  
  # Update LRC plot
  curve_plot <- reactive({
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
    
    ymin <- if(is.na(zoom_limits$ymin)) {
      min(ut$Cost, cad$Cost, cai$Cost)
    } else {
      zoom_limits$ymin
    }
    ymax <- if(is.na(zoom_limits$ymax)) {
      max(ut$Cost, cad$Cost, cai$Cost)
    } else {
      zoom_limits$ymax
    }
    xmin <- if(is.na(zoom_limits$xmin)) {
      min(ut$Lot, cad$Lot, cai$Lot)
    } else {
      zoom_limits$xmin
    }
    xmax <- if(is.na(zoom_limits$xmax)) {
      max(ut$Lot, cad$Lot, cai$Lot)
    } else {
      zoom_limits$xmax
    }
    
    line_height_inches <- par("cin")[2] * par("cex") * par("lheight")
    plot_height_inches <- par("pin")[2]
    inset_fraction <- (-3 * line_height_inches) / plot_height_inches
    
    plot(x = ut$Lot,
         y = ut$Cost,
         col = palette[ut$Model],
         pch = point_shapes[ut$Model],
         type = "b",
         ylim = c(ymin, ymax),
         xlim = c(xmin, xmax),
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
           inset = inset_fraction,
           xpd = NA)
  }) |>
    bindEvent(learning(),
              rate(),
              input$t1,
              input$y_axis,
              unit_seq(),
              zoom_limits$ymin,
              zoom_limits$ymax,
              zoom_limits$xmin,
              zoom_limits$xmax)
  
  output$curve_plot <- renderPlot({
    validate(need(n_lots() >= 2,
                  "The number of lots must be at least 2."))
    validate(need(input$learning_slope > 0,
                  "The learning slope must be positive."))
    validate(need(input$rate_slope > 0,
                  "The rate slope must be positive."))
    validate(need(input$t1 >= 0,
                  "T1 must be nonnegative."))
    
    par(mar = c(0.5, 4.1, 2.7, 0),
        cex = 1.3,
        las = 1)
    curve_plot()
  })
  
  # Interactive plotting quantity stream adjustment
  observe({
    req(input$qty_click)
    click_x <- round(input$qty_click$x)
    click_y <- round(input$qty_click$y)
    req(1 <= click_x,
        click_x <= n_lots(),
        1 <= click_y)
    
    new_stream <- qty_stream()
    new_stream[[click_x, "Qty"]] <- click_y
    qty_stream(new_stream)
  }) |>
    bindEvent(input$qty_click)
  
  # Interactive curve plot zoom
  observe({
    req(input$curve_brush)
    zoom_limits$ymin <- input$curve_brush$ymin
    zoom_limits$ymax <- input$curve_brush$ymax
    zoom_limits$xmin <- input$curve_brush$xmin
    zoom_limits$xmax <- input$curve_brush$xmax
  }) |>
    bindEvent(input$zoom_in)
  
  observe({
    zoom_limits$ymin <- NA_real_
    zoom_limits$ymax <- NA_real_
    zoom_limits$xmin <- NA_real_
    zoom_limits$xmax <- NA_real_
  }) |>
    bindEvent(input$zoom_out,
              input$reset,
              input$y_axis)
  
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
    
    data.frame(Lot = seq_len(n_lots()),
               Qty = rep(1, n_lots())) |>
      qty_stream()
  }) |>
    bindEvent(input$reset)
  
}
