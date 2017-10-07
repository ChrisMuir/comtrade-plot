library(ggplot2)
library(scales)

no_data_func <- function(msg, val_vs_kg = FALSE) {
  # Function for building an "error message" plot, to be displayed when there 
  # is no data to plot.
  # msg: str, either error message on why the API query failed, or message 
  #   indicating the query was successful but simply returned no data.
  # val_vs_kg: logical.
  # Output is a ggplot object that displays a text error message.
  if (val_vs_kg) {
    msg_text <- paste0("Returned data does not contain variable 'Weight in KG'.\n", 
                       "Please select 'Value in USD'.")
  } else if (msg == "no_data") {
    msg_text <- paste0("The API query returned no data.\n\n", 
                       "The API connection was successful, but the search ", 
                       "resulted in no data.\nTry a different search")
  } else {
    msg_text <- paste0("The API query returned no data.\n\n", 
                       "Error message from the API query:\n", 
                       msg)
  }
  
  p <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100) + 
    annotate(geom = "text", x = 5, y = 50, label = msg_text, color = "red")
  return(p)
}

get_plot_title <- function(reporters, partners, trade_dir) {
  # Function for automated creation of a ggplot title.
  # reporters: char vector, reporter countries supplied by the user.
  # partners: char vector, partner countries supplied by the user.
  # trade_dir: char vector, all trade directions supplied by the user.
  # Output is a char string that will be used as the title of the output plot.
  
  # Get reporter country text.
  if ("All" %in% reporters) {
    reporters <- "All Countries"
  } else {
    reporters <- paste(reporters, collapse = ", ")
  }
  # Get partner country text.
  if ("All" %in% partners) {
    partners <- "All Countries"
  } else if ("World" %in% partners) {
    partners <- "the World"
  } else {
    partners <- paste(partners, collapse = ", ")
  }
  # Get trade direction text and filler text.
  if (all(!"Exports" %in% trade_dir, 
          !"Re-Exports" %in% trade_dir, 
          !"All" %in% trade_dir)) {
    trade_dir <- paste(trade_dir, collapse = ", ")
    filler_1 <- " into "
    filler_2 <- " from "
  } else if (all(!"Imports" %in% trade_dir, 
                 !"Re-Imports" %in% trade_dir, 
                 !"All" %in% trade_dir)) {
    trade_dir <- paste(trade_dir, collapse = ", ")
    filler_1 <- " from "
    filler_2 <- " into "
  } else {
    trade_dir <- "All Trade"
    filler_1 <- " between "
    filler_2 <- " and "
  }
  return(paste0(trade_dir, filler_1, reporters, filler_2, partners))
}

ggplot_func <- function(df, val_vs_kg, reporters, partners, trade_dir) {
  # Takes various inputs and creates a ggplotly object.
  # df: data frame, returned object from a call to the Comtrade API.
  # val_vs_kg: char vector, either "value" or "weight".
  # reporters: char vector, reporter countries supplied by the user.
  # partners: char vector, partner countries supplied by the user.
  # trade_dir: char vector, all trade directions supplied by the user.
  # Output is ggplotly object that will be displayed in the main panel of the 
  # app.
  
  # Generate the plot title.
  plot_title <- get_plot_title(reporters, partners, trade_dir)
  
  # Transformations based on which variable is to be mapped to the y-axis, 
  # which will be either "Weight in KG" or "Value in USD" and is determined by 
  # the value of input val_vs_kg.
  if (val_vs_kg == "weight") {
    if (typeof(df$`Net Weight kg`) == "integer") {
      # Create plot data frame.
      plotdf <- df %>% 
        group_by_(.dots = c("`Partner Country`", "Year")) %>% 
        summarise(value = as.numeric(sum(`Net Weight kg`, na.rm = TRUE)))
      y_label <- "total weight of shipments in KG"
      # Create hover variable.
      plotdf$hover <- NA
      for (i in seq_len(nrow(plotdf))) {
        lab <- paste0("Year: ", plotdf$Year[i], "<br>", 
                      "Value: ", scales::comma(plotdf$value[i]), "<br>", 
                      "Partner: ", plotdf$`Partner Country`[i])
        plotdf$hover[i] <- lab
      }
    } else {
      p <- no_data_func("weight", val_vs_kg = TRUE)
      return(ggplotly(p))
    }
  } else if (val_vs_kg == "value") {
    # Create plot data frame.
    plotdf <- df %>% 
      group_by_(.dots = c("`Partner Country`", "Year")) %>% 
      summarise(value = as.numeric(sum(`Trade Value usd`, na.rm = TRUE)))
    y_label <- "total value of shipments in USD"
    # Create hover variable.
    plotdf$hover <- NA
    for (i in seq_len(nrow(plotdf))) {
      lab <- paste0("Year: ", plotdf$Year[i], "<br>", 
                    "Value: ", scales::dollar(plotdf$value[i]), "<br>", 
                    "Partner: ", plotdf$`Partner Country`[i])
      plotdf$hover[i] <- lab
    }
  }
  
  # Generate ggplot object, which will be wrapped in plotly::ggplotly().
  p <- ggplot(plotdf, aes(Year, value, color = factor(`Partner Country`))) + 
    geom_point(size = 2.5, aes(text = hover)) + 
    geom_line(size = 1) + 
    scale_x_continuous(limits = c(min(plotdf$Year), max(plotdf$Year)), 
                       breaks = seq.int(min(plotdf$Year), max(plotdf$Year), 2)) + 
    labs(title = plot_title, y = y_label, 
         color = "Partner\nCountries", linetype = "Partner\nCountries") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1), 
          axis.title = element_text(size = 10), 
          axis.text = element_text(size = 8), 
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8), 
          title = element_text(size = 12))
  return(ggplotly(p, tooltip = "text"))
}

shinyServer(function(input, output) {
  ship_data <- eventReactive(input$get_plot, {
    # Treat input$commod then execute API call (only updates when user clicks 
    # the "Plot" button).
    
    # Isolate the commodity code from each input commodity.
    codes <- comtradr::ct_commodity_lookup(input$commod, 
                                           return_code = TRUE, 
                                           return_char = TRUE, 
                                           verbose = FALSE)
    # API call.
    comtradr::ct_search(reporters = input$reporter, 
                        partners = input$partner, 
                        trade_direction = tolower(input$trade_direction), 
                        commod_codes = codes)
  }, ignoreNULL = FALSE)
  
  user_input <- eventReactive(input$get_plot, {
    # Record the inputs supplied by the user (only updates when user clicks 
    # the "Plot" button).
    list(
      reporters = input$reporter, 
      partners = input$partner, 
      trade_dir = input$trade_direction
    )
  }, ignoreNULL = FALSE)
  
  output$resPlot <- renderPlotly({
    # Plot the output.
    res <- tryCatch(ship_data(), error = function(e) e)
    input_vals <- user_input()
    
    if (methods::is(res, "error")) {
      no_data_func(res$message)
    } else if (nrow(res) == 0) {
      no_data_func("no_data")
    } else {
      res <- comtradr::ct_use_pretty_cols(res)
      p <- ggplot_func(res, input$value_vs_kg, input_vals$reporters, 
                       input_vals$partners, input_vals$trade_dir)
    }
  })
  
})