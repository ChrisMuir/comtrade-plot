library(shiny)
library(dplyr)
library(ggplot2)
library(comtradr)

no_data_func <- function(res, val_vs_kg = FALSE) {
  if (val_vs_kg) {
    msg_text <- paste0("Returned data does not contain variable 'Weight in KG'. ", 
                       "Please select 'Value in USD' and re-plot.")
  } else if (res$msg == "Ok" && is.null(res$details)) {
    msg_text <- paste0("The API query returned no data.\n\n", 
                       "The API connection was successful, but the search ", 
                       "resulted in no data.\nTry a different search")
  } else if (res$msg == "Could not complete connection to API") {
    msg_text <- paste0("The API query returned no data.\n\n", 
                       "Could not connect to the API. Either your internet ", 
                       "connection is bad, or the API is down.")
  } else {
    msg_text <- paste0("The API query returned no data.\n\n", 
                       "Here are the return details provided by the API:\n", 
                       paste(res$msg, res$details, sep = ", "))
  }
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, msg_text, cex = 1.6, col = "black")
}

ggplot_func <- function(res, val_vs_kg) {
  df <- res$data
  if (val_vs_kg == "weight") {
    if (typeof(df$`Netweight (kg)`) == "integer") {
      plotdf <- df %>% 
        group_by_(.dots = c("Partner", "Year")) %>% 
        summarise(value = as.numeric(sum(`Netweight (kg)`, na.rm = TRUE)))
      y_label <- "total weight of shipments in KG"
    } else {
      return(no_data_func(res, val_vs_kg = TRUE))
    }
  } else if (val_vs_kg == "value") {
    plotdf <- df %>% 
      group_by_(.dots = c("Partner", "Year")) %>% 
      summarise(value = as.numeric(sum(`Trade Value (US$)`, na.rm = TRUE)))
    y_label <- "total value of shipments in USD"
  }

  p <- ggplot(plotdf, aes(Year, value, color = factor(Partner))) + 
    geom_point(size = 4) + 
    geom_line(size = 1.5) + 
    scale_x_continuous(limits = c(min(plotdf$Year), max(plotdf$Year)), 
                       breaks = seq.int(min(plotdf$Year), max(plotdf$Year), 2)) + 
    labs(x = "year", y = y_label, color = "Partner\nCountries", 
         linetype = "Partner\nCountries") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1), 
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          legend.text = element_text(size = 14), 
          legend.title = element_text(size = 14))
  return(p)
}

commodity_code_lookup <- function(commod_desc, commoditydf) {
  output <- commoditydf[commoditydf$commodity == commod_desc, ]$code
  if (length(output) == 0) {
    output <- commod_desc
  }
  return(output)
}

shinyServer(function(input, output) {
  
  ship_data <- eventReactive(input$get_plot, {
    codes <- vapply(
      input$commod, function(x) commodity_code_lookup(x, commoditydf), 
      character(1), 
      USE.NAMES = FALSE
    )
    comtradr::ct_search(reporters = input$reporter, 
                        partners = input$partner, 
                        countrytable = countrydf, 
                        tradedirection = input$trade_direction, 
                        commodcodes = codes)
  }, ignoreNULL = FALSE)
  
  output$resPlot <- renderPlot({
    res <- ship_data()
    
    if (!is.null(res$data) && nrow(res$data) > 0) {
      p <- ggplot_func(res, input$value_vs_kg)
      print(p)
    } else {
      no_data_func(res)
    }
  })
  
})