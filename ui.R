library(shiny)

## TODO: Add link for bug reports (after I post source code on GitHub.)

# Create vectors of country names and commodities from the data frames downloaded 
# from the UN Comtrade website (data frames are created in the global.R file).
reporters <- countrydf[countrydf$type == "reporter", ]$`country name`
partners <- countrydf[countrydf$type == "partner", ]$`country name`
commodities <- commoditydf$commodity

shinyUI(fluidPage(
  
  # Application title
  tags$div(
    tags$h1("UN Comtrade Data Viz"), 
    tags$h2("Tool for plotting inter-country shipping data from the United Nations Comtrade DB")
  ), 
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      HTML("Provide input for Reporting Country, Partner Countries, 
           Commodities, Trade Direction, and y-axis metric"),
      br(), 
      br(), 
      selectInput("reporter", 
                  label = "Reporting Country:",
                  selected = "China", 
                  choices = reporters,
                  multiple = TRUE),
      selectInput("partner", 
                  label = "Partner Countries:", 
                  selected = c("USA", "Mexico", "Rep. of Korea"), 
                  choices = partners, 
                  multiple = TRUE), 
      selectInput("commod", 
                  label = "HS Code / Commodities:", 
                  selected = "TOTAL - Total of all HS commodities", 
                  choices = commodities, 
                  multiple = TRUE), 
      checkboxGroupInput("trade_direction", 
                   label = "Trade Direction for Reporting Country:", 
                   choices = c("Imports" = "imports", "Exports" = "exports", "Re-Imports" = "re-imports", "Re-Exports" = "re-exports", "All" = "all"), 
                   selected = "exports"), 
      radioButtons("value_vs_kg", 
                         label = paste("Plot 'value of shipments' or 'weight of shipments' along y-axis", 
                                       "(Many Comtrade datasets do NOT include data on shipment weight):", sep = "\n"), 
                         choices = c("Value in USD" = "value", "Weight in KG" = "weight"), 
                         selected = "value"), 
      actionButton("get_plot", "Plot"), 
      br(), 
      br(), 
      tags$ul(
        tags$li(HTML("Created using packages <a href='https://github.com/ChrisMuir/comtradr'>comtradr</a> 
                     and <a href='https://github.com/tidyverse/ggplot2'>ggplot2</a>.")), 
        tags$li(HTML("Source for all shipping data is <a href='https://comtrade.un.org/data/'>UN Comtrade</a> 
                     (<a href='https://comtrade.un.org/data/doc/api/'>link</a> 
                     to the full documentation of the UN Comtrade API).")), 
        tags$li("This app is not affiliated with the United Nations in any way.")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #p(class="text-small", "Plot inter-country shipping data"), 
      plotOutput("resPlot", height = "800px")
    )
  )
))