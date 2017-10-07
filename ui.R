library(shinycssloaders)

# Get vector of reporter country names.
reporters <- comtradr:::get_country_db() %>% 
  filter(reporter) %>% 
  .[["country_name"]] %>% 
  .[. != "All"]

# Get vector of partner country names.
partners <- comtradr:::get_country_db() %>% 
  filter(partner) %>% 
  .[["country_name"]] %>% 
  .[. != "All"]

# Get vector of commodities.
commodities <- comtradr:::get_commodity_db()$commodity

# Create UI.
shinyUI(fluidPage(
  
  # Application title.
  tags$div(
    tags$h1("UN Comtrade Data Viz"), 
    tags$h3("Tool for plotting inter-country shipping data from the United Nations Comtrade DB")
  ), 
  br(), 
  
  # Sidebar with a a number of fields for user input.
  sidebarLayout(
    sidebarPanel(
      HTML("Provide input for Reporting Country, Partner Countries, 
           Commodities, Trade Direction, and y-axis metric."),
      br(), 
      HTML("Click the 'Plot' button to update the plot."), 
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
                         choices = c("Imports" = "Imports", 
                                     "Exports" = "Exports", 
                                     "Re-Imports" = "Re-Imports", 
                                     "Re-Exports" = "Re-Exports", 
                                     "All" = "All"), 
                         selected = "Exports"), 
      
      radioButtons("value_vs_kg", 
                   label = paste("Plot 'value of shipments' or 'weight of shipments' along y-axis", 
                                 "(Many Comtrade datasets do NOT include data on shipment weight):", sep = "\n"), 
                   choices = c("Value in USD" = "value", "Weight in KG" = "weight"), 
                   selected = "value"), 
      
      # Plot will not update until user clicks the "Plot" button.
      actionButton("get_plot", "Plot"), 
      br(), 
      br(), 
      
      tags$ul(
        tags$li(HTML("Source code for this app can be found 
                     <a href='https://github.com/ChrisMuir/comtrade_plot_shinyapp'>here</a>. 
                     Please report bugs 
                     <a href='https://github.com/ChrisMuir/comtrade_plot_shinyapp/issues'>here</a>.")), 
        tags$li(HTML("Source for all shipping data is <a href='https://comtrade.un.org/data/'>UN Comtrade</a> 
                     (<a href='https://comtrade.un.org/data/doc/api/'>link</a> 
                     to the full documentation of the UN Comtrade API).")), 
        tags$li("This app is not affiliated with the United Nations in any way.")
      )
    ),
    
    # Show plot of data returned from the API call.
    mainPanel(
      plotlyOutput("resPlot", height = "768px") %>% withSpinner()
    )
  )
))