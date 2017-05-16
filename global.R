library(comtradr)

# Download the countries table and commodities table. Server.R will use both of 
# these data frames, ui.R will use a number of vectors taken from these data frames.
countrydf <- comtradr::ct_countries_table()
commoditydf <- comtradr::ct_commodities_table("HS")

# Create vectors that will be used by ui.R.
reporters <- countrydf[countrydf$type == "reporter", ]$`country name`
partners <- countrydf[countrydf$type == "partner", ]$`country name`