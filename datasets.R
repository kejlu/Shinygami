library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(devtools)

# dollars in millions
countries_rev <- read.csv("apple_revenue.csv", header = FALSE)
colnames(countries_rev) <- c('Year', 'Quarter', 'Americas', 'Europe', 'Asia', 
'Retail', 'Total', 'Q_Y', 'Y_Q')

countries_rev2 <- read.csv("apple2_revenue.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(countries_rev2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Region', 'Rev')


# units in thousands
prod_units <- read.csv("apple_prod_units.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(prod_units) <- c('Year', 'Quarter', 'iPhone', 'iPad', 'Mac', 'iPod', 'Q_Y', 'Y_Q')

prod_units2 <- read.csv("apple2_prod_units.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(prod_units2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Product', 'Units')

# dollars in millions
prod_rev <- read.csv("apple_prod_revenue.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(prod_rev) <- c('Year', 'Quarter', 'iPhone', 'iPad', 'Mac', 'iPod', 'Q_Y', 'Y_Q')

prod_rev2 <- read.csv("apple2_prod_revenue.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(prod_rev2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Product', 'Rev')