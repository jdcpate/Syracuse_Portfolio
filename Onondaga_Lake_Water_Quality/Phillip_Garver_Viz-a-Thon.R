####  Author: Phillip A. Garver
####  Team Members: Justin Pate, Phillip A. Garver
##
####  Viz-a-Thon:: IST719 Fall 2019
##
##    Data Source: Data from Onondaga Lake Water Quality Monitoring Buoy
##    NOAA National Climate Data Center (https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND)
##    2016-2018 data from Station USW00014771 (SYRACUSE HANCOCK INTERNATIONAL AIRPORT, NY US)
##    Data from Syracuse Airport Meterological Station



install.packages(c("alluvial", "animation", "ape", "aplpack", "circular", "devtools", "easypackages","extrafont", "flexdashboard", "ggalluvial", "ggalt", "ggmap", "ggplot2", "ggthemes", "hexbin", "highcharter", "igraph", "jsonlite", "lattice", "leaflet", "lubridate", "mapproj", "maps", "plotrix", "png", "pracma", "raster", "RColorBrewer", "RCurl", "readxl", "reshape", "reshape2", "rgdal", "riverplot", "rnaturalearth", "rworldmap", "scales", "scatterplot3d", "shiny", "showtext", "SnowballC", "stringr", "sunburstR", "sysfonts", "TeachingDemos", "Ternary", "tidyr", "tidytext", "tm", "treemap", "vcd", "vioplot", "waffle", "wordcloud", "wordcloud2", "XML"))


devtools::install_github("ropensci/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")

library(readxl)

data_info <- read_excel("BuoyData.xlsx")

my_data <- read_excel("BuoyData.xlsx", sheet = "2016-2018 data")

View(data_info)

View(my_data)

str(my_data)

##  DATE_TIME is already in POSIXct format, so shouldn't need changed

min(my_data$DATE_TIME)
max(my_data$DATE_TIME)

my.data <- my_data[, 1:8]

colnames(my.data) <- c("Date", "Depth", "Temp", "Saltiness"
                       , "pH", "Oxygen", "Cloudiness", "Algae")



plot(my.data$Date, my.data$Temp, las = 1.5)

plot(my.data$Date, my.data$Saltiness, las = 1.5)

plot(my.data$Date, my.data$pH, las = 1.5)

plot(my.data$Date, my.data$Oxygen, las = 1.5)

plot(my.data$Date, my.data$Cloudiness, las = 1.5)

plot(my.data$Date, my.data$Algae, las = 1.5)



####  Remove any NA data 

algae <- na.omit(my.data)

View(algae)


####  Aggregate the data by Date - this clusters by year as there are dates where no readings are taken

my.algae <- aggregate(algae$Algae, list(algae$Date), FUN = sum)


####  Set Column Names for the Aggregate Data

colnames(my.algae) <- c("Date", "Algae")


####  Set the Colors by Value

library(RColorBrewer)


col.vec <- rep(rgb(102,204,0, maxColorValue = 255, alpha = 100), nrow(my.algae))

col.vec[my.algae$Algae < 44.31] <- rgb(102,102,255, maxColorValue = 255, alpha = 75)

col.vec[my.algae$Algae > 130.91] <- rgb(102,0,51, maxColorValue = 255, alpha =150)

par(mar=c(5,4,2,1))
plot(my.algae, pch = 18
     , las = 1.5, ylab = "Chlorophyll-a (micrograms per liter)"
     , col = col.vec, main = "Measure of Algae Content"
     , sub = "by Aggregated Years: 2016-2018"
     , cex = 1.2, cex.lab = .85, cex.axis = .75, cex.main = 1.2)


boxplot(log10(algae[,2:8]), las = 1.5, cex.axis = .65
        , col = terrain.colors(8), main = "Areas of Measurement"
        , cex.main = 1)


boxplot(log10(algae[,5:8]), las = 1.5, cex.axis = .75
        , col = terrain.colors(6))

