library(tidyverse)
install.packages("plotly")
library(plotly)
install.packages("leaflet")
library(leaflet)
library(ggplot2)


countries_info = subset(lfs_sexage_raw, Age == "15 to 24" & Series == "Unemployment rate" & Time == "2022" & Sex == "All persons")

countries_info <- countries_info[order(countries_info$Value), ]

row.names(countries_info) = NULL

color_scale <- colorRamp(c("red", "orange","yellow"))


fig <- plot_ly(countries_info,
               type = "scattergeo",
               locationmode = "country names",
               locations = ~Country,
               color = ~Value,
               text = ~paste("Country: ", Country, "<br>UR: ", Value),
               colors = color_scale,
               marker = list(size = 10)) %>%
  layout(title = "Unemployment rate in 2022",
         geo = list(
           showframe = FALSE,
           projection = list(type = 'mercator'),
           bgcolor = "lightblue",  
           showcoastlines = TRUE,  
           showland = TRUE,        
           landcolor = "lightgreen",  
           showocean = TRUE,       
           oceancolor = "lightblue"  
         ))

fig


cc <- c("green","blue", "red")

graph <- ggplot(data1994_2004, aes(x = Year, y = Value, fill = Sex, group=1,
                                  text = paste("Sex:", Sex, "<br>Year:", Year, "<br>UR:", Value))) +
  geom_line(color="black") +
  geom_point(shape = 21, size = 2.8) + 
  labs(x = "Year", y = "Unemployment Rate", title = "Unemployment Rate in Japan") +
  facet_wrap(~ Sex) +
  scale_fill_manual(values = cc) +
  labs(fill = "Population") +
  theme_grey() 

ggplotly(graph, tooltip = "text")















