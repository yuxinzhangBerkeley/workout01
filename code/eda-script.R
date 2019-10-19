library(ggplot2)
library(dplyr)
library(maps)
library(readr)
library(lubridate)
setwd("/Users/yuxinzhang/Downloads/STAT133/workout01/")


#4) Data Importing in R

df =read_csv("./data/ibtracs-2010-2015.csv",
             col_names = TRUE, 
             col_types = cols(), 
             na = c("-999.","-1.0","0.0"))#Specify strings of missing values with "-999.", "-1.0", and "0.0".
df$Basin = as.factor(df$Basin)
df$ISO_time = as.character(df$ISO_time)

str(df)
sink(file = './output/data-summary.txt')
summary(df)
sink()

#5) Data Visualization
## 1) A sinlge map with the trajectories of all the storms (all years 2010-2015)
# saving plot in pdf format

pdf("./images/map-all-storms.pdf")
map('world', interior = FALSE, fill = TRUE, col = 'lightblue')
map('world', interior = TRUE, fill= FALSE, col = 'white', add = TRUE) 
points(df[,c("Longitude","Latitude")], col = 'blue',cex = 0.1, pch = 18)
abline(h = 0, col = "red")
title(main = "Storms Trajectories Visualization")
dev.off()


png("./images/map-all-storms.png")
map('world', interior = FALSE, fill = TRUE, col = 'lightblue')
map('world', interior = TRUE, fill= FALSE, col = 'white', add = TRUE) 
points(df[,c("Longitude","Latitude")], col = 'blue',cex = 0.1, pch = 18)
abline(h = 0, col = "red")
title(main = "Storms Trajectories Visualization")
dev.off()



#2) Graph to visualize the storms in the basins EP (Eastern Pacific) and NA (North Atlantic),
#facetted by year. 
#Save this plot both in PDF and PNG formats as map-ep-na-storms-by-year.pdf and map-ep-na-storms-by-year.png inside the images/ folder.

df_ep_na=df[df$Basin == c('EP','NA'), ]
worldmap <- map_data("world")

map_ep_na_by_year=ggplot() + 
  geom_map(data = worldmap, map = worldmap,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f") + 
  geom_point(data = df_ep_na, 
             aes(x = Longitude, y = Latitude, color = Basin),
             cex = 0.2, pch = 10) +
  facet_wrap(~ Season, ncol=3)
map_ep_na_by_year

ggsave("./images/map-ep-na-storms-by-year.pdf")
ggsave("./images/map-ep-na-storms-by-year.png")

## 3)Graph to visualize the storms in the basins EP (Eastern Pacific) and NA (North Atlantic), 
#facetted by month. 
#Save this plot both in PDF and PNG formats as map-ep-na-storms-by-month.pdf and map-ep-na-storms-by-month.png inside the images/ folder.

df_ep_na['Month'] = month(df_ep_na$ISO_time, label = TRUE)

map_ep_na_by_month=ggplot() + 
  geom_map(data = worldmap, map = worldmap,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f") + 
  geom_point(data = df_ep_na, 
             aes(x = Longitude, y = Latitude, color = Basin),
             cex = 0.2, pch = 10) +
  facet_wrap(~ Month, ncol=3)
map_ep_na_by_month
ggsave("./images/map-ep-na-storms-by-month.pdf")
ggsave("./images/map-ep-na-storms-by-month.png")











