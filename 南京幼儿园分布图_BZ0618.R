

rm(list = ls())
setwd("C:/Users/53182/Desktop/托育项目/南京托育工作")
#安装包
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(lubridate)
library(dplyr)
library(rvest)
library(rjson)
library(rlist)
library(stringr)
source("C:/Users/53182/Desktop/托育项目/南京托育工作/coord_transform.r")
library(sf)
library(gdistance)
library(data.table)
library(sp)
library(raster)
library(terra)
library(foreign)
library(hhi)
library(leaflet)
library(ggplot2)
library(dplyr)
library(hchinamap)
library(showtext)



# 打开南京地图作为底图
nanjing_geojson <- "C:/Users/53182/Desktop/托育项目/南京托育工作/南京市_县.geojson"

# 读取南京市幼儿园地理位置信息
nanjing_map <- st_read(nanjing_geojson)
coordinates_BZ0618 <- read_excel("coordinates_BZ0618.xlsx")%>%
  mutate(机构托位数=0)

# 绘制南京市幼儿园分布和人数的热力图。目前我没每个有幼儿园容纳的人数
ggplot() +  
  geom_sf(data = nanjing_map) +    
  geom_tile(data = coordinates_BZ0618, aes(x = long, y = lat, fill = 机构托位数),    
            width = 0.008, height = 0.008) +    
  scale_fill_gradientn(name = "2023",    
                       breaks = c(0, 100, 200, 300, 400, 500),    
                       colours = c("#49F700", "#DB8E00", "#F45100","#F45100" ,"#FF051E"),    
                       limits = c(0, max(coordinates_BZ0618$机构托位数, na.rm = TRUE)))    
 theme_minimal() 





                      
                       



