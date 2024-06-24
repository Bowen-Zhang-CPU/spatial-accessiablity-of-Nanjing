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

##1.绘制南京市幼儿园的地理位置分布图

# 打开南京地图作为底图，相关geojson文件需要去网上下载
nanjing_geojson <- "C:/Users/53182/Desktop/托育项目/南京托育工作/南京市_县.geojson"

# 读取南京市幼儿园地理位置信息
nanjing_map <- st_read(nanjing_geojson)
coordinates_BZ0618 <- read_excel("coordinates_BZ0618.xlsx")%>%
  mutate(机构托位数=0)

# 绘制南京市幼儿园分布和人数的热力图。不过目前我没有每个幼儿园容纳的人数
p <-ggplot() +  
  geom_sf(data = nanjing_map) +    
  geom_tile(data = coordinates_BZ0618, aes(x = long, y = lat, fill = 机构托位数),    
            width = 0.005, height = 0.005) +    
  scale_fill_gradientn(name = "2023",    
                       breaks = c(0, 100, 200, 300, 400, 500),    
                       colours = c("#49F700", "#DB8E00", "#F45100","#F45100" ,"#FF051E"),    
                       limits = c(0, max(coordinates_BZ0618$机构托位数, na.rm = TRUE)))    
theme_minimal() 
ggsave("南京幼儿园可及性_附道路.png", plot = p, width = 10, height = 8, dpi = 300)

##2.以绘制的各个幼儿园为中心，分别画出15min,30min,60m的可及性图（分别对应1km，2km，4km）
#首先将幼儿园坐标转化为sf对象
kindergartens_sf <- st_as_sf(coordinates_BZ0618, coords = c("long", "lat"), crs = 4326)
# 创建可及性区域，再合并起来
# 创建15分钟、30分钟和60分钟可及性范围
accessibility_15min <- st_buffer(kindergartens_sf, dist = 1000)  # 15min可及性
accessibility_30min <- st_buffer(kindergartens_sf, dist = 2000)  # 30min可及性
accessibility_60min <- st_buffer(kindergartens_sf, dist = 4000)  # 60min可及性

# 添加`time`属性
accessibility_15min <- mutate(accessibility_15min, time = "15min")
accessibility_30min <- mutate(accessibility_30min, time = "30min")
accessibility_60min <- mutate(accessibility_60min, time = "60min")

# 合并缓冲区数据
buffers <- rbind(accessibility_15min, accessibility_30min, accessibility_60min)

#绘制15min，30min，60,min可及性图
p <-ggplot() +  
  geom_sf(data = nanjing_map, fill = "grey", color = "black") +  # 南京市地图
  geom_sf(data = buffers, aes(fill = time), alpha = 0.5) +  # 缓冲区
  geom_sf(data = kindergartens_sf, color = "#7CFC00", size = 1) +  # 幼儿园位置
  scale_fill_manual(values = c("15min" = "#7CFC00", "30min" = "blue", "60min" = "purple")) +
  theme_minimal() + 
  labs(title = "南京市幼儿园可及性范围图", x = "经度", y = "纬度", fill = "可及时间")  
ggsave("南京幼儿园可及性.png", plot = p, width = 10, height = 8, dpi = 300)


##3.绘制附加道路的可及性图


#读取本地带道路的江苏地图shapefile文件,这个需要去网上下载的，包括shp，shx等格式
jiangsu_road <- st_read("江苏省_道路.shp", crs = "+init=epsg:4326")
#把主干道和次干道先提取出来
jiangsu_road_1<-jiangsu_road %>% 
  filter(grepl("主干道",fclass_CN))
jiangsu_road_2<-jiangsu_road %>% 
  filter(grepl("次干道",fclass_CN))
jiangsu_road_all<-rbind(jiangsu_road_1,jiangsu_road_2)
#把道路加到南京的底图上
nanjing_map <- st_transform(nanjing_map, st_crs(jiangsu_road_all))#转换坐标系
nanjing_map_with_road <- st_intersection(jiangsu_road_all, nanjing_map)#把南京地图与江苏道路相交，提取公共部分

#绘制南京幼儿园附加道路的可及性图
p <-ggplot() +
  geom_sf(data = nanjing_map, fill = "grey", color = "black", size = 1) +
  geom_sf(data = accessibility_15min, aes(fill = "15min"),alpha = 0.2,color = NA) +
  geom_sf(data = accessibility_30min, aes(fill = "30min"),alpha = 0.2 ,color = NA) +
  geom_sf(data = accessibility_60min, aes(fill = "60min"), alpha = 0.2 ,color = NA) +
  scale_fill_manual(name = "可及时间", values = c("15min" = "#7CFC00", "30min" = "blue", "60min" = "purple")) +
  geom_sf(data = nanjing_map_with_road, fill = "grey", color = "black", size = 1)+
  theme_minimal() + 
  labs(title = "南京幼儿园可及性_附道路", x = "经度", y = "纬度", fill = "可及时间")  
ggsave("南京幼儿园可及性_附道路.png", plot = p, width = 10, height = 8, dpi = 300)
  




