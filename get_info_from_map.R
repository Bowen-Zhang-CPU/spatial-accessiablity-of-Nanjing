# 清除当前环境
rm(list=ls())

# 设置路径
setwd("C:\用户\53182\桌面\苏州人口项目\南京托育工作")

# 加载包
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(stringi)
library(lubridate)
library(tableone)
library(stringr)
library(writexl)
library(rvest)
library(rjson)
library(rlist)
library(xlsx)
source('coord_transform.r')


# 一、构建地理信息抓取函数======================================================
my_key <- 'c9e5ce1559291978be7cba4b3579a6b6'#这是高德地图api
key <- c('c9e5ce1559291978be7cba4b3579a6b6',
         '94ff47c2ae72871b49194f06eee2c4a8',
         '50cf69a8b8a5f515312495ddb6cf3e15',
         'a018501cba0d489cd4b4130ffccbcc94',
         'e05d3276e193c3a40f1a81d9a9d654d8',
         'ac915e9c8a063790b9ae1804a3123571',
         '5477422f1bad0326faf4927d639d7a31')

##=========================================================================
# 方式1：通过地理编码抓取------------------优先用此方法，没找到的幼儿园用方法2再跑一下
get_coord_f <- function(addr, key = my_key){
  url <- paste('https://restapi.amap.com/v3/geocode/geo?address=', 
               addr,
               '&output=JSON&key=', key, sep = '' )
  html <- read_html(url)
  addr_json <- html %>% 
    html_element(xpath = '//body/p') %>% 
    html_text()
  rst <- fromJSON(addr_json)
  if(rst$status == 0){
    coord = rst$info
    city_d = rst$info
  }else{
    coord <- rst$geocodes[[1]]$location
    formatted_address <- rst$geocodes[[1]]$formatted_address
    province <- rst$geocodes[[1]]$province
    city <- rst$geocodes[[1]]$city
    district <- ifelse(length(rst$geocodes[[1]]$district) == 0, 0, rst$geocodes[[1]]$district[[1]])
  }
  coord_ <- str_split(coord, ',')[[1]]
  long <- GCJ02toWGS84_lng(as.numeric(coord_[1]),as.numeric(coord_[2]))
  lat <- GCJ02toWGS84_lat(as.numeric(coord_[1]),as.numeric(coord_[2]))
  hos_df <- c(long, lat, formatted_address, coord, province, city, district)
  return(hos_df)
}


get_coord_f('中国药科大学')  # 测试函数结果是否与以下一致
# "118.909010616695"    "31.8999311280703"       "江苏省南京市江宁区中国药科大学" 
# "118.914024,31.897751"   "江苏省"     "南京市"        "江宁区" 
# 分别代表：经度（wgs84），纬度（wgs84），高德名称，高德坐标，省，市，区县

# 二、批量抓取坐标==============================================================
# 读取数据
hospital_all <- read_xlsx("C:/Users/53182/Desktop/苏州人口项目/南京托育工作/截至2021年南京市幼儿园名单.xlsx")

hospital_all <- hospital_all %>%
  rename( 通用名 = colnames(hospital_all[2]))
# 获取唯一的医疗机构名称
addr <- unique(hospital_all$幼儿园名称)

coordinates <- data.frame(addr = addr)
coordinates$long <- NA
coordinates$lat <- NA
coordinates$formatted_address <- NA
coordinates$coord <- NA
coordinates$province <- NA
coordinates$city <- NA
coordinates$district <- NA
coordinates$err <- 0

for (i in seq_along(addr)){
  tryCatch({
    keyi <- i%%7+1
    res <- get_coord_f(addr[i],key = key[keyi])  # 以方式1进行调用
    coordinates$long[i] <- res[1]
    coordinates$lat[i] <- res[2]
    coordinates$formatted_address[i] <- res[3]
    coordinates$coord[i] <- res[4]
    coordinates$province[i] <- res[5]
    coordinates$city[i] <- res[6]
    coordinates$district[i] <- res[7]
  }, error = function(e){
    coordinates$err[i] <<- 1
    print(e[[1]])
  })
  if (i %% 40 == 0){##打印进度
    cat(as.character(Sys.time()), '--', round(i / length(addr) * 100, 2), '% finished~~\n', sep = '')
  }
  Sys.sleep(0.35)
}



# 导出数据
write.csv(coordinates,row.names = FALSE, fileEncoding = "GBK")
coordinates<-coordinates %>% 
  mutate(long= as.numeric(long),
         lat= as.numeric(lat),
         formatted_address= as.character(formatted_address),
         coord= as.character(coord),
         province= as.character(province),
         city= as.character(city),
         district= as.character(district)) %>% 
  distinct()

write_xlsx(coordinates, "coordinates.xlsx")

##===============================================================
# 我计划用陆露提供的方法2抓方法1里有问题的幼儿园
# 方式2：通过搜索POI抓取---------------------------------
get_coord_poi <- function(addr, key = my_key){
  url_1 <- paste('https://restapi.amap.com/v3/place/text?keywords=', 
                 addr,
                 '&output=JSON&key=', key, sep = '' )
  html <- read_html(url_1)
  addr_json <- html %>% 
    html_element(xpath = '//body/p') %>% 
    html_text()
  rst_1 <- fromJSON(addr_json)
  if(rst_1$status == 0){
    coord = rst_1$info
    city_d = rst_1$info
  }else if(length(rst_1$pois) == 0){
    coord <- NA
    formatted_address <- NA
    province <- NA
    city <- NA
    district <- NA
  }else{
    coord <- rst_1$pois[[1]]$location
    formatted_address <- rst_1$pois[[1]]$name
    province <- rst_1$pois[[1]]$pname
    city <- rst_1$pois[[1]]$cityname
    district <- rst_1$pois[[1]]$adname
    #    district <- ifelse(length(rst_1$pois[[1]]$adname) == 0, 0, rst_1$pois[[1]]$adname[[1]])
  }
  coord_ <- str_split(coord, ',')[[1]]
  long <- GCJ02toWGS84_lng(as.numeric(coord_[1]),as.numeric(coord_[2]))
  lat <- GCJ02toWGS84_lat(as.numeric(coord_[1]),as.numeric(coord_[2]))
  hos_df <- c(long, lat, formatted_address, coord, province, city, district)
  return(hos_df)
}


get_coord_poi('中国药科大学')   #测试函数
# "118.913190554744"     "31.9050765453977"     "中国药科大学江宁校区" "118.918208,31.902898"
# "江苏省"               "南京市"               "江宁区"  




# 二、批量抓取坐标==============================================================
# 读取数据
kindergarten_Nanjing <- read_xlsx("C:/Users/53182/Desktop/苏州人口项目/南京托育工作/截至2021年南京市幼儿园名单.xlsx")
# 获取唯一的医疗机构名称
addr <- unique(hospital_all$医疗机构名称)

coordinates <- data.frame(addr = addr)
coordinates$long <- NA
coordinates$lat <- NA
coordinates$coord <- NA
coordinates$province <- NA
coordinates$city <- NA
coordinates$district <- NA
coordinates$err <- 0

for (i in seq_along(addr)){
  tryCatch({
    keyi <- i%%7+1
    res <- get_coord_poi(addr[i], key = key[keyi])  # 以方式1进行调用
    coordinates$long[i] <- res[1]
    coordinates$lat[i] <- res[2]
    coordinates$formatted_address[i] <- res[3]
    coordinates$coord[i] <- res[4]
    coordinates$province[i] <- res[5]
    coordinates$city[i] <- res[6]
    coordinates$district[i] <- res[7]
  }, error = function(e){
    coordinates$err[i] <<- 1
    print(e[[1]])
  })
  if (i %% 40 == 0){##打印进度
    cat(as.character(Sys.time()), '--', round(i / length(addr) * 100, 2), '% finished~~\n', sep = '')
  }
  Sys.sleep(0.35)
}



# 导出数据
write_xlsx(coordinates, "coordinates3.xlsx")

