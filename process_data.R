## -----------------------------------------------------------
process_data <- function(dataset) {
  dataset$f_inc <- exp(dataset$f_inc)
  dataset$pro <- as.integer(as.factor(dataset$province))
  dataset <- dataset[which(dataset$year != 2017),]
  dataset <- dataset[which(!(dataset$source == "" & dataset$pre!=1)),]
  dataset <- filter(dataset, province!="海南")
  dataset <- filter(dataset, province!="上海")
  dataset <- filter(dataset, province!="西藏")
  dataset <- filter(dataset, province!="北京")
  dataset <- dplyr::select(dataset,y0_straw, y0_wood, y0_dung,
                           y0_coal, y0_gas, y0_biogas, y0_ele, 
                           year, province, pro, hhold, f_pop, f_size, 
                           f_inc, cr, f_land, Ex_coal, Ex_forest, 
                           Ex_hydro, Ex_road,pre)
  # fill NA in Ex_coal
  dataset <- filter(dataset, !is.na(Ex_coal)) %>%
    group_by(province) %>%
    summarise(mean_coal = mean(Ex_coal)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$Ex_coal)),"Ex_coal"] <- dataset[which(is.na(dataset$Ex_coal)),"mean_coal"]
  
  # fill NA in Ex_road
  dataset <- filter(dataset, !is.na(Ex_road)) %>%
    group_by(province) %>%
    summarise(mean_road = mean(Ex_road)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$Ex_road)),"Ex_road"] <- dataset[which(is.na(dataset$Ex_road)),"mean_road"]
  
  #  fill NA in  cr
  dataset <- filter(dataset, !is.na(cr)) %>%
    group_by(province) %>%
    summarise(mean_cr = mean(cr)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$cr)),"cr"] <- dataset[which(is.na(dataset$cr)),"mean_cr"]
  
  #  fill NA in  Ex_hydro
  dataset <- filter(dataset, !is.na(Ex_hydro)) %>%
    group_by(province) %>%
    summarise(mean_hydro = mean(Ex_hydro)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$Ex_hydro)),"Ex_hydro"] <- dataset[which(is.na(dataset$Ex_hydro)),"mean_hydro"]
  
  #  if f_inc > 200000 and f_inc < 500 or f_inc is NA, then fill in  mean
  dataset <- filter(dataset, f_inc < 200000 & f_inc > 500) %>%
    group_by(province,year) %>%
    summarise(mean_inc = mean(f_inc)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$f_inc > 200000 | 
                  dataset$f_inc < 500 | is.na(dataset$f_inc)),"f_inc"] <- 
    dataset[which(dataset$f_inc > 200000 | 
                    dataset$f_inc < 500 | is.na(dataset$f_inc)),"mean_inc"]
  
  # if mean_inc is na ,than fill in mean at the level of province
  dataset[which(is.na(dataset$f_inc) & dataset$province == "新疆"),
          "f_inc"] <- 
    dataset[which(dataset$year %in% seq(1992,1996) & 
                    dataset$province == "宁夏"),"f_inc"]
  
  dataset[which(is.na(dataset$f_inc) & dataset$province == "重庆"),
          "f_inc"] <- 
    dataset[which(dataset$pre == 1 & dataset$year == "1996" &
                    dataset$province == "重庆"),"f_inc"]
  dataset[which(is.na(dataset$f_inc) & dataset$province == "陕西"),
          "f_inc"] <- 
    dataset[which(dataset$pre == 1 & dataset$year == "1993" &
                    dataset$province == "陕西"),"f_inc"]
  
  #  if f_size > 500 and f_size < 10 or is NA, then fill in  mean
  dataset <- filter(dataset, f_size < 500 & f_size > 10) %>%
    group_by(province,year) %>%
    summarise(mean_size = mean(f_size)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$f_size > 500 | 
                  dataset$f_size < 10 | is.na(dataset$f_size)),"f_size"] <- 
    dataset[which(dataset$f_size > 500 | 
                    dataset$f_size < 10 | is.na(dataset$f_size)),"mean_size"]
  
  # if mean_size is na ,than fill in mean at the level of nation
  dataset$mean_size_nation <- mean(dataset$f_size,na.rm = T)
  dataset[which(is.na(dataset$f_size)),"f_size"] <- 
    dataset[which(is.na(dataset$f_size)),"mean_size_nation"]
  
  #  if f_pop > 8 or is na, then fill in  mean
  dataset <- filter(dataset, f_pop < 8) %>%
    group_by(province,year) %>%
    summarise(mean_pop = mean(f_pop)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$f_pop > 8 | is.na(dataset$f_pop)),"f_pop"] <- 
    dataset[which(dataset$f_pop > 8 | is.na(dataset$f_pop)),"mean_pop"]
  
  #  if f_land < 1 or f_land is na, then fill in  mean
  dataset <- filter(dataset, f_land > 1) %>%
    group_by(province,year) %>%
    summarise(mean_land = mean(f_land)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$f_land < 1 | is.na(dataset$f_land)),"f_land"] <- 
    dataset[which(dataset$f_land < 1 | is.na(dataset$f_land)),"mean_land"]
  
  # if mean_land is na ,than fill in mean at the level of nation
  dataset$mean_land_nation <- mean(dataset$f_land,na.rm = T)
  dataset[which(is.na(dataset$f_land)),"f_land"] <- 
    dataset[which(is.na(dataset$f_land)),"mean_land_nation"]
  ## -----------------------------------------------------------
  ## cope with outliner
  
  ## 1内蒙古
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year == "2012"),"Ex_coal"] <- 99391
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year == "2016"),"Ex_road"] <- 196061
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year == "2015"),"Ex_road"] <- 175374
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year %in% seq(1992,2001)),"Ex_hydro"] <- 49530
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year == "2016"),"Ex_hydro"] <- 95245
  dataset[which(dataset$province == "内蒙古" &
                  dataset$year == "2015"),"Ex_forest"] <- 2488
  #dataset <- dataset[-c(2,18,27),]  ## delete 2,18 row
  
  ## 2广东
  dataset[which(dataset$province == "广东" &
                  dataset$year %in% seq(1992,2001)),"Ex_hydro"] <- 4700000
  dataset[which(dataset$province == "广东" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 7.43
  dataset[which(dataset$province == "广东" &
                  dataset$year %in% seq(1992,1997)),"Ex_coal"] <- 300
  dataset[which(dataset$province == "广东" &
                  dataset$year == "2016"),"cr"] <- 2172
  
  ## 3福建
  dataset[which(dataset$province == "福建" &
                  dataset$year %in% seq(1992,2001)),"Ex_hydro"] <- 4200000
  dataset[which(dataset$province == "福建" &
                  dataset$year %in% seq(1992,2003)),"Ex_forest"] <- 735
  dataset[which(dataset$province == "福建" &
                  dataset$year %in% seq(1992,2002)),"f_land"] <- 1.12
  dataset[which(dataset$province == "福建" &
                  dataset$year == "2016"),"cr"] <- 
    
    ## 4天津
    dataset[which(dataset$province == "天津" &
                    dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 5000
  dataset[which(dataset$province == "天津" &
                  dataset$year %in% seq(1992,1997)),"Ex_forest"] <- 9.110
  dataset[which(dataset$province == "天津" &
                  dataset$year == "2016"),"cr"] <- 217.6699
  
  ## 5新疆
  dataset[which(dataset$province == "新疆" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 583259
  dataset[which(dataset$province == "新疆" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 63.960
  dataset[which(dataset$province == "新疆" &
                  dataset$year == "2016"),"cr"] <- 2891.532
  ## 6江苏
  dataset[which(dataset$province == "江苏" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 39
  dataset[which(dataset$province == "江苏" &
                  dataset$year == "2016"),"cr"] <- 4646.169
  
  ## 7浙江
  dataset[which(dataset$province == "浙江" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 41.190
  dataset[which(dataset$province == "浙江" &
                  dataset$year == "2016"),"cr"] <- 937.4212
  dataset[which(dataset$province == "浙江" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 2300000
  
  ## 8安徽
  dataset[which(dataset$province == "安徽" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 82.560
  dataset[which(dataset$province == "安徽" &
                  dataset$year == "2016"),"cr"] <- 4146.885
  dataset[which(dataset$province == "安徽" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 504097
  
  ## 9广西
  dataset[which(dataset$province == "广西" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 75.250
  dataset[which(dataset$province == "广西" &
                  dataset$year == "2016"),"cr"] <- 4596.666
  dataset[which(dataset$province == "广西" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <-2200000
  
  ## 10陕西
  dataset[which(dataset$province == "陕西" &
                  dataset$year == "2016"),"cr"] <- 1513.7030
  dataset[which(dataset$province == "陕西" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <-512362
  ## -----------------------------------------------------------
  ## 11山西
  dataset[which(dataset$province == "山西" &
                  dataset$year %in% seq(1992,2002)),"Ex_forest"] <- 105
  dataset[which(dataset$province == "山西" &
                  dataset$year =="2003"),"Ex_forest"] <- 208
  dataset[which(dataset$province == "山西" &
                  dataset$year == "2016"),"cr"] <- 1562.052
  dataset[which(dataset$province == "山西" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <-150930.0
  
  ## 12 河南
  dataset[which(dataset$province == "河南" &
                  dataset$year == "2016"),"cr"] <- 7383.8550
  dataset[which(dataset$province == "河南" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <-326598.0
  
  ## 13河北
  dataset[which(dataset$province == "河北" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 323934.0
  dataset[which(dataset$province == "河北" &
                  dataset$year %in% seq(1992,2002)),"Ex_forest"] <- 201
  dataset[which(dataset$province == "河北" &
                  dataset$year == "2016"),"cr"] <- 4157.646
  
  ## 14 黑龙江
  dataset[which(dataset$province == "黑龙江" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 183835.0
  dataset[which(dataset$province == "黑龙江" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 289.050
  dataset[which(dataset$province == "黑龙江" &
                  dataset$year == "2016"),"cr"] <- 7078.559
  
  ## 15 辽宁
  dataset[which(dataset$province == "辽宁" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 340180.0
  dataset[which(dataset$province == "辽宁" &
                  dataset$year %in% seq(1992,2001)),"Ex_forest"] <- 122
  dataset[which(dataset$province == "辽宁" &
                  dataset$year == "2016"),"cr"] <- 2397.178
  
  ## 16 吉林
  dataset[which(dataset$province == "吉林" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 325708.0
  dataset[which(dataset$province == "吉林" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 90.040
  dataset[which(dataset$province == "吉林" &
                  dataset$year == "2016"),"cr"] <- 4367.775
  
  ## 17 重庆
  dataset[which(dataset$province == "重庆" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 1100000
  dataset[which(dataset$province == "重庆" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 146.500
  dataset[which(dataset$province == "重庆" &
                  dataset$year == "2016"),"cr"] <- 1193.193
  
  ## 18 四川
  dataset[which(dataset$province == "四川" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 4600000
  dataset[which(dataset$province == "四川" &
                  dataset$year == "2016"),"cr"] <- 4004.6851
  
  ## 19 云南
  dataset[which(dataset$province == "云南" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 2800000
  dataset[which(dataset$province == "云南" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 424.500
  
  ## 20 贵州
  dataset[which(dataset$province == "贵州" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 1100000
  dataset[which(dataset$province == "贵州" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 211.920
  dataset[which(dataset$province == "贵州" &
                  dataset$year == "2016"),"cr"] <- 1499.331
  ## -----------------------------------------------------------
  
  ## 21 宁夏
  dataset[which(dataset$province == "宁夏" &
                  dataset$year %in% seq(1992,2004)),"Ex_hydro"] <- 3200
  dataset[which(dataset$province == "宁夏" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 38.710
  dataset[which(dataset$province == "宁夏" &
                  dataset$year == "2016"),"cr"] <- 472.4616
  
  ## 22 甘肃
  dataset[which(dataset$province == "甘肃" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 563494
  dataset[which(dataset$province == "甘肃" &
                  dataset$year == "2016"),"cr"] <- 1430.3920
  
  ## 23 青海
  dataset[which(dataset$province == "青海" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 275125.0
  dataset[which(dataset$province == "青海" &
                  dataset$year == "2016"),"cr"] <- 164.8624
  
  ## 24 江西
  dataset[which(dataset$province == "江西" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 1400000
  dataset[which(dataset$province == "江西" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 53.070
  dataset[which(dataset$province == "江西" &
                  dataset$year == "2016"),"cr"] <- 2508.183
  
  
  ## 25 湖南
  dataset[which(dataset$province == "湖南" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 2800000
  dataset[which(dataset$province == "湖南" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 28.620
  dataset[which(dataset$province == "湖南" &
                  dataset$year == "2016"),"cr"] <- 3741.718
  
  ## 26 湖北
  dataset[which(dataset$province == "湖北" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 2200000
  dataset[which(dataset$province == "湖北" &
                  dataset$year %in% seq(1992,1998)),"Ex_forest"] <- 241.710
  dataset[which(dataset$province == "湖北" &
                  dataset$year == "2016"),"cr"] <- 3697.645
  
  ## 27 山东
  dataset[which(dataset$province == "山东" &
                  dataset$year %in% seq(1992,2002)),"Ex_hydro"] <- 66449.00
  dataset[which(dataset$province == "山东" &
                  dataset$year == "2016"),"cr"] <- 5883.128
  dataset <- dataset[which(!(dataset$province == "山东" & dataset$cr< 1000)),]
}  
  

wood_process <- function(df_data){
  df_wood <- df_data
  df_wood <- filter(df_wood, year!= 2010)
  df_wood <- filter(df_wood, year!= 1993)
  df_wood <- df_wood[which(is.na(df_wood$y0_wood) == F),]
  df_wood[which(df_wood$y0_wood > 6000),"y0_wood"] <- df_wood[which(df_wood$y0_wood > 6000),"y0_wood"] / 10 
  sum(df_wood$y0_wood==0,na.rm = T)/nrow(df_wood)
  df_wood_nzero <- filter(df_wood, y0_wood!=0)
  df_wood_nzero <- df_wood_nzero[which(df_wood_nzero$y0_wood > 20),]
  df_wood_nzero[which(df_wood_nzero$y0_wood > 2000),"y0_wood"] <- df_wood_nzero[which(df_wood_nzero$y0_wood > 2000),"y0_wood"] / 2 
  
  df_wood_nzero[which(df_wood_nzero$year %in% c(2012,2013) & 
                        df_wood_nzero$y0_wood > 2000),"y0_wood"] <-
    df_wood_nzero[which(df_wood_nzero$year %in% c(2012,2013) & 
                          df_wood_nzero$y0_wood > 2000),"y0_wood"]/3
  
  df_wood_nzero <- df_wood_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_wood_nzero, by = c("pro","year"))
  #df_wood_nzero <- df_wood_nzero %>% count(pro,year) %>% left_join(df_wood_nzero, by = c("pro","year"))
  
  group_wood <- dplyr::select(df_wood_nzero, -y0_straw, -y0_dung,
                              -y0_coal, -y0_gas, -y0_biogas, -y0_ele, - province) %>%
    group_by(year,pro) %>%  # groupings
    summarise_each(funs(mean))          # operation
  
  group_wood$y0_wood <- log(group_wood$y0_wood)
  
  
  ##combined with df_predict
  df_predict$n <- NA
  group_wood <- df_predict %>%
    dplyr::select(-y0_straw, -y0_dung,
                  -y0_coal, -y0_gas, -y0_biogas, -y0_ele) %>%
    rbind(group_wood) 
  
  group_wood$Ex_coal <- log(group_wood$Ex_coal)
  group_wood$Ex_hydro <- log(group_wood$Ex_hydro)
  group_wood$Ex_road <- log(group_wood$Ex_road)  
  group_wood$Ex_forest <- log(group_wood$Ex_forest)    
  group_wood$cr <- log(group_wood$cr)      
  group_wood$f_inc <- log(group_wood$f_inc)  
  #group_wood$f_size <- log(group_wood$f_size) 
  
  df_wood_nzero$Ex_coal <- log(df_wood_nzero$Ex_coal)
  df_wood_nzero$Ex_hydro <- log(df_wood_nzero$Ex_hydro)
  df_wood_nzero$Ex_road <- log(df_wood_nzero$Ex_road)  
  df_wood_nzero$Ex_forest <- log(df_wood_nzero$Ex_forest)    
  df_wood_nzero$cr <- log(df_wood_nzero$cr)      
  df_wood_nzero$f_inc <- log(df_wood_nzero$f_inc)
  df_wood_nzero$y0_wood <- log(df_wood_nzero$y0_wood)
  
  result <- list(group_wood,df_wood_nzero)
}

straw_process <- function(df_data){
  df_straw <- df_data
  df_straw <- df_straw[which(is.na(df_straw$y0_straw) == F),]
  df_straw <- filter(df_straw, y0_straw!=0)
  
  df_straw[which(df_straw$y0_straw > 8000),"y0_straw"] <- df_straw[which(df_straw$y0_straw > 8000),"y0_straw"] / 4
  #sum(df_straw$y0_straw==0,na.rm = T)/nrow(df_straw)
  
  df_straw_nzero <- df_straw[which(df_straw$y0_straw > 50),]
  #df_straw_nzero[which(df_straw_nzero$y0_straw > 2000),"y0_straw"] <- df_straw_nzero[which(df_straw_nzero$y0_straw > 2000),"y0_straw"] / 2 
  
  df_straw_nzero <- df_straw_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_straw_nzero, by = c("pro","year"))
  #df_straw_nzero <- df_straw_nzero %>% count(pro,year) %>% left_join(df_straw_nzero, by = c("pro","year"))
  
  group_straw <- dplyr::select(df_straw_nzero, -y0_wood, -y0_dung,
                        -y0_coal, -y0_gas, -y0_biogas, -y0_ele, - province) %>%
    group_by(year,pro) %>%  # groupings
    summarise_each(funs(mean))          # operation
  
  group_straw$y0_straw <- log(group_straw$y0_straw)
  
  ##combined with df_predict
  df_predict$n <- NA
  group_straw <- df_predict %>%
    dplyr::select(-y0_wood, -y0_dung,
           -y0_coal, -y0_gas, -y0_biogas, -y0_ele) %>%
    rbind(group_straw) 
  
  group_straw$Ex_coal <- log(group_straw$Ex_coal)
  group_straw$Ex_hydro <- log(group_straw$Ex_hydro)
  group_straw$Ex_road <- log(group_straw$Ex_road)  
  group_straw$Ex_forest <- log(group_straw$Ex_forest)    
  group_straw$cr <- log(group_straw$cr)      
  group_straw$f_inc <- log(group_straw$f_inc)  
  #group_straw$f_size <- log(group_straw$f_size) 
  
  result <- list(group_straw,df_straw_nzero)
}

ele_process <- function(df_data){
  df_ele <- df_data
  df_ele <- df_ele[which(is.na(df_ele$y0_ele) == F),]
  df_ele <- filter(df_ele, y0_ele!=0)
  
  #df_ele[which(df_ele$y0_ele > 8000),"y0_ele"] <- df_ele[which(df_ele$y0_ele > 8000),"y0_ele"] / 4
  #sum(df_ele$y0_ele==0,na.rm = T)/nrow(df_ele)
  df_ele <- filter(df_ele, y0_ele < 8000) 
  df_ele_nzero <- filter(df_ele, y0_ele > 100)
  
  #df_ele_nzero[which(df_ele_nzero$y0_ele > 2000),"y0_ele"] <- df_ele_nzero[which(df_ele_nzero$y0_ele > 2000),"y0_ele"] / 2 
  
  df_ele_nzero <- df_ele_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_ele_nzero, by = c("pro","year"))
  #df_ele_nzero <- df_ele_nzero %>% count(pro,year) %>% left_join(df_ele_nzero, by = c("pro","year"))
  
  group_ele <- dplyr::select(df_ele_nzero, -y0_wood, -y0_dung,
                             -y0_coal, -y0_gas, -y0_biogas, -y0_straw, - province) %>%
    group_by(year,pro) %>%  # groupings
    summarise_each(funs(mean))          # operation
  
  group_ele$y0_ele <- log(group_ele$y0_ele)
  # #2拟合BOXCOX 模型
  # b=boxcox(formula, data= group_ele) # 定义函数类型和数???
  # I=which(b$y==max(b$y))
  # b$x[I]#lambda=0.55
  
  #plot(density(group_ele$y0_ele))
  
  ##combined with df_predict
  df_predict$n <- NA
  group_ele <- df_predict %>%
    dplyr::select(-y0_wood, -y0_dung,
                  -y0_coal, -y0_gas, -y0_biogas, -y0_straw) %>%
    rbind(group_ele) 
  
  group_ele$Ex_coal <- log(group_ele$Ex_coal)
  group_ele$Ex_hydro <- log(group_ele$Ex_hydro)
  group_ele$Ex_road <- log(group_ele$Ex_road)  
  group_ele$Ex_forest <- log(group_ele$Ex_forest)    
  group_ele$cr <- log(group_ele$cr)      
  group_ele$f_inc <- log(group_ele$f_inc) 
  #group_ele$f_size <- log(group_ele$f_size) 
  
  result <- list(group_ele, df_ele_nzero)
}

biogas_process <- function(df_data){
  df_biogas <- df_data
  df_biogas <- df_biogas[which(is.na(df_biogas$y0_biogas) == F),]
  df_biogas <- filter(df_biogas, y0_biogas!=0)
  
  df_biogas[which(df_biogas$y0_biogas > 2000),"y0_biogas"] <- df_biogas[which(df_biogas$y0_biogas > 2000),"y0_biogas"] / 2
  #sum(df_biogas$y0_biogas==0,na.rm = T)/nrow(df_biogas)
  
  df_biogas_nzero <- df_biogas[which(df_biogas$y0_biogas > 20),]
  #df_biogas_nzero <- df_biogas[which(df_biogas$y0_biogas > 20),]
  #df_biogas_nzero[which(df_biogas_nzero$y0_biogas > 2000),"y0_biogas"] <- df_biogas_nzero[which(df_biogas_nzero$y0_biogas > 2000),"y0_biogas"] / 2 
  
  df_biogas_nzero <- df_biogas_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_biogas_nzero, by = c("pro","year"))
  #df_biogas_nzero <- df_biogas_nzero %>% count(pro,year) %>% left_join(df_biogas_nzero, by = c("pro","year"))
  
  group_biogas <- dplyr::select(df_biogas_nzero, -y0_wood, -y0_dung,
                                -y0_coal, -y0_gas, -y0_ele, -y0_straw, - province) %>%
    group_by(year,pro) %>%  # groupings
    summarise_each(funs(mean))          # operation
  
  group_biogas$y0_biogas <- log(group_biogas$y0_biogas)
  
  ##combined with df_predict
  df_predict$n <- NA
  group_biogas <- df_predict %>%
    dplyr::select(-y0_wood, -y0_dung,
                  -y0_coal, -y0_gas, -y0_biogas, -y0_straw) %>%
    rbind(group_biogas) 
  
  group_biogas$Ex_coal <- log(group_biogas$Ex_coal)
  group_biogas$Ex_hydro <- log(group_biogas$Ex_hydro)
  group_biogas$Ex_road <- log(group_biogas$Ex_road)  
  group_biogas$Ex_forest <- log(group_biogas$Ex_forest)    
  group_biogas$cr <- log(group_biogas$cr)      
  group_biogas$f_inc <- log(group_biogas$f_inc)  
  #group_biogas$f_size <- log(group_biogas$f_size) 
  
  df_biogas_nzero$Ex_coal <- log(df_biogas_nzero$Ex_coal)
  df_biogas_nzero$Ex_hydro <- log(df_biogas_nzero$Ex_hydro)
  df_biogas_nzero$Ex_road <- log(df_biogas_nzero$Ex_road)  
  df_biogas_nzero$Ex_forest <- log(df_biogas_nzero$Ex_forest)    
  df_biogas_nzero$cr <- log(df_biogas_nzero$cr)      
  df_biogas_nzero$f_inc <- log(df_biogas_nzero$f_inc)
  df_biogas_nzero$y0_biogas <- log(df_biogas_nzero$y0_biogas)
  
  result <- list(group_biogas, df_biogas_nzero)
}


coal_process <- function(df_data){
  df_coal <- df_data
  #df_coal <- filter(df_coal, year!= 2010)
 # df_coal <- filter(df_coal, year!= 1993)
  df_coal <- df_coal[which(is.na(df_coal$y0_coal) == F),]
  df_coal[which(df_coal$y0_coal > 20000),"y0_coal"] <- df_coal[which(df_coal$y0_coal > 20000),"y0_coal"] / 10 
  sum(df_coal$y0_coal==0,na.rm = T)/nrow(df_coal)
  df_coal_nzero <- filter(df_coal, y0_coal!=0)
  df_coal_nzero <- df_coal_nzero[which(df_coal_nzero$y0_coal > 20),]
  #df_coal_nzero[which(df_coal_nzero$y0_coal > 2000),"y0_coal"] <- df_coal_nzero[which(df_coal_nzero$y0_coal > 2000),"y0_coal"] / 2 
  
  # df_coal_nzero[which(df_coal_nzero$year %in% c(2012,2013) & 
  #                       df_coal_nzero$y0_coal > 2000),"y0_coal"] <-
  #   df_coal_nzero[which(df_coal_nzero$year %in% c(2012,2013) & 
  #                         df_coal_nzero$y0_coal > 2000),"y0_coal"]/3
  
  df_coal_nzero <- df_coal_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_coal_nzero, by = c("pro","year"))
  #df_coal_nzero <- df_coal_nzero %>% count(pro,year) %>% left_join(df_coal_nzero, by = c("pro","year"))
  
  group_coal <- dplyr::select(df_coal_nzero, -y0_straw, -y0_dung,
                              -y0_wood, -y0_gas, -y0_biogas, -y0_ele, - province) %>%
    group_by(year,pro) %>%  # groupings
    summarise_each(funs(mean))          # operation
  
  group_coal$y0_coal <- log(group_coal$y0_coal)
  
  
  ##combined with df_predict
  df_predict$n <- NA
  group_coal <- df_predict %>%
    dplyr::select(-y0_straw, -y0_dung,
                  -y0_wood, -y0_gas, -y0_biogas, -y0_ele) %>%
    rbind(group_coal) 
  
  group_coal$Ex_coal <- log(group_coal$Ex_coal)
  group_coal$Ex_hydro <- log(group_coal$Ex_hydro)
  group_coal$Ex_road <- log(group_coal$Ex_road)  
  group_coal$Ex_forest <- log(group_coal$Ex_forest)    
  group_coal$cr <- log(group_coal$cr)      
  group_coal$f_inc <- log(group_coal$f_inc)  
  #group_coal$f_size <- log(group_coal$f_size) 
  
  df_coal_nzero$Ex_coal <- log(df_coal_nzero$Ex_coal)
  df_coal_nzero$Ex_hydro <- log(df_coal_nzero$Ex_hydro)
  df_coal_nzero$Ex_road <- log(df_coal_nzero$Ex_road)  
  df_coal_nzero$Ex_forest <- log(df_coal_nzero$Ex_forest)    
  df_coal_nzero$cr <- log(df_coal_nzero$cr)      
  df_coal_nzero$f_inc <- log(df_coal_nzero$f_inc)
  df_coal_nzero$y0_coal <- log(df_coal_nzero$y0_coal)
  
  result <- list(group_coal,df_coal_nzero)
}