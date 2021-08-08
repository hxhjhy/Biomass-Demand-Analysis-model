## -----------------------------------------------------------
process_data <- function(dataset) {
  dataset$f_inc <- exp(dataset$f_inc)
  dataset$pro <- as.integer(as.factor(dataset$province))
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
 }  
  

wood_process <- function(df_data){
  df_wood <- df_data
  df_wood <- df_wood[which(is.na(df_wood$y0_wood) == F),]
  sum(df_wood$y0_wood==0,na.rm = T)/nrow(df_wood)
  df_wood_nzero <- filter(df_wood, y0_wood!=0)
  df_wood_nzero <- df_wood_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_wood_nzero, by = c("pro","year"))
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
  df_straw_nzero <- filter(df_straw, y0_straw!=0)
  df_straw_nzero <- df_straw_nzero %>% group_by(pro, year) %>% summarise(n = n()) %>% left_join(df_straw_nzero, by = c("pro","year"))
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
