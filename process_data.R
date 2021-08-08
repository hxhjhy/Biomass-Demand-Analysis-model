## -----------------------------------------------------------
process_data <- function(dataset) {
  dataset$H_inc <- exp(dataset$H_inc)
  dataset$pro <- as.integer(as.factor(dataset$province))
  dataset <- dataset[which(!(dataset$source == "" & dataset$pre!=1)),]
  dataset <- filter(dataset, province!="海南")
  dataset <- filter(dataset, province!="上海")
  dataset <- filter(dataset, province!="西藏")
  dataset <- filter(dataset, province!="北京")
  dataset <- dplyr::select(dataset,y0_straw, y0_wood, y0_dung,
                           y0_coal, y0_gas, y0_biogas, y0_ele, 
                           year, province, pro, hhold, H_pop, H_size, 
                           H_inc, P_straw, P_land, P_coal, P_forest, 
                          P_hydro, P_road,pre)
  # fill NA in P_coal
  dataset <- filter(dataset, !is.na(P_coal)) %>%
    group_by(province) %>%
    summarise(mean_coal = mean(P_coal)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$P_coal)),"P_coal"] <- dataset[which(is.na(dataset$P_coal)),"mean_coal"]
  
  # fill NA in P_road
  dataset <- filter(dataset, !is.na(P_road)) %>%
    group_by(province) %>%
    summarise(mean_road = mean(P_road)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$P_road)),"P_road"] <- dataset[which(is.na(dataset$P_road)),"mean_road"]
  
  #  fill NA in  P_straw
  dataset <- filter(dataset, !is.na(P_straw)) %>%
    group_by(province) %>%
    summarise(mean_P_straw = mean(P_straw)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$P_straw)),"P_straw"] <- dataset[which(is.na(dataset$P_straw)),"mean_P_straw"]
  
  #  fill NA in  P_hydro
  dataset <- filter(dataset, !is.na(P_hydro)) %>%
    group_by(province) %>%
    summarise(mean_hydro = mean(P_hydro)) %>%
    right_join(dataset, by="province")  # operation
  dataset[which(is.na(dataset$P_hydro)),"P_hydro"] <- dataset[which(is.na(dataset$P_hydro)),"mean_hydro"]
  
   # if mean_inc is na ,than fill in mean at the level of province
  dataset[which(is.na(dataset$H_inc) & dataset$province == "新疆"),
          "H_inc"] <- 
    dataset[which(dataset$year %in% seq(1992,1996) & 
                    dataset$province == "宁夏"),"H_inc"]
  
  dataset[which(is.na(dataset$H_inc) & dataset$province == "重庆"),
          "H_inc"] <- 
    dataset[which(dataset$pre == 1 & dataset$year == "1996" &
                    dataset$province == "重庆"),"H_inc"]
  dataset[which(is.na(dataset$H_inc) & dataset$province == "陕西"),
          "H_inc"] <- 
    dataset[which(dataset$pre == 1 & dataset$year == "1993" &
                    dataset$province == "陕西"),"H_inc"]
  
  #  if H_size > 500 and H_size < 10 or is NA, then fill in  mean
  dataset <- filter(dataset, H_size < 500 & H_size > 10) %>%
    group_by(province,year) %>%
    summarise(mean_size = mean(H_size)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$H_size > 500 | 
                  dataset$H_size < 10 | is.na(dataset$H_size)),"H_size"] <- 
    dataset[which(dataset$H_size > 500 | 
                    dataset$H_size < 10 | is.na(dataset$H_size)),"mean_size"]
  
  # if mean_size is na ,than fill in mean at the level of nation
  dataset$mean_size_nation <- mean(dataset$H_size,na.rm = T)
  dataset[which(is.na(dataset$H_size)),"H_size"] <- 
    dataset[which(is.na(dataset$H_size)),"mean_size_nation"]
  
  #  if H_pop > 8 or is na, then fill in  mean
  dataset <- filter(dataset, H_pop < 8) %>%
    group_by(province,year) %>%
    summarise(mean_pop = mean(H_pop)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$H_pop > 8 | is.na(dataset$H_pop)),"H_pop"] <- 
    dataset[which(dataset$H_pop > 8 | is.na(dataset$H_pop)),"mean_pop"]
  
  #  if P_land < 1 or P_land is na, then fill in  mean
  dataset <- filter(dataset, P_land > 1) %>%
    group_by(province,year) %>%
    summarise(mean_land = mean(P_land)) %>%
    right_join(dataset, by=c("province","year"))  # operation
  dataset[which(dataset$P_land < 1 | is.na(dataset$P_land)),"P_land"] <- 
    dataset[which(dataset$P_land < 1 | is.na(dataset$P_land)),"mean_land"]
  
  # if mean_land is na ,than fill in mean at the level of nation
  dataset$mean_land_nation <- mean(dataset$P_land,na.rm = T)
  dataset[which(is.na(dataset$P_land)),"P_land"] <- 
    dataset[which(is.na(dataset$P_land)),"mean_land_nation"]
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
  
  group_wood$P_coal <- log(group_wood$P_coal)
  group_wood$P_hydro <- log(group_wood$P_hydro)
  group_wood$P_road <- log(group_wood$P_road)  
  group_wood$P_forest <- log(group_wood$P_forest)    
  group_wood$P_straw <- log(group_wood$P_straw)      
  group_wood$H_inc <- log(group_wood$H_inc)  
  #group_wood$H_size <- log(group_wood$H_size) 
  
  df_wood_nzero$P_coal <- log(df_wood_nzero$P_coal)
  df_wood_nzero$P_hydro <- log(df_wood_nzero$P_hydro)
  df_wood_nzero$P_road <- log(df_wood_nzero$P_road)  
  df_wood_nzero$P_forest <- log(df_wood_nzero$P_forest)    
  df_wood_nzero$P_straw <- log(df_wood_nzero$P_straw)      
  df_wood_nzero$H_inc <- log(df_wood_nzero$H_inc)
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
  
  group_straw$P_coal <- log(group_straw$P_coal)
  group_straw$P_hydro <- log(group_straw$P_hydro)
  group_straw$P_road <- log(group_straw$P_road)  
  group_straw$P_forest <- log(group_straw$P_forest)    
  group_straw$P_straw <- log(group_straw$P_straw)      
  group_straw$H_inc <- log(group_straw$H_inc)  
  #group_straw$H_size <- log(group_straw$H_size) 
  
  result <- list(group_straw,df_straw_nzero)
}
