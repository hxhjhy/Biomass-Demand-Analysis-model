## -----------------------------------------------------------
library(MASS)
library(haven)
library("tidyr")
library("sp")
library("spacetime")
library(data.table)
library("plyr")
library("dplyr")
options(dplyr.summarise.inform = FALSE)
library("ggplot2")
library("gstat")
library("RColorBrewer")
library("sp")
library("spdep")
library("spacetime")
library("mgcv")
library("maps")
library("grid")
library("gridExtra")
library("mapdata")
library(maptools)
library(INLA)
library(splines)
library("mlr")
#library("tidyverse")
remove(list=ls())
options(max.print=1000000)
## -----------------------------------------------------------
setwd("D:\\study\\biomass")
source('process_data.R',encoding = "utf-8")
source('inla_compute.R')
source('predict_compute.R')
source('choice_variable.R')
source("biomass_plot.R")
source('model_validation.R')
source('model_selection.R')
## -----------------------------------------------------------
# prepare the dataset
dataset <-read_stata("biomass.dta")
dataset <- data.table(dataset)
dataset <- process_data(dataset)  
df_predict <- filter(dataset, pre == 1) %>% 
  distinct(province, year, .keep_all = TRUE)
df_data <- filter(dataset, is.na(pre))
## -----------------------------------------------------------
#  Construct spatial matirx
map <- rgdal :: readOGR("bou2_4p.shp")
map <- subset(map, AREA > 0.4)
nb <- poly2nb(map)
head(nb)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
#wood.nb <- nb2mat(nb, style = "B", zero.policy= T) 
wood.nb.rs <- nb2mat(nb, style = "W", zero.policy= T) 
image(inla.graph2matrix(g),xlab="",ylab="")
# Plot results
coords <- coordinates(map)
jpeg(file = "neighboor plot.jpg",width=600*2,height=3*600,res=72*2)

## wood -----------------------------------------------------------
group_wood <-wood_process(df_data)[[1]]
df_wood_nzero <- wood_process(df_data)[[2]]
wood.inla <- ene.inla("wood",6, group_wood)
wood_result_province <- fitted_value(group_wood, wood.inla)[[1]]
wood_result_nation <- fitted_value(group_wood, wood.inla)[[2]]

## straw -----------------------------------------------------------
group_straw <-straw_process(df_data)[[1]]
df_straw_nzero <- straw_process(df_data)[[2]]
straw.inla <- ene.inla("straw",6,group_straw)
straw_result_province <- fitted_value(group_straw, straw.inla)[[1]]
straw_result_nation <- fitted_value(group_straw, straw.inla)[[2]]

## save_result--------------------------------------------------

library("openxlsx")
sheets = list("wood_result_nation" = wood_result_nation, 
          "wood_result_province" = wood_result_province,
          "straw_result_nation" = straw_result_nation, 
          "straw_result_province" = straw_result_province)

write.xlsx(sheets,"result_combined.xlsx")

#model selection------------------------------------------------------------

loocv <- function(fit){     ### to compute LCPO
  -2 * log(fit$cpo$cpo)
}
wood_dics <- list()
wood_cpos <- list()
straw_dics <- list()
straw_cpos <- list()

for (n in seq(1,6)){  
  mdoel.inla.straw <- ene.inla("straw",n,group_straw)
  straw_dics[n] <- straw.inla.straw$dic$dic
  straw_cpos[n] <- sum(loocv(model.inla.straw),na.rm=TRUE)
}

for (n in seq(1,6)){  
  model.inla.wood <- ene.inla("wood",n,group_wood)
  wood_dics[n] <- model.inla.wood$dic$dic
  wood_cpos[n] <- sum(loocv(model.inla.wood),na.rm=TRUE)
}

## model validation-------------------------------------------------------
# wood
valid_group_wood <- filter(group_wood,is.na(pre)) ## using group_wood to valid
cross_valid <- valid_cross_test('wood',valid_group_wood, 1) # five-folds to cross-validation
group.vld.wood.m <- valid_machine_test("wood",group_wood, 1)
#summary results of annually iterated or province iterated cross-validation for biomass consumption
yvg.wood <- valid_test('year','wood',valid_group_wood)   #year trend
pvg.wood <- valid_test('pro','wood',valid_group_wood)  # pro trend
yvg.wood$group.flag <- "wood"
pvg.wood$group.flag <- "wood"
yvg.wood <- yvg.wood %>% rename(true = y0_wood)
pvg.wood <- pvg.wood %>% rename(true = y0_wood)
#---------------------------------------------------------------------------
# straw
valid_group_straw <- filter(group_straw,is.na(pre)) ## using group_straw to valid
cross_valid <- valid_cross_test('straw',valid_group_straw, 2) # five-folds to cross-validation
group.vld.straw.m <- valid_machine_test("straw",group_straw, 2)
yvg.straw <- valid_test('year','straw',valid_group_straw)   #year trend
pvg.straw <- valid_test('pro','straw',valid_group_straw)  # pro trend
yvg.straw$group.flag <- "straw"
pvg.straw$group.flag <- "straw"
yvg.straw <- yvg.straw %>% rename(true = y0_straw)
pvg.straw <- pvg.straw %>% rename(true = y0_straw)


#plot------------------------------------------------------------

## year.group
lst.group = list(yvg.wood, yvg.straw)
comb.group.yv <- Reduce(function(x,y) merge(x,y,all=T),lst.group)  ### combine the list
comb.group.yv <- group_result_v(comb.group.yv)

jpeg(file = "LOOCV-group-year.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.group.year(comb.group.yv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-group-year-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.group.year.t(comb.group.yv)
while (!is.null(dev.list()))  dev.off()

## province.group------------------------------------------------------------------------
lst.group.p = list(pvg.wood, pvg.straw)
comb.group.pv <- Reduce(function(x,y) merge(x,y,all=T),lst.group.p)  ### combine the list
comb.group.pv <- group_result_pv(comb.group.pv)

jpeg(file = "LOOCV-group-province.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.group.pro(comb.group.pv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-group-province-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.group.pro.t(comb.group.pv)
while (!is.null(dev.list()))  dev.off()














