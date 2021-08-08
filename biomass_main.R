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
wood.nb <- nb2mat(nb, style = "B", zero.policy= T) 
#wood.nb.rs <- nb2mat(nb, style = "W", zero.policy= T) 
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

round(wood.inla$summary.fitted.values$mean,3)

## straw -----------------------------------------------------------
group_straw <-straw_process(df_data)[[1]]
df_straw_nzero <- straw_process(df_data)[[2]]
straw.inla <- ene.inla("straw",6,group_straw)
straw_result_province <- fitted_value(group_straw, straw.inla)[[1]]
straw_result_nation <- fitted_value(group_straw, straw.inla)[[2]]

## ele-----------------------------------------------------------
group_ele <-ele_process(df_data)[[1]]
df_ele_nzero <- ele_process(df_data)[[2]]
ele.inla <- ene.inla("ele",6,group_ele)
ele_result_province <- fitted_ele(group_ele, ele.inla)[[1]]
ele_result_nation <- fitted_ele(group_ele, ele.inla)[[2]]

## save_result--------------------------------------------------

library("openxlsx")
sheets = list("wood_result_nation" = wood_result_nation, 
          "wood_result_province" = wood_result_province,
          "straw_result_nation" = straw_result_nation, 
          "straw_result_province" = straw_result_province, 
          "ele_result_nation" = ele_result_nation, 
          "ele_result_province" = ele_result_province)

write.xlsx(sheets,"result_combined.xlsx")

## Cross-validation--------------------------------------------------
# wood
valid_group_wood <- filter(group_wood,is.na(pre)) ## using group_wood to valid
valid_obs_wood <- valid_obs("wood", df_wood_nzero)  ## using observation to valid

#(1) group valid
yvg.wood <- valid_test('year','wood',valid_group_wood)   #year trend
pvg.wood <- valid_test('pro','wood',valid_group_wood)  # pro trend
yvg.wood$obs.flag <- "wood"
pvg.wood$group.flag <- "wood"
yvg.wood <- yvg.wood %>% rename(true = y0_wood)
pvg.wood <- pvg.wood %>% rename(true = y0_wood)
#--------------------------------------------------------------
# straw
valid_group_straw <- filter(group_straw,is.na(pre)) ## using group_straw to valid
valid_obs_straw <- valid_obs("straw",df_straw_nzero)  ## using observation to valid

#(1) group valid
yvg.straw <- valid_test('year','straw',valid_group_straw)   #year trend
pvg.straw <- valid_test('pro','straw',valid_group_straw)  # pro trend
yvg.straw$obs.flag <- "straw"
pvg.straw$group.flag <- "straw"
yvg.straw <- yvg.straw %>% rename(true = y0_straw)
pvg.straw <- pvg.straw %>% rename(true = y0_straw)
#--------------------------------------------------------------

#model selection------------------------------------------------------------

loocv <- function(fit){     ### to compute LCPO
  -2 * log(fit$cpo$cpo)
}

dics <- list()
cpos <- list()

for (n in seq(1,6)){
  model.inla <- ene.inla("biogas",n,group_biogas)
  dics[n] <- model.inla$dic$dic
  cpos[n] <- sum(loocv(model.inla),na.rm=TRUE)
}

#model validation------------------------------------------------------------
#obs.vld.wood <- valid_cross_test("wood", df_wood_nzero,1)
#obs.vld.straw <- valid_cross_test("straw", df_straw_nzero,2)
#obs.vld.biogas <- valid_cross_test("biogas", df_biogas_nzero,3)
#obs.vld.ele <- valid_cross_test("ele", df_ele_nzero,3)

group.vld.wood <- valid_cross_test("wood", group_wood,1)
group.vld.straw <- valid_cross_test("straw", group_straw,2)
group.vld.biogas <- valid_cross_test("biogas", df_biogas_nzero,3)
group.vld.ele <- valid_cross_test("ele", group_ele,4)

group.vld.wood.m <- valid_machine_test("wood",group_wood, 1)
group.vld.straw.m <- valid_machine_test("straw",group_straw, 2)
group.vld.biogas.m <- valid_machine_test("biogas",df_biogas_nzero, 3)
group.vld.ele.m <- valid_machine_test("ele",group_ele, 4)

#plot------------------------------------------------------------

lst.obs = list( yvo.wood, yvo.straw, yvo.biogas)
lst.group = list(yvg.wood, yvg.straw, yvg.biogas)
comb.group.yv <- Reduce(function(x,y) merge(x,y,all=T),lst.group)  ### combine the list
comb.obs.yv <- Reduce(function(x,y) merge(x,y,all=T),lst.obs)  ### combine the list

## year.group------------------------------------------------------------------------
comb.group.yv <- group_result_v(comb.group.yv)

jpeg(file = "LOOCV-group-year.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.group.year(comb.group.yv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-group-year-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.group.year.t(comb.group.yv)
while (!is.null(dev.list()))  dev.off()

## province.group------------------------------------------------------------------------
#lst.obs.p = list( pvo.wood, pvo.straw,  pvo.biogas)
lst.group.p = list(pvg.wood, pvg.straw, pvg.biogas)
comb.group.pv <- Reduce(function(x,y) merge(x,y,all=T),lst.group.p)  ### combine the list
#comb.obs.pv <- Reduce(function(x,y) merge(x,y,all=T),lst.obs.p)  ### combine the list

comb.group.pv <- group_result_pv(comb.group.pv)

jpeg(file = "LOOCV-group-province.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.group.pro(comb.group.pv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-group-province-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.group.pro.t(comb.group.pv)
while (!is.null(dev.list()))  dev.off()














