## -----------------------------------------------------------
library(MASS)
library(haven)
library("tidyr")
library("STRbook")
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
setwd("D:\\study\\study\\science\\paper\\biomass\\2021_new_data")
source('process_data.R',encoding = "utf-8")
source('inla.compute.R')
source('predict_compute.R')
source('result.adj.R')
source('Cross-validation.R')
source("validation-adj.R")
source("biomass.plot.R")
source('model.validation.R')
## -----------------------------------------------------------
dataset <-read_stata("hx_data322.dta")
dataset <- data.table(dataset)
dataset <- process_data(dataset)  
df_predict <- filter(dataset, pre == 1) %>% 
  distinct(province, year, .keep_all = TRUE)
df_data <- filter(dataset, is.na(pre))
## -----------------------------------------------------------
# Irregular lattice
map <- rgdal :: readOGR("bou2_4p.shp")
map <- subset(map, AREA > 0.4)
nb <- poly2nb(map)
head(nb)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
wood.nb <- nb2mat(nb, style = "B", zero.policy= T) 
wood.nb.rs <- nb2mat(nb, style = "W", zero.policy= T) 
image(inla.graph2matrix(g),xlab="",ylab="")
# Plot original results
#coords <- coordinates(map)
#jpeg(file = "neighboor plot.jpg",width=600*2,height=3*600,res=72*2)
# Show the results
#plot(map)
#plot(nb, coords, col="red", lwd = 2,add = TRUE)
#while (!is.null(dev.list()))  dev.off()

## coal -----------------------------------------------------------
group_coal <-coal_process(df_data)[[1]]
df_coal_nzero <- coal_process(df_data)[[2]]
coal.inla <- ene.inla("coal",6, group_coal)

coal_result_province <- fitted_value(group_coal, coal.inla)[[1]]
coal_result_nation <- fitted_value(group_coal, coal.inla)[[2]]

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

## biogas-----------------------------------------------------------
group_biogas <-biogas_process(df_data)[[1]]
df_biogas_nzero <- biogas_process(df_data)[[2]]
biogas.inla <- ene.inla("biogas",6,group_biogas)
biogas_result_province <- fitted_biogas(group_biogas, biogas.inla)[[1]]
biogas_result_nation <- fitted_biogas(group_biogas, biogas.inla)[[2]]

## save_result--------------------------------------------------

library("openxlsx")
sheets = list("wood_result_nation" = wood_result_nation, 
          "wood_result_province" = wood_result_province,
          "straw_result_nation" = straw_result_nation, 
          "straw_result_province" = straw_result_province, 
          "ele_result_nation" = ele_result_nation, 
          "ele_result_province" = ele_result_province,
          "biogas_result_nation" = biogas_result_nation, 
          "biogas_result_province" = biogas_result_province)

write.xlsx(sheets,"result_combined5.xlsx")


library("openxlsx")
sheets = list("coal_result_nation" = coal_result_nation, 
              "coal_result_province" = coal_result_province)

write.xlsx(sheets,"result_coal.xlsx")

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

#(2) obs valid.
yvo.wood <- valid_test('year','wood',valid_obs_wood)  #year trend
pvo.wood <- valid_test('pro','wood',valid_obs_wood)  # pro trend
yvo.wood$obs.flag <- "obs.wood"
pvo.wood$group.flag <- "obs.wood"
yvo.wood <- yvo.wood %>% rename(true = y0_wood)
pvo.wood <- pvo.wood %>% rename(true = y0_wood)

#(3) missing-value-trend.

jsvg.wood <- js_valid_test("wood", valid_group_wood)
valid_obs_wood_js <- valid_obs_wood %>% distinct(f_inc, .keep_all = TRUE)  
jsvo.wood <- js_valid_test("wood", valid_obs_wood_js)
jsvg.wood$group.flag <- "group.wood"
jsvo.wood$group.flag <- "obs.wood"
jsvg.wood <- jsvg.wood %>% rename(true = y0_wood)
jsvo.wood <- jsvo.wood %>% rename(true = y0_wood)

#(4) using hebei(2012???2013,2014,2016) to predict 
#guangdong(2013,2016).

valid_obs_wood_hb <- valid_obs_wood %>% filter(pro == 10 | pro == 31)  
valid_obs_wood_hb <- valid_obs_wood_hb %>% distinct(f_inc,.keep_all = TRUE)
hbvo.wood <- hb_valid_test("wood", valid_obs_wood_hb)
hbvo.wood$obs.flag <- "obs.wood"
hbvo.wood <- hbvo.wood %>% rename(true = y0_wood)
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

#(2) obs valid.
yvo.straw <- valid_test('year','straw',valid_obs_straw)  #year trend
pvo.straw <- valid_test('pro','straw',valid_obs_straw)  # pro trend
yvo.straw$obs.flag <- "obs.straw"
pvo.straw$group.flag <- "obs.straw"
yvo.straw <- yvo.straw %>% rename(true = y0_straw)
pvo.straw <- pvo.straw %>% rename(true = y0_straw)

#(3) missing-value-trend.
jsvg.straw <- js_valid_test("straw", valid_group_straw)
valid_obs_straw_js <- valid_obs_straw %>% distinct(f_inc, .keep_all = TRUE)  
jsvo.straw <- js_valid_test("straw", valid_obs_straw_js)
jsvg.straw$group.flag <- "group.straw"
jsvo.straw$group.flag <- "obs.straw"
jsvg.straw <- jsvg.straw %>% rename(true = y0_straw)
jsvo.straw <- jsvo.straw %>% rename(true = y0_straw)

province_year <- filter(dataset, is.na(pre)) %>% 
  distinct(year,province, pro) 

#(4) using hebei(2012???2013,2014,2016) to predict 
#zhejiang(2013,2016).

valid_obs_straw_hb <- valid_obs_straw %>% filter(pro == 10 | pro == 31)  
valid_obs_straw_hb <- valid_obs_straw_hb %>% distinct(f_inc,.keep_all = TRUE)
hbvo.straw <- hb_valid_test("straw", valid_obs_straw_hb)
hbvo.straw$obs.flag <- "obs.straw"
hbvo.straw <- hbvo.straw %>% rename(true = y0_straw)

#--------------------------------------------------------------
# biogas
valid_group_biogas <- filter(group_biogas,is.na(pre)) ## using group_biogas to valid
valid_obs_biogas <- valid_obs("biogas",df_biogas_nzero)  ## using observation to valid
#(1) group valid
yvg.biogas <- valid_test('year','biogas',valid_group_biogas)   #year trend
pvg.biogas <- valid_test('pro','biogas',valid_group_biogas)  # pro trend
yvg.biogas$obs.flag <- "biogas"
pvg.biogas$group.flag <- "biogas"
yvg.biogas <- yvg.biogas %>% rename(true = y0_biogas)
pvg.biogas <- pvg.biogas %>% rename(true = y0_biogas)

#(2) obs valid.
yvo.biogas <- valid_test('year','biogas',valid_obs_biogas)  #year trend
pvo.biogas <- valid_test('pro','biogas',valid_obs_biogas)  # pro trend
yvo.biogas$obs.flag <- "obs.biogas"
pvo.biogas$group.flag <- "obs.biogas"
yvo.biogas <- yvo.biogas %>% rename(true = y0_biogas)
pvo.biogas <- pvo.biogas %>% rename(true = y0_biogas)

#(3) missing-value-trend. 2008,2009 have no biogas for jiangsu
# jsvg.biogas <- js_valid_test("biogas", valid_group_biogas)
# valid_obs_biogas_js <- valid_obs_biogas %>% distinct(f_inc, .keep_all = TRUE)
# jsvo.biogas <- js_valid_test("biogas", valid_obs_biogas_js)
# jsvg.biogas$group.flag <- "group.biogas"
# jsvo.biogas$group.flag <- "obs.biogas"
# jsvg.biogas <- jsvg.biogas %>% rename(true = y0_biogas)
# jsvo.biogas <- jsvo.biogas %>% rename(true = y0_biogas)

# # #(4) using hebei(2012???2013,2014,2016) to predict 
# # #guangdong(2013,2016).

# valid_obs_biogas_hb <- valid_obs_biogas %>% filter(pro == 10 | pro == 31)
# valid_obs_biogas_hb <- valid_obs_biogas_hb %>% distinct(f_inc,.keep_all = TRUE)
# hbvo.biogas <- hb_valid_test("biogas", valid_obs_biogas_hb)
# hbvo.biogas$obs.flag <- "obs.biogas"
# hbvo.biogas <- hbvo.biogas %>% rename(true = y0_biogas)



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

## jiangsu.obs-2009------------------------------------------------------------------------

lst.obs.js = list( jsvo.wood, jsvo.straw)
comb.obs.jsv <- Reduce(function(x,y) merge(x,y,all=T),lst.obs.js)  ### combine the list
comb.obs.jsv <- obs_result_jsv(comb.obs.jsv)

jpeg(file = "LOOCV-obs-js-2009.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.obs.jsv(comb.obs.jsv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-obs-js-2009-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.obs.jsv.t(comb.obs.jsv)
while (!is.null(dev.list()))  dev.off()

## using hebei(2012???2013,2014,2016) to predict 
#using hebei(2012???2013,2014,2016) to predict zhejiang-----------------------------
lst.obs.hb = list( hbvo.wood, hbvo.straw)
comb.obs.hbv <- Reduce(function(x,y) merge(x,y,all=T),lst.obs.hb)  ### combine the list
comb.obs.hbv <- obs_result_hbv(comb.obs.hbv)

jpeg(file = "LOOCV-obs-hb-zhejiang.jpg",width=600*4,height=3*600,res=72*2)   # by every type
plot.obs.hbv(comb.obs.hbv)
while (!is.null(dev.list()))  dev.off()

jpeg(file = "LOOCV-obs-hb-zhejiang-total.jpg",width=600*4,height=3*600,res=72*2)   # total 
plot.obs.hbv.t(comb.obs.hbv)
while (!is.null(dev.list()))  dev.off()











