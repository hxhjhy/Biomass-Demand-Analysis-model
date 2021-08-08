plot.group.year <- function(comb.group.yv){ 
  cols = c("#336699", "#99CCFF", "#009966", "#FF9900", "#CC0033")
  
  p <- ggplot(comb.group.yv,aes(x = true,y = pred)) + 
    geom_point(aes(colour = year),size = 4, shape = 15) + 
    scale_color_gradientn(colours = cols) + facet_wrap(~obs.flag) + 
    geom_abline(intercept= 0, slope=1, size = 1.3,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1.7, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    theme( axis.text = element_text( size = 24 ),
           axis.text.x = element_text( size = 24),
           axis.title = element_text( size = 26), #face = "bold"
           panel.grid.minor = element_blank(),
           legend.title = element_text(size=10),
           legend.background = element_rect(colour = 'grey'),
           strip.text = element_text(size = 20),
           #strip.background = element_rect(fill = NA, colour = NA),
           #Panel.background = element_rect(fill = NA, colour = NA),
           legend.position = c(1.05,0.5)) +
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) +
    labs(x= "true_value(log)",y = "predict_value(log)")
  
  #构???注释数据框+ theme_bw() 
  library(ggpubr)
  f_labels <- data.frame(obs.flag = c("straw", "wood"), 
                         N = c(paste("N", "== 71", sep= ""),
                               paste("N", "== 93", sep= "")), 
                         formula = c('Predicted ~ 0.86*Observed+1.04',
                                     'Predicted ~ 0.75*Observed+1.57'),
                         R2 = c(paste("R^2", "== 0.64", sep= ""),
                                paste("R^2", "== 0.62", sep= "")),
                         RMSE = c(paste("RMSE", "== 0.46", sep= ""),
                                  paste("RMSE", "== 0.39", sep= "")),
                         MAPE = c(paste("MAPE", "= 0.69%", sep= " "),
                                  paste("MAPE", "= 1.19%", sep= " ")))
  
  g_plot <- p + geom_text(x=1.79, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 6) +   
    geom_text(x=1.47, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 6) + 
    geom_text(x=3.28, y= 8, aes(label=formula),       
              data=f_labels, size = 6) +                         # fontface = 'italic'
    geom_text(x=1.3, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 6) +                        # fontface = 'italic'
    geom_text(x=1.9, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 6) + ggtitle("LOOCV-group-year")
  g_plot
  
} 


plot.group.year.t <- function(comb.group.yv){ 
  cols = c("#336699", "#99CCFF", "#009966", "#FF9900", "#CC0033")
  
  p <- ggplot(comb.group.yv,aes(x = true,y = pred)) + 
    geom_point(aes(colour = year),size = 4, shape = 15) + 
    scale_color_gradientn(colours = cols)  + 
    geom_abline(intercept= 0, slope=1, size = 1.3,col = "black") +
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1.7, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    theme( axis.text = element_text( size = 34 ),
           axis.text.x = element_text( size = 34 ),
           axis.title = element_text( size = 36),
           panel.grid.minor = element_blank(),
           legend.title = element_text(size=20),
           strip.text = element_text(size = 20)) +
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) +
    labs(x= "true_value(log)",y = "predict_value(log)")
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame( N = paste("N", "== 208", sep= ""), 
                          formula = 'Predicted ~ 0.91*Observed+0.54',
                          R2 = paste("R^2", "== 0.80", sep= ""),
                          RMSE = paste("RMSE", "== 0.44", sep= ""),
                          MAPE = paste("MAPE", "= 0.16%", sep= " ")) 
  
  
  g_plot <- p + geom_text(x=1.11, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 8) +    
    geom_text(x=0.9, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 8) + 
    geom_text(x=2, y= 8, aes(label=formula),       
              data=f_labels, size = 8) +                         # fontface = 'italic'
    geom_text(x=0.8, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 8) +                        # fontface = 'italic'
    geom_text(x=1.2, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 8) + ggtitle("LOOCV-group-year-total")
  g_plot
  
} 


plot.group.pro <- function(comb.group.pv){ 
  cols = c("#336699", "#99CCFF", "#009966", "#FF9900", "#CC0033")
  
  p <- ggplot(comb.group.pv,aes(x = true,y = pred)) + 
    geom_point(aes(colour = pro),size = 4, shape = 15) + 
    scale_color_gradientn(colours = cols) + facet_wrap(~group.flag) + 
    geom_abline(intercept= 0, slope=1, size = 1.3,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1.7, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    theme( axis.text = element_text( size = 24 ),
           axis.text.x = element_text( size = 24 ),
           axis.title = element_text( size = 26),
           panel.grid.minor = element_blank(),
           legend.title = element_text(size=15),
           legend.background = element_rect(colour = 'grey'),
           strip.text = element_text(size = 20)) +
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) +
    labs(x= "true_value(log)",y = "predict_value(log)")
  

  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame(group.flag = c("straw", "wood"), 
                         N = c(paste("N", "== 71", sep= ""),
                               paste("N", "== 93", sep= "")), 
                         formula = c('Predicted ~ 0.67*Observed+2.16',
                                     'Predicted ~ 0.84*Observed+1.23'),
                         R2 = c(paste("R^2", "== 0.55", sep= ""),
                                paste("R^2", "== 0.49", sep= "")),
                         RMSE = c(paste("RMSE", "== 0.51", sep= ""),
                                  paste("RMSE", "== 0.53", sep= "")),
                         MAPE = c(paste("MAPE", "= 1.65%", sep= " "),
                                  paste("MAPE", "= 1.82%", sep= " ")))
  
  g_plot <- p + geom_text(x=1.75, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 6) +   
    geom_text(x=1.45, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 6) + 
    geom_text(x=3.4, y= 8, aes(label=formula),       
              data=f_labels, size = 6) +                         # fontface = 'italic'
    geom_text(x=1.25, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 6) +                        # fontface = 'italic'
    geom_text(x=1.95, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 6) + ggtitle("LOOCV-group-province")
  g_plot
  
} 

plot.group.pro.t <- function(comb.group.pv){ 
  cols = c("#336699", "#99CCFF", "#009966", "#FF9900", "#CC0033")
  
  p <- ggplot(comb.group.pv,aes(x = true,y = pred)) + 
    geom_point(aes(colour = pro),size = 4, shape = 15) + 
    scale_color_gradientn(colours = cols) + 
    geom_abline(intercept= 0, slope=1, size = 1.3,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1.7, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    theme( axis.text = element_text( size = 34 ),
           axis.text.x = element_text( size = 34 ),
           axis.title = element_text( size = 36),
           panel.grid.minor = element_blank(),
           legend.title = element_text(size=20),
           strip.text = element_text(size = 20)) +
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) +
    labs(x= "true_value(log)",y = "predict_value(log)")
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame( N = paste("N", "== 208", sep= ""), 
                          formula = 'Predicted ~ 0.89*Observed+0.64',
                          R2 = paste("R^2", "== 0.77", sep= ""),
                          RMSE = paste("RMSE", "== 0.48", sep= ""),
                          MAPE = paste("MAPE", "= 0.74%", sep= " ")) 
  
  
  g_plot <- p + geom_text(x=1.11, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 8) +    
    geom_text(x=0.9, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 8) + 
    geom_text(x=2, y= 8, aes(label=formula),       
              data=f_labels, size = 8) +                         # fontface = 'italic'
    geom_text(x=0.8, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 8) +                        # fontface = 'italic'
    geom_text(x=1.2, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 8) + ggtitle("LOOCV-group-province-total")
  g_plot
  
} 

plot.obs.jsv <- function(comb.obs.jsv){ 
  col = c("#009933","#0099CC")
  p <- ggplot(comb.obs.jsv,aes(x = true,y = pred_js)) + 
    geom_point(aes(col = group.flag),size = 1.5, shape = 15) + 
    facet_wrap(~group.flag) + 
    geom_abline(intercept= 0, slope=1, size = 1,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) + theme_bw() 
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame(group.flag = c("obs.straw", "obs.wood"), 
                         N = c(paste("N", "== 55", sep= ""), 
                               paste("N", "== 32", sep= "")), 
                         formula = c('Predicted ~ 0.69*Observed+1.35',
                                     'Predicted ~ 0.62*Observed+2.15'),
                         R2 = c(paste("R^2", "== 0.38", sep= ""), 
                                paste("R^2", "== 0.59", sep= "")),
                         RMSE = c(paste("MRSE", "== 1.19", sep= ""), 
                                  paste("MRSE", "== 0.93", sep= "")),
                         MAPE = c(paste("MAPE", "= 11.76%", sep= " "),
                                  paste("MAPE", "= 7.66%", sep= " ")))
  
  g_plot <- p + geom_text(x=1.55, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 5) +   
    geom_text(x=1.35, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 5) + 
    geom_text(x=2.4, y= 8, aes(label=formula),       
              data=f_labels, size = 5) +                         # fontface = 'italic'
    geom_text(x=1.15, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 5) +                        # fontface = 'italic'
    geom_text(x=1.65, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 5) + ggtitle("validation jiangsu 20009")
  g_plot
  
} 

plot.obs.jsv.t <- function(comb.obs.jsv){ 
  col = c("#009933","#0099CC")
  p <- ggplot(comb.obs.jsv,aes(x = true,y = pred_js)) + 
    geom_point(aes(col = group.flag),size = 1.5, shape = 15) + 
    geom_abline(intercept= 0, slope=1, size = 1,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) + theme_bw() 
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame( N = paste("N", "== 87", sep= ""), 
                          formula = 'Predicted ~ 0.57*Observed+2.26',
                          R2 = paste("R^2", "== 0.59", sep= ""),
                          RMSE = paste("MRSE", "== 1.11", sep= ""),
                          MAPE = paste("MAPE", "= 4.61%", sep= " "))
  
  g_plot <- p + geom_text(x=1.55, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 5) +   
    geom_text(x=1.35, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 5) + 
    geom_text(x=2.4, y= 8, aes(label=formula),       
              data=f_labels, size = 5) +                         # fontface = 'italic'
    geom_text(x=1.15, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 5) +                        # fontface = 'italic'
    geom_text(x=1.65, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 5) + ggtitle("validation jiangsu 20009")
  g_plot
  
} 


plot.obs.hbv <- function(comb.obs.hbv){ 
  col = c("#009933","#0099CC")
  p <- ggplot(comb.obs.hbv,aes(x = true,y = pred_js)) + 
    geom_point(aes(col = obs.flag),size = 1.5, shape = 15) + 
    facet_wrap(~obs.flag) + 
    geom_abline(intercept= 0, slope=1, size = 1,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) + theme_bw() 
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame(obs.flag = c("obs.straw", "obs.wood"), 
                         N = c(paste("N", "== 46", sep= ""), 
                               paste("N", "== 51", sep= "")), 
                         formula = c('Predicted ~ 0.52*Observed+2.53',
                                     'Predicted ~ 0.79*Observed+0.85'),
                         R2 = c(paste("R^2", "== 0.45", sep= ""), 
                                paste("R^2", "== 0.75", sep= "")),
                         RMSE = c(paste("MRSE", "== 0.64", sep= ""), 
                                  paste("MRSE", "== 0.75", sep= "")),
                         MAPE = c(paste("MAPE", "= 1.51%", sep= " "),
                                  paste("MAPE", "= 7.05%", sep= " ")))
  
  g_plot <- p + geom_text(x=1.55, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 5) +   
    geom_text(x=1.35, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 5) + 
    geom_text(x=2.4, y= 8, aes(label=formula),       
              data=f_labels, size = 5) +                         # fontface = 'italic'
    geom_text(x=1.15, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 5) +                        # fontface = 'italic'
    geom_text(x=1.65, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 5) + ggtitle("hebei to predict zhejiang")
  g_plot
  
} 

plot.obs.hbv.t <- function(comb.obs.jsv){ 
  col = c("#009933","#0099CC")
  p <- ggplot(comb.obs.hbv,aes(x = true,y = pred_js)) + 
    geom_point(aes(col = obs.flag),size = 1.5, shape = 15) + 
    geom_abline(intercept= 0, slope=1, size = 1,col = "black") + 
    stat_smooth(method=lm, fullrange=T,linetype = "dashed", 
                se = FALSE,size=1, xseq = seq(-1,12, length=80),
                col = "#FF0033") + 
    coord_fixed(xlim = c(0, 10),ylim = c(0, 10)) + theme_bw() 
  
  
  #构???注释数据框
  library(ggpubr)
  f_labels <- data.frame( N = paste("N", "== 97", sep= ""), 
                          formula = 'Predicted ~ 0.68*Observed+1.55',
                          R2 = paste("R^2", "== 0.65", sep= ""),
                          RMSE = paste("MRSE", "== 0.69", sep= ""),
                          MAPE = paste("MAPE", "= 4.42%", sep= " "))
  
  g_plot <- p + geom_text(x=1.5, y=9, aes(label= RMSE), parse = T, 
                          data=f_labels, size = 5) +   
    geom_text(x=1.3, y=9.5, aes(label= R2), parse = T, 
              data=f_labels, size = 5) + 
    geom_text(x=2.2, y= 8, aes(label=formula),       
              data=f_labels, size = 5) +                         # fontface = 'italic'
    geom_text(x=1.15, y=10, aes(label= N),parse = T,      # parse is related with ?plotmath
              data=f_labels, size = 5) +                        # fontface = 'italic'
    geom_text(x=1.6, y=8.5, aes(label= MAPE),      # parse is related with ?plotmath
              data=f_labels, size = 5) + ggtitle("validation jiangsu 20009")
  g_plot
  
} 