plot.group.year <- function(comb.group.yv){ 
  cols = c("#336699", "#99CCFF", "#009966", "#FF9900", "#CC0033")
  
  p <- ggplot(comb.group.yv,aes(x = true,y = pred)) + 
    geom_point(aes(colour = year),size = 4, shape = 15) + 
    scale_color_gradientn(colours = cols) + facet_wrap(~group.flag) + 
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
