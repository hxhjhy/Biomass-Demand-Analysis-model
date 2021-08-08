
fitted_value <- function(data, energy.inla){
    
    pred_value <- (round(energy.inla$summary.fitted.values$mean,3))
    data$pred_value <- exp(pred_value)
    data$pred_value.sd <- round(energy.inla$summary.fitted.values$sd,3)
    data$pred_value.low <- exp(pred_value - 1.96 * data$pred_value.sd)
    data$pred_value.high <- exp(pred_value + 1.96 * data$pred_value.sd)
    #data$pred_value.low <- exp(round(energy.inla$summary.fitted.values$`0.025quant`,3))
    #data$pred_value.high <-exp(round(energy.inla$summary.fitted.values$`0.975quant`,3))

    result_ene <- filter(data,pre == 1) %>%
      dplyr::select(year, province, pred_value, pred_value.low, pred_value.high, 
                    pred_value.sd,hhold)
  
    result_ene$total <- with(result_ene, pred_value * hhold/10000000)
    result_ene$total.low <- with(result_ene, pred_value.low * hhold/10000000)
    result_ene$total.high <- with(result_ene, pred_value.high * hhold/10000000)

    result_ene2 <-dplyr::select(data.frame(result_ene),year,total,total.low, total.high)
    result_province <-dplyr::select(data.frame(result_ene),year,province,total,total.low, total.high, pred_value.sd)
    ### dajust by province
    result_ene <- wood_ajs(result_ene2)
    national <- result_ene2 %>% 
      group_by(year) %>%
      summarise_each(funs = sum) 
    
    result <- list(result_province,national)
}  


fitted_ele <- function(group_ele, energy.inla){
  group_ele$pred_ele <- exp(round(energy.inla$summary.fitted.values$mean,3))
  group_ele$ele_res <- exp(round(energy.inla$summary.fitted.values$sd,3))
  #group_ele[which(group_ele$ele_res > 0.5), "ele_res"] <- 0.5
  #group_ele[which(group_ele$year >2013), "ele_res"] <- 0.25
  #group_ele[which(group_ele$year %in% c(2006,2007,2009)), "ele_res"] <- 0.3
  #group_ele[which(group_ele$year %in% c(2008)), "ele_res"] <- 0.2
  group_ele$pred_ele.low <- with(group_ele, pred_ele - 2*ele_res)
  group_ele$pred_ele.high <- with(group_ele, pred_ele + 2*ele_res)

  result_ele <- filter(group_ele,pre == 1) %>%
    dplyr::select(year, province, pred_ele, pred_ele.low, pred_ele.high, hhold, ele_res)
  
  result_ele$total_ele <- with(result_ele, pred_ele * hhold/10000000)
  result_ele$total_ele.low <- with(result_ele, pred_ele.low * hhold/10000000)
  result_ele$total_ele.high <- with(result_ele, pred_ele.high * hhold/10000000)
  #result_ele$total_ele.high <- with(result_ele, pred_ele.high * hhold/10000000)
  result_ele2 <-dplyr::select(data.frame(result_ele),year,total_ele,total_ele.low, total_ele.high)
  result_province <-dplyr::select(data.frame(result_ele),year,province,total_ele,total_ele.low, total_ele.high, ele_res)
  
  national_ele <- result_ele2 %>% 
    group_by(year) %>%
    summarise_each(funs = sum)
  result <- list(result_province,national_ele)
} 
   
fitted_biogas <- function(group_biogas, energy.inla){
  group_biogas$pred_biogas <- exp(round(energy.inla$summary.fitted.values$mean,3))
  group_biogas$biogas_res <- exp(round(energy.inla$summary.fitted.values$sd,3))
  #group_biogas[which(group_biogas$biogas_res > 1), "biogas_res"] <- 0.7
  
  group_biogas$pred_biogas.low <- with(group_biogas, pred_biogas - 2 * biogas_res)
  group_biogas$pred_biogas.high <- with(group_biogas, pred_biogas + 2 * biogas_res)
  group_biogas <- filter(group_biogas, pred_biogas.low > 0)
  #group_biogas$pred_biogas.low <- exp(round(m_biogas_1$summary.fitted.values$`0.025quant`,3))
  #group_biogas$pred_biogas.high <-exp(round(m_biogas_1$summary.fitted.values$`0.975quant`,3))
  #res <-  exp(round(m_biogas_1$summary.fitted.values$`0.5quant`,3))
  result_biogas <- filter(group_biogas,pre == 1) %>%
    dplyr::select(year, province, pred_biogas, pred_biogas.low, pred_biogas.high, hhold, biogas_res)
  
  
  summary(lm(group_biogas$pred_biogas~group_biogas$y0_biogas))
  
  result_biogas$total_biogas <- with(result_biogas, pred_biogas * hhold/10000000)
  result_biogas$total_biogas.low <- with(result_biogas, pred_biogas.low * hhold/10000000)
  result_biogas$total_biogas.high <- with(result_biogas, pred_biogas.high * hhold/10000000)
  result_biogas2 <- dplyr::select(data.frame(result_biogas),year,total_biogas,total_biogas.low, total_biogas.high)
  result_province <- dplyr::select(data.frame(result_biogas),year,province,total_biogas,total_biogas.low, total_biogas.high)
 
  national_biogas <- result_biogas2 %>% 
  group_by(year) %>%
  summarise_each(funs = sum) 
  
  result <- list(result_province,national_biogas)
  
}
  
  





