# This function is to predict the biomass consumption for province and national country


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

    result_ene <-dplyr::select(data.frame(result_ene),year,total,total.low, total.high)
    result_province <-dplyr::select(data.frame(result_ene),year,province,total,total.low, total.high, pred_value.sd)
    national <- result_ene %>% 
      group_by(year) %>%
      summarise_each(funs = sum) 
    result <- list(result_province,national)
}  
