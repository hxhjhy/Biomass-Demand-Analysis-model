
## Model compute--------------------------------------------------

ene.inla <- function(name, formula_num, data){     #using name to select model
  data <- data%>% rename(y = exp(paste("y0",name,sep = "_")))

  data$pro2 <- data$pro
  formula <- switch(formula_num, formula_1, formula_2, formula_3, formula_4,
                    formula_5, formula_6)
  ene <- inla(formula, data = data,  
                 family = "gaussian",
                 control.predictor = list(compute = T),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))      
}










