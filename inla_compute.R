
## Model compute--------------------------------------------------

ene.inla <- function(name, formula_num, data){     #using formula_num to select model
  
  y <- data[paste("y0",name,sep = "_")]
  data$y <- y
  data$pro2 <- data$pro
  formula <- model.selection(formula_num)
  ene <- inla(formula, data = data,  
                 family = "gaussian",
                 control.predictor = list(compute = T),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))      
}










