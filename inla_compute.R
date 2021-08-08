
## Model compute--------------------------------------------------

ene.inla <- function(name, formula_num, data){     #using formula_num to select model
  
  y <- data[paste("y0",name,sep = "_")]
  data$y <- y
  data$pro2 <- data$pro
  if (name == "wood") {formula = model.selection.wood(formula_num)}
  else ( formula = model.selection.straw(formula_num))
  ene <- inla(formula, data = data,  
                 family = "gaussian",
                 control.predictor = list(compute = T),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))      
}








