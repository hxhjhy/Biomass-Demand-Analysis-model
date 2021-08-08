### five-folds to cross-validation
valid_cross_test <- function(name, valid_group,ene_num){
  response = paste("y0",name, sep = "_")
  switch(ene_num, set.seed(21), set.seed(22))
  valid_group <- valid_group[which(!is.na(valid_group[,response])),]
  data_idx = sample(1:nrow(valid_group), size = 0.8 * nrow(valid_group)) ## train data is 80%
  valid_group$link <- NA
  trn_data = valid_group[data_idx, ]
  valid_group[-data_idx,"link"] <- 1  ### the conterpart of the row of data_idx is to predict
  model.inla <- ene.inla(name, 6, valid_group)
  pred_value <- na.omit((round(model.inla$summary.fitted.values$mean,3)))[-data_idx]
  true_value <- as.matrix(valid_group[which(valid_group$link == 1),response])[,]
  data <- data.frame(pred_value = pred_value, true_value = true_value)
  MAPE<- MAPE(data)
  RMSE <- RMSE(data)
  ls <- list(MAPE,RMSE, summary(lm(pred_value~true_value, data = data))$r.squared, model.inla)
}

#summary results of annually iterated or province iterated cross-validation for biomass consumption
valid_test <- function(name, ene, valid_group){
  list = c(as.matrix(unique(valid_group[name])))  #year or pro
  lst <- list()
  i = 0
  for (n in list){
    i = i + 1 
    data <- valid_group
    data$link <- NA
    response = paste("y0",ene, sep = "_")
    data[which(data[name] == n),response] <- NA
    data[which(data[name] == n),"link"] <- 1    #need to predict
    m.inla <- ene.inla(ene,6,data)
    pred_value <- na.omit((round(m.inla$summary.fitted.values$mean,3))[data$link])
    lst[[i]] <- data.frame(pred = pred_value, 
                           true = valid_group[which(valid_group[name] == n),response],
                           flag = data[which(data[name] == n),name])
  }
  group_v <- Reduce(function(x,y) merge(x,y,all=T),lst)  ### combine the list
}

                    
valid_machine_test <- function(name, data, ene_num){   ### compare with other machine learning methods

  data.m <- as_tibble(as.data.frame(data)) 
  data.m <- switch(ene_num, 
                   select(data.m, y0_wood,  
                         year,pro,H_pop, 
                         H_inc, P_coal, P_forest, P_land),
                   select(data.m, y0_straw,  
                          year,pro,H_pop,H_inc, P_straw, P_land, P_coal, P_forest, 
                          P_road))

  data.m <- na.omit(data.m)
  response = paste("y0",name, sep = "_")
  regr.task = makeRegrTask(data = data.m, target = response)
  print(regr.task)
  
  glm <- makeLearner("regr.glm")
  svm <- makeLearner("regr.svm")
  rf <-  makeLearner("regr.randomForest",par.vals = list("ntree" = 3)) # 5, 3
  
  glm.model = train(learner = glm, task = regr.task)
  svm.model = train(learner = svm, task = regr.task)
  rf.model = train(learner = rf, task = regr.task)
  
  glm.predict <- predict(glm.model, task = regr.task)
  svm.predict <- predict(svm.model, task = regr.task)
  rf.predict <- predict(rf.model, task = regr.task)
  
  glm.pred.data <- select(glm.predict$data, -id)
  svm.pred.data <- select(svm.predict$data, -id)
  rf.pred.data <- select(rf.predict$data, -id)
  
  ls.glm <- list(MAPE(glm.pred.data),
             RMSE(glm.pred.data),
             summary(lm(truth~response, data = glm.pred.data))$r.squared)
  
  ls.svm <- list(MAPE(svm.pred.data),
                 RMSE(svm.pred.data),
                 summary(lm(truth~response, data = svm.pred.data))$r.squared)
 
   ls.rf <- list(MAPE(rf.pred.data),
                 RMSE(rf.pred.data),
                 summary(lm(truth~response, data = rf.pred.data))$r.squared)

  result <- list(ls.glm, ls.svm, ls.rf)
}

MAPE <- function(x){  
  #abs(mean((pred - true)/true))
  abs(mean((x$pred_value - x$true_value)/x$true_value))
}

RMSE <- function(x){  
  sqrt(mean((x$pred_value - x$true_value)^2))
}





