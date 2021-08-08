MAPE <- function(x){  
  #abs(mean((pred - true)/true))
  abs(mean((x$pred_value - x$true_value)/x$true_value))
}

RMSE <- function(x){  
  sqrt(mean((x$pred_value - x$true_value)^2))
}

### five-folds to cross-validation
valid_cross_test <- function(name, valid_group,ene_num){
  response = paste("y0",name, sep = "_")
  #switch(ene_num, set.seed(121), set.seed(124), set.seed(33), set.seed(44))
  valid_group <- valid_group[which(!is.na(valid_group[,response])),]
  #data_idx = sample(1:nrow(valid_group), size = 0.8 * nrow(valid_group)) ## train data is 80%
  #valid_group$link <- NA
  #trn_data = valid_group[data_idx, ]
  #valid_group[-data_idx,"link"] <- 1  ### the conterpart of the row of data_idx is to predict
  model.inla <- ene.inla(name, 6, valid_group)
  #pred_value <- na.omit((round(model.inla$summary.fitted.values$mean,3)))[-data_idx]
  pred_value <- na.omit((round(model.inla$summary.fitted.values$mean,3)))
  true_value <- as.matrix(valid_group[,response])[,]
  #true_value <- as.matrix(valid_group[which(valid_group$link == 1),response])[,]
  data <- data.frame(pred_value = pred_value, true_value = true_value)
  MAPE<- MAPE(data)
  RMSE <- RMSE(data)
  ls <- list(MAPE,RMSE, summary(lm(pred_value~true_value, data = data))$r.squared, model.inla)
}

MAPE.m <- function(x){  
  #abs(mean((pred - true)/true))
  abs(mean((x$response - x$truth)/x$truth))
}

RMSE.m <- function(x){  
  sqrt(mean((x$response - x$truth)^2))
}


valid_machine_test <- function(name, data, ene_num){

  data.m <- as_tibble(as.data.frame(data)) 
  data.m <- switch(ene_num, 
                   select(data.m, y0_wood,  
                         year,pro,f_pop, f_size, 
                         f_inc, cr, f_land, Ex_coal, Ex_forest, 
                         Ex_hydro, Ex_road),
                   select(data.m, y0_straw,  
                          year,pro,f_pop, f_size, 
                          f_inc, cr, f_land, Ex_coal, Ex_forest, 
                          Ex_hydro, Ex_road),
                   select(data.m, y0_biogas,  
                          year,pro,f_pop, f_size, 
                          f_inc, cr, f_land, Ex_coal, Ex_forest, 
                          Ex_hydro, Ex_road),
                   select(data.m, y0_ele,  
                          year,pro,f_pop, f_size, 
                          f_inc, cr, f_land, Ex_coal, Ex_forest, 
                          Ex_hydro, Ex_road))

  data.m <- na.omit(data.m)
  
  response = paste("y0",name, sep = "_")
  
  regr.task = makeRegrTask(data = data.m, target = response)
  
  print(regr.task)
  
  glm <- makeLearner("regr.glm")
  svm <- makeLearner("regr.svm")
  rf <-  makeLearner("regr.randomForest",par.vals = list("ntree" = 3)) # 5, 3
  #print(rf$par.set)
  #nnet <- makeLearner("regr.nnet")  #Neural Network
  #cforest <-  makeLearner("regr.cforest")
  
  glm.model = train(learner = glm, task = regr.task)
  svm.model = train(learner = svm, task = regr.task)
  rf.model = train(learner = rf, task = regr.task)
  #rf.o.model = train(learner = cforest, task = regr.task)
  
  glm.predict <- predict(glm.model, task = regr.task)
  svm.predict <- predict(svm.model, task = regr.task)
  rf.predict <- predict(rf.model, task = regr.task)
  #rf.o.predict <- predict( rf.o.model, task = regr.task)
  
  glm.pred.data <- select(glm.predict$data, -id)
  svm.pred.data <- select(svm.predict$data, -id)
  rf.pred.data <- select(rf.predict$data, -id)
  
  ls.glm <- list(MAPE.m(glm.pred.data),
             RMSE.m(glm.pred.data),
             summary(lm(truth~response, data = glm.pred.data))$r.squared)
  
  ls.svm <- list(MAPE.m(svm.pred.data),
                 RMSE.m(svm.pred.data),
                 summary(lm(truth~response, data = svm.pred.data))$r.squared)
 
   ls.rf <- list(MAPE.m(rf.pred.data),
                 RMSE.m(rf.pred.data),
                 summary(lm(truth~response, data = rf.pred.data))$r.squared)

  result <- list(ls.glm, ls.svm, ls.rf)
}

listLearners()$class

# ## »®·ÖÑù±¾¼¯
# data_idx = sample(1:nrow(group_wood.m), size = 0.8 * nrow(group_wood.m)) ## train data is 80%
# trn_data = group_wood.m[data_idx, ]
# tst_data = group_wood.m[-data_idx, ]


# # Make a task
#   response = paste("y0",name, sep = "_")
#   regr.task = makeRegrTask(data = data, target = response)
#   print(regr.task)
#   
#   set.seed(1234)
#   # Define a search space for each learner'S parameter
#   # ps_ksvm = makeParamSet(
#   #   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
#   # )
#   # 
#   # ps_rf = makeParamSet(
#   #   makeIntegerParam("num.trees", lower = 1L, upper = 200L)
#   # )
#   
#   # Choose a resampling strategy
#   rdesc = makeResampleDesc("CV", iters = 5L)
#   
#   # Choose a performance measure
#   meas = rmse
#   
#   # Choose a tuning method
#   #ctrl = makeTuneControlCMAES(budget = 100L)
#   
#   # Make tuning wrappers
#   # tuned.ksvm = makeTuneWrapper(learner = "regr.ksvm", resampling = rdesc, measures = meas,
#   #                              par.set = ps_ksvm, control = ctrl, show.info = FALSE)
#   # tuned.rf = makeTuneWrapper(learner = "regr.ranger", resampling = rdesc, measures = meas,
#   #                            par.set = ps_rf, control = ctrl, show.info = FALSE)
# 
# # Four learners to be compared
# lrns = list(makeLearner("regr.glm"), tuned.ksvm, tuned.rf)
# 
# # Conduct the benchmark experiment
# bmr = benchmark(learners = lrns, tasks = regr.task, resamplings = rdesc, measures = rmse, 
#                 show.info = FALSE)
# 
# getBMRAggrPerformances(bmr)








