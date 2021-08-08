
## the functions to do variable selection

library(bestglm)

straw_choice <- function(df_straw_nzero){
  
    var_straw <- dplyr::select(as.data.frame(df_straw_nzero),
                              f_pop, f_size, 
                               f_inc, cr, f_land, Ex_coal, Ex_forest, 
                               Ex_hydro, Ex_road, y0_straw)

    var_straw <- na.omit(var_straw)
    set.seed(123) 
    ind <- sample(2, nrow(var_straw), replace = TRUE, prob = c(0.7, 0.3))
    train <- var_straw[ind==1, ] 
    test <- var_straw[ind==2, ] 
    full.fit <- glm(y0_straw ~ ., family = gaussian, data = train)
    test.values <- predict(full.fit, newdata = test)
    Xy <- train
    Xy<- plyr::rename(Xy, c("y0_straw"="y"))
    Bestglm.fit <- bestglm(Xy = Xy, IC = "CV", family=gaussian)
    Bestglm.fit <- bestglm(Xy = Xy, IC = "CV",CVArgs = list(Method = "HTF",
                                         K = 10, REP = 1), family=gaussian)
    plot(0:(dim(Xy)[2]-1),Bestglm.fit$Subsets$CV,type='b',
         xlab = "numbers of Features", ylab = "error",
         main = "error by Feature Inclusion")
    coef(Bestglm.fit$BestModel)  ##find the optimal variables
}


wood_choice <- function(df_wood_nzero){
  
    var_wood <- dplyr::select(as.data.frame(df_wood_nzero),
                              f_pop, f_size, 
                               f_inc, cr, f_land, Ex_coal, Ex_forest, 
                               Ex_hydro, Ex_road, y0_wood)

    var_wood <- na.omit(var_wood)
    set.seed(123) 
    ind <- sample(2, nrow(var_wood), replace = TRUE, prob = c(0.7, 0.3))
    train <- var_wood[ind==1, ] 
    test <- var_wood[ind==2, ] 
    full.fit <- glm(y0_wood ~ ., family = gaussian, data = train)
    test.values <- predict(full.fit, newdata = test)
    Xy <- train
    Xy<- plyr::rename(Xy, c("y0_wood"="y"))
    Bestglm.fit <- bestglm(Xy = Xy, IC = "CV", family=gaussian)
    Bestglm.fit <- bestglm(Xy = Xy, IC = "CV",CVArgs = list(Method = "HTF",
                                         K = 10, REP = 1), family=gaussian)
    plot(0:(dim(Xy)[2]-1),Bestglm.fit$Subsets$CV,type='b',
         xlab = "numbers of Features", ylab = "error",
         main = "error by Feature Inclusion")
    coef(Bestglm.fit$BestModel)  ##find the optimal variables
}
