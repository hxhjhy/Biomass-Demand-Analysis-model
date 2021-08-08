
####加载数据
df_straw_nzero$Ex_hydro <- log(df_straw_nzero$Ex_hydro)
df_straw_nzero$Ex_road <- log(df_straw_nzero$Ex_road)  
df_straw_nzero$Ex_forest <- log(df_straw_nzero$Ex_forest)    
df_straw_nzero$cr <- log(df_straw_nzero$cr)      
df_straw_nzero$f_inc <- log(df_straw_nzero$f_inc) 
df_straw_nzero$Ex_coal <- log(df_straw_nzero$Ex_coal) 
df_straw_nzero$y0_straw <- log(df_straw_nzero$y0_straw) 

var_straw <- dplyr::select(as.data.frame(df_straw_nzero),
                          f_pop, f_size, 
                           f_inc, cr, f_land, Ex_coal, Ex_forest, 
                           Ex_hydro, Ex_road, y0_straw)

var_straw <- na.omit(var_straw)

set.seed(123) 
ind <- sample(2, nrow(var_straw), replace = TRUE, prob = c(0.7, 0.3))
## 按照7:3切分数据集
train <- var_straw[ind==1, ] #得到训练集
test <- var_straw[ind==2, ] #得到测试集
full.fit <- glm(y0_straw ~ ., family = gaussian, data = train)

test.values <- predict(full.fit, newdata = test)

library(bestglm)
Xy <- train
Xy<- plyr::rename(Xy, c("y0_straw"="y"))

Bestglm.fit <- bestglm(Xy = Xy, IC = "CV", family=gaussian)



Bestglm.fit <- bestglm(Xy = Xy, IC = "CV",CVArgs = list(Method = "HTF",
                                     K = 10, REP = 1), family=gaussian)

#table(is.na(Xy))  


plot(0:(dim(Xy)[2]-1),Bestglm.fit$Subsets$CV,type='b',
     xlab = "numbers of Features", ylab = "error",
     main = "error by Feature Inclusion")


##画出变量数量和误差之间的关系
coef(Bestglm.fit$BestModel)  ##查看最终选择的最优变量



####加载数据
df_wood_nzero$Ex_hydro <- log(df_wood_nzero$Ex_hydro)
df_wood_nzero$Ex_road <- log(df_wood_nzero$Ex_road)  
df_wood_nzero$Ex_forest <- log(df_wood_nzero$Ex_forest)    
df_wood_nzero$cr <- log(df_wood_nzero$cr)      
df_wood_nzero$f_inc <- log(df_wood_nzero$f_inc) 
df_wood_nzero$Ex_coal <- log(df_wood_nzero$Ex_coal) 
df_wood_nzero$y0_wood <- log(df_wood_nzero$y0_wood) 

var_wood <- dplyr::select(as.data.frame(df_wood_nzero),
                           f_pop, f_size, 
                           f_inc, cr, f_land, Ex_coal, Ex_forest, 
                           Ex_hydro, Ex_road, y0_wood)

var_wood <- na.omit(var_wood)

set.seed(1234) 
ind <- sample(2, nrow(var_wood), replace = TRUE, prob = c(0.7, 0.3))
## 按照7:3切分数据集
train <- var_wood[ind==1, ] #得到训练集
test <- var_wood[ind==2, ] #得到测试集
full.fit <- glm(y0_wood ~ ., family = gaussian, data = train)

test.values <- predict(full.fit, newdata = test)

library(bestglm)
Xy <- train
#Xy<- plyr::rename(Xy, c("y0_wood"="y"))

Bestglm.fit <- bestglm(Xy = Xy, IC = "CV",CVArgs = list(Method = "HTF",
                                                        K = 10, REP = 1), family=gaussian)
Bestglm.fit2 <- bestglm(Xy = Xy, IC = "CV", family=gaussian)

#table(is.na(Xy))  


plot(0:(dim(Xy)[2]-1),Bestglm.fit2$Subsets$CV,type='b',
     xlab = "numbers of Features", ylab = "error",
     main = "error by Feature Inclusion")


##画出变量数量和误差之间的关系
coef(Bestglm.fit$BestModel)  ##查看最终选择的最优变量





corr.test(var_wood, adjust = "none", use = "complete")

library(psych)




mean(df_wood_nzero$y0_wood)















