#### neural network ####
#### forestfires #####

str(forestfires)
View(forestfires)
attach(forestfires)
forestfires1 <- forestfires

#Converting Salary column into factor
forestfires1$size_category[size_category ==" small"]<-0
forestfires1$size_category[size_category==" large"]<-1
forestfires1$size_category<-as.numeric(forestfires1$size_category)
forestfires1$size_category<-factor(forestfires1$size_category)



View(forestfires1)
table(forestfires1$size_category)



### normalize data ##
normalize <- function(x){
  return(x-min(x))/(max(x)-min(x))
}

View(forestfires1)
forestfires_norm <- as.data.frame(lapply(forestfires1[3:31],normalize))
View(forestfires_norm)



## creating training and testing data ##
forestfires_train <- forestfires_norm[1:330,]
forestfires_test <- forestfires_norm[331:517,]



## train a model on the data##
install.packages("neuralnet")
library("neuralnet")



### simple ANN with only a single hidden neuron ##
forestfires_model <- neuralnet(formula = size_category ~ FFMC+
                                 DMC + DC + ISI + temp + RH + wind + rain + area +
                                 dayfri + daymon + daysat + daysun + daythu + daytue + daywed +
                                 monthapr + monthaug + monthdec + monthfeb +
                                 monthjan + monthjul
                               + monthjun + monthmar + monthmay + monthnov + 
                                 monthoct + monthsep, 
                               data = forestfires_train )


## visulization the network topology ##
plot(forestfires_model)



## evaluating model performence ##
## obtain model result ##
forestfires_test[1:28]
results_model <- predict(forestfires_model,forestfires_test[1:28])
results_model




## obtain predicted strength value ##
str(results_model)
predicted_size_category <- results_model



## cor between predicted and actual value 
cor(predicted_size_category,forestfires_test$size_category)


## neural network topology with 2 hidden network
forestfires_model2 <- neuralnet(formula = size_category ~ FFMC+
                                  DMC + DC + ISI + temp + RH + wind + rain + area +
                                  dayfri + daymon + daysat + daysun + daythu + daytue + daywed +
                                  monthapr + monthaug + monthdec + monthfeb +
                                  monthjan + monthjul
                                + monthjun + monthmar + monthmay + monthnov + 
                                  monthoct + monthsep, 
                                data = forestfires_train , hidden = c(2,2),
                                algorithm = 'backprop',learningrate = 0.0001,
                                linear.output = F, stepmax = 1e+08, act.fct = 'tanh')


plot(forestfires_model2)

## cheacking accuracy ##


## evaluating model performence ##
## obtain model result ##
results_mode2 <- predict(forestfires_model2,forestfires_test[1:28])
results_mode2



## obtain predicted strength value ##
str(results_mode2)
predicted_size_category2 <- results_mode2



# cor between predicted and actual value 
cor(predicted_size_category2,forestfires_test$size_category)

