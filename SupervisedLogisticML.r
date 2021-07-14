###Tutorial logistic regression in R###
#https://www.geeksforgeeks.org/logistic-regression-in-r-programming/
library(dplyr)

summary(mtcars)

#Performing Logistic regression on dataset
#Logistic regression is implemented in R using glm() by training the model using features or variables in the dataset.

install.packages("ROCR")
install.packages("caTools")

library(ROCR)
library(caTools)

split <- sample.split(mtcars, SplitRatio = 0.8)
split

train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")

logistic_model = glm(vs ~ wt + disp, data = train_reg, family = "binomial")

# Predict test data based on model 
predict_reg = predict(logistic_model, test_reg, type="response")

predict_reg

# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)


# Evaluating model accuracy
# using confusion matrix
table(test_reg$vs, predict_reg)

missing_classerr <- mean(predict_reg != test_reg$vs)
print(paste('Accuracy =', 1 - missing_classerr))


#ROC Curve
ROCPred <- prediction(predict_reg, test_reg$vs) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")


auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)