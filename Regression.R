

# Logistic regression
# This time we use other data such as sentiments as well

logit_fit <- glm(weekend_label~., family = binomial(link = "logit"), data = training_df)
summary(logit_fit)

#Backwards and forwards automatic model selection to select optimal features (too slow, because too many features)
#step(logit_fit, data=training_df, direction="both")
#
#Optimal model according to automatic model selection:
#logit_fit = glm(formula = weekend_label ~ to + have + so + the + on + thx + 
#      print + i + today + when + h + talk + sid + what + tomorrow + 
#      this + let + him + as + schedul + your + did + good + body_length, 
#    family = binomial(link = "logit"), data = training_df)
#summary(logit_fit)

logit_pred <- predict(logit_fit, newdata= testing_df, type = "response")
logit_pred = as.data.frame(logit_pred)
logit_pred <- logit_pred %>% mutate(choice=ifelse(logit_pred>=0.51, 1, 0))
x = logit_pred$choice
y = testing_df$weekend_label
CrossTable(x, y, prop.chisq=FALSE)

#Calculate accuracy rate on test set
accuracy = 0
conf = table(x,y)
for(i in 1:2)
{
  accuracy = accuracy + conf[i,i]
}
accuracy = accuracy/length(x)
accuracy #0.6030151. A bit better than Naive Bayes, but still much lower than baseline


##
# Logistic regression 
# WITHOUT WORD COUNT: ONLY SENTIMENTS AND EMAIL LENGTH 
##
logit_fit <- glm(weekend_label~body_length+subject_length+positive_sent+negative_sent+anger_sent+anticipation_sent+disgust_sent+fear_sent+joy_sent+sadness_sent+surprise_sent+trust_sent, family = binomial(link = "logit"), data = training_df)
summary(logit_fit)

#Backwards and forwards automatic model selection to select optimal features
step(logit_fit, data=training_df, direction="both")

#Optimal model according to automatic model selection:
logit_fit = glm(formula = weekend_label ~ subject_length + negative_sent + 
                  anticipation_sent + trust_sent, family = binomial(link = "logit"), 
                data = training_df)
summary(logit_fit)

logit_pred <- predict(logit_fit, newdata= testing_df, type = "response")
logit_pred = as.data.frame(logit_pred)
logit_pred <- logit_pred %>% mutate(choice=ifelse(logit_pred>=0.51, 1, 0))
x = logit_pred$choice
y = testing_df$weekend_label
CrossTable(x, y, prop.chisq=FALSE)

#Calculate accuracy rate on test set
accuracy = 0
conf = table(x,y)
for(i in 1:2)
{
  accuracy = accuracy + conf[i,i]
}
accuracy = accuracy/length(x)
accuracy #0.6314908. Accuracy is a bit better, this time very close to the baseline


# SVM


library(e1071)

set.seed(1)

#Cross-validation
tune_res =tune(svm, weekend_label~., kernel ="radial", scale=TRUE, data = training_df,
               ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
summary(tune_res)#Output: best cost parameter is 1

#We do cross-validation again to see if there is a better parameter around 1
tune_res =tune(svm, weekend_label~., kernel ="radial", scale=TRUE, data = training_df,
               ranges=list(cost=c(0.2, 0.4,0.6,0.8,1,1.2,1.5,2,3,4,5) ))
summary(tune_res)#Output: best cost parameter is still 1

fit_svm <- svm(weekend_label~., kernel ="radial", cost=1,scale=TRUE, data = training_df)
summary(fit_svm)


predicted <- predict(fit_svm, testing_df)

x = predicted
y = testing_df$weekend_label
CrossTable(x, y, prop.chisq=FALSE)


#Calculate accuracy rate on test set
accuracy = 0
conf = table(x,y)
for(i in 1:2)
{
  accuracy = accuracy + conf[i,i]
}
accuracy = accuracy/length(x)
accuracy #0.6448911. Accuracy better than baseline! Not by much unfortunately



