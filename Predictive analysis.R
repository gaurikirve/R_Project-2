
# PREDICTIVE ANALYSIS 
# Predicting whether an email was sent during the weekend or not
#


#Create dfm, after stemming
email_dfm <- dfm(email_corpus, stem = TRUE)

#Remove stopwords
#email_dfm = removeFeatures(email_dfm, stopwords("english"))

#Removing rare words
freqTerms <- topfeatures(email_dfm, n = nfeature(email_dfm))
email_dfm = removeFeatures(email_dfm, names(freqTerms[freqTerms < 20]))#Remove all words that appear less than 20 times in the entire dataset

#store dfm into a df
email_df = as.data.frame(email_dfm)

#Number of documents in the corpus
nb_texts = ndoc(email_corpus)

#Training data ratio
training_ratio = 0.7

#Add other useful columns to email_df
body_length = emails$body_length
subject_length = emails$subject_length
weekend_label = as.factor(emails$weekend)
positive_sent = sentiment$positive_weighted
negative_sent = sentiment$negative_weighted
anger_sent = sentiment$anger_weighted
anticipation_sent = sentiment$anticipation_weighted
disgust_sent = sentiment$disgust_weighted
fear_sent = sentiment$fear_weighted
joy_sent = sentiment$joy_weighted
sadness_sent = sentiment$sadness_weighted
surprise_sent = sentiment$surprise_weighted
trust_sent = sentiment$trust_weighted
email_df = cbind(email_df, weekend_label) %>% cbind(body_length) %>% cbind(subject_length) %>%
  cbind(positive_sent) %>%
  cbind(negative_sent) %>%
  cbind(anger_sent) %>%
  cbind(anticipation_sent) %>%
  cbind(disgust_sent) %>%
  cbind(fear_sent) %>%
  cbind(joy_sent) %>%
  cbind(sadness_sent) %>%
  cbind(surprise_sent) %>%
  cbind(trust_sent) 

training_size = floor(training_ratio*nb_texts)
training_df = email_df[1:training_size,]

testing_df = email_df[(training_size+1):nb_texts,]

#Percentage of emails sent during the week in training set
1-(sum(as.numeric(training_df$weekend)-1)/(training_size+1)) #0.6468055

#Accuracy baseline: how well would we perform on the testing set if we were predicting everything as being sent during the week (the most likely case)? Predictions with models should not be lower than that 
1-(sum(as.numeric(testing_df$weekend)-1)/(nb_texts-(training_size+1))) #0.6325503



##
# Naive Bayes (using quanteda built-in function)
# Based only on words count, not using additional attributes such as sentiment
##

#Create training labels vector
training_labels = emails$weekend
training_labels[ceiling(training_ratio*nb_texts):nb_texts] = NA #Remove label for texts that will be the training set

#Training
NB = textmodel_NB(email_dfm, training_labels)

#Predict on test set
pred = predict(NB, newdata = email_dfm[ceiling(training_ratio*nb_texts):nb_texts,])

#Plot confusion matrix from gmodels package
x = pred$nb.predicted
y = emails$weekend[ceiling(training_ratio*nb_texts):nb_texts]
CrossTable(x, y, prop.chisq=FALSE)

#Calculate accuracy rate on test set
accuracy = 0
conf = table(x,y)
for(i in 1:2)
{
  accuracy = accuracy + conf[i,i]
}
accuracy = accuracy/length(x)
accuracy #0.5862647. Much worse than baseline



