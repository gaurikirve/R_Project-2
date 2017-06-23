#Required packages
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(quanteda)
library(gmodels)
library(scales)
library(readr)
library(RColorBrewer)

#Connect to db
db <- dbConnect(dbDriver("SQLite"), "../input/database.sqlite")

#get all emails
emails <- dbGetQuery(db, "
                     SELECT ExtractedBodyText body,MetadataSubject subject, MetadataDateSent date 
                     FROM Emails e 
                     INNER JOIN Persons p 
                     ON e.SenderPersonId=P.Id 
                     WHERE p.Name='Hillary Clinton'  
                     ORDER BY RANDOM()")




# DATA CLEANING 

#Create new column with weekdays, and column weekend
emails = emails %>% separate(date,"date", sep = "T") %>% mutate(
  weekday = weekdays(as.Date(emails$date)),
  weekend = ifelse(weekday %in% c('Saturday','Sunday'),1,0)
)

#Set levels order
emails$weekday = factor(emails$weekday, ordered=TRUE, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


#Clean some of the email bodies that still contain part of the header (after manual inspection of emails)
emails = emails %>% mutate(body = sub("H <.*Re:.*\\n", "", body)) %>%
  mutate(body = sub("H <.*Re:", "", body)) %>%
  mutate(body = sub("H <.*RELEASE IN.*B6", "", body)) %>%
  mutate(body = sub("RELEASE\\nIN PART B6\\n", "", body)) %>%
  mutate(body = sub("RELEASE\\nIN PART B6", "", body)) %>%
  mutate(body = sub("RELEASE IN PART\\nB6", "", body)) %>%
  mutate(body = sub("RELEASE IN PART.*\\B1", "", body)) %>%
  mutate(body = sub("U.S.*B6", "", body)) %>%
  mutate(body = sub("H <.*Fw:", "", body)) %>%
  mutate(body = sub('Declassify on: 04/23/2035', "", body)) %>%
  mutate(body = sub("H <.*B6\\nB6\\n", "", body)) %>%
  mutate(body = sub("H <.*PM\\n", "", body)) %>%
  mutate(body = sub("H <.*AM\\n", "", body)) %>%
  mutate(body = sub("B6", "", body)) %>%
  mutate(body = sub("B5", "", body))



emails = emails %>% mutate(analysis_body = paste(subject,body,sep=". ")) %>% #Marge subject with body, inside analysis_body, the column we will use for most analyses
  mutate(body_length = nchar(body), #Create variable with length of email body
         subject_length = nchar(subject),
         total_length = body_length + subject_length) %>% #Create variable with length of email subject
  filter(total_length != 0) #Remove empty emails



#Create corpus
email_corpus = corpus(emails$analysis_body)

#Aggregate all emails of each weekday together for analysis
day1 = paste(emails$analysis_body[emails$weekday=='Monday'],collapse=". ")
day2 = paste(emails$analysis_body[emails$weekday=='Tuesday'],collapse=". ")
day3 = paste(emails$analysis_body[emails$weekday=='Wednesday'],collapse=". ")
day4 = paste(emails$analysis_body[emails$weekday=='Thursday'],collapse=". ")
day5 = paste(emails$analysis_body[emails$weekday=='Friday'],collapse=". ")
day6 = paste(emails$analysis_body[emails$weekday=='Saturday'],collapse=". ")
day7 = paste(emails$analysis_body[emails$weekday=='Sunday'],collapse=". ")
weekday_vector = c(day1,day2,day3,day4,day5,day6,day7)

#Create corpus of those 7 texts
weekdays_corpus = corpus(weekday_vector,docnames = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

#Create dfm, after stemming and removing stopwords
weekdays_dfm <- dfm(weekdays_corpus, verbose = TRUE, stem = TRUE, ignoredFeatures = stopwords("english"))

#Same without stemming
weekdays_dfm_noStemming <- dfm(weekdays_corpus, verbose = TRUE, stem = FALSE, ignoredFeatures = stopwords("english"))