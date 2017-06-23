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


# DESCRIPTIVE ANALYSIS 

#bar chart of emails sent per weekday
data = emails %>% mutate(total = n()) %>%
  group_by(weekday) %>%
  summarise(count = n(),
            total = mean(total),
            freq = count/total)
data %>%
  ggplot(aes(x = weekday,y = freq)) + 
  geom_bar(stat="identity",aes(,fill = weekday)) +
  theme(legend.position = "none") +
  geom_text(aes(label=paste(count,"\n(",scales::percent(freq),")",sep="")),vjust=0.45) +
  xlab("Day") +
  ylab("Percentage") +
  ggtitle("E-mails sent per day of the week")


#Bar chart of median emails length per weekday
data = emails %>% group_by(weekday) %>%
  summarise(median_length = median(body_length))

data %>%
  ggplot(aes(x = weekday,y = median_length)) + 
  geom_bar(stat="identity",aes(,fill = weekday)) +
  theme(legend.position = "none") +
  geom_text(aes(label=median_length),vjust=-0.4) +
  xlab("Day") +
  ylab("Median Length (characters)") +
  ggtitle("Median body length sent per day of the week")


#Bar chart of median emails subject length per weekday
data = emails %>% group_by(weekday) %>%
  summarise(median_length = median(subject_length))

data %>%
  ggplot(aes(x = weekday,y = median_length)) + 
  geom_bar(stat="identity",aes(,fill = weekday)) +
  theme(legend.position = "none") +
  geom_text(aes(label=median_length),vjust=-0.4) +
  xlab("Day") +
  ylab("Median Length (characters)") +
  ggtitle("Median subject length sent per day of the week")


#Bar chart of median emails length per weekday
data = emails %>% group_by(weekday) %>%
  summarise(median_length = round(mean(body_length),digits=2))

data %>%
  ggplot(aes(x = weekday,y = median_length)) + 
  geom_bar(stat="identity",aes(,fill = weekday)) +
  theme(legend.position = "none") +
  geom_text(aes(label=median_length),vjust=-0.4) +
  xlab("Day") +
  ylab("Average Length (characters)") +
  ggtitle("Average body length sent per day of the week")

###########################
### WORD CLOUDS PER DAY ###
###########################

#Remove additional email stopwords (to experiment with)
#weekdays_dfm_noStemming = removeFeatures(weekdays_dfm_noStemming,c("will","can","pls","thx","w","also","print","call"))

#Global word cloud
plot(weekdays_dfm_noStemming, max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))

#One word cloud for each of the 7 days
plot(weekdays_dfm_noStemming[1,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[2,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[3,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[4,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[5,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[6,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))
plot(weekdays_dfm_noStemming[7,], max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .2))



