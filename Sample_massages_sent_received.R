emailsFromHillary <- dbGetQuery(db, "
SELECT p.Name Sender,
                                ExtractedBodyText EmailBody
                                FROM Emails e
                                INNER JOIN Persons p ON e.SenderPersonId=P.Id
                                WHERE p.Name='Hillary Clinton'
                                AND e.ExtractedBodyText != ''
                                ORDER BY RANDOM()")
print.table(head(emailsFromHillary))

library(tm)
library(wordcloud)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}

makeWordCloud(emailsFromHillary[["EmailBody"]])

emailsToHillary <- dbGetQuery(db, "
SELECT p.Name Recipient,
       ExtractedBodyText EmailBody
FROM Emails e
INNER JOIN EmailReceivers r ON r.EmailId=e.Id
INNER JOIN Persons p ON r.PersonId=P.Id
WHERE p.Name='Hillary Clinton'
  AND e.ExtractedBodyText != ''
ORDER BY RANDOM()")
print.table(head(emailsToHillary))

makeWordCloud(emailsToHillary[["EmailBody"]][1:2000])

