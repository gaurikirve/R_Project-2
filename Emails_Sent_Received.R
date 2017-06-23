commonSenders <- dbGetQuery(db, "
SELECT p.Name, COUNT(p.Name) NumEmailsSent
                            FROM Emails e
                            INNER JOIN Persons p ON e.SenderPersonId=p.Id
                            GROUP BY p.Name
                            ORDER BY COUNT(p.Name) DESC
                            LIMIT 10")

library(ggplot2)
ggplot(commonSenders, aes(x=reorder(Name, NumEmailsSent), y=NumEmailsSent)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") +
  ylab("Number of Emails Sent") + 
  theme(plot.title=element_text(size=14))

commonRecipients <- dbGetQuery(db, "
SELECT p.Name, COUNT(p.Name) NumEmailsReceived
                               FROM Emails e
                               INNER JOIN EmailReceivers r ON r.EmailId=e.Id
                               INNER JOIN Persons p ON r.PersonId=p.Id
                               GROUP BY p.Name
                               ORDER BY COUNT(p.Name) DESC
                               LIMIT 10")

library(ggplot2)
ggplot(commonRecipients, aes(x=reorder(Name, NumEmailsReceived), y=NumEmailsReceived)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=16) +
  xlab("") + 
  ylab("Number of Emails Received") + 
  theme(plot.title=element_text(size=14))