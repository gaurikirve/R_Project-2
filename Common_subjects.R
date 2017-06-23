commonSubjects <- dbGetQuery(db, "
SELECT MetadataSubject Subject,
                             COUNT(Id) NumberOfOccurences
                             FROM Emails
                             GROUP BY MetaDataSubject
                             ORDER BY COUNT(Id) DESC")
print.table(head(commonSubjects, n=20))

