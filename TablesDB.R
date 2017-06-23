library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "../input/database.sqlite")

library(dplyr)
tables <- dbGetQuery(db, "SELECT Name FROM sqlite_master WHERE type='table'")
colnames(tables) <- c("Name")
tables <- tables %>%
  rowwise() %>%
  mutate(RowCount=dbGetQuery(db, paste0("SELECT COUNT(Id) RowCount FROM ", Name))$RowCount[1])
print.table(tables)

