omdb <- read.csv(choose.files())
tomatoes <- read.csv(choose.files())
db <- merge(new_omdb, new_tomatoes, by.x = "ID", by.y = "ID")
new_omdb <- omdb
new_omdb[c("imdbID", "Released", "Writer", "Cast", "imdbVotes", "Awards", "lastUpdated")] <-  NULL
new_tomatoes <- tomatoes
new_tomatoes[c("Image", "Reviews", "Fresh", "Rotten", "Consensus", "userReviews", "DVD", 
               "Production", "Website", "lastUpdated")] <- NULL

head(db)
write.csv(db, "magisterka.csv")
summary(db)
db$ID <- NULL
