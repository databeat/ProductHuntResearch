# PRODUCT HUNT

library(lubridate)
library(dplyr)
library(caret)

# read in the data
posts <- read.csv(file = "/Users/jonathanortiz/code/ProductHunt/jq/ProductHunt_Posts_11242014_to_11232016.csv", header = TRUE)
str(posts) # check out the data

# rename the 'day' column to 'date' and set to Date
names(posts)[names(posts)=="day"] <- "date"
posts$date <- as.Date(posts$date)
str(posts$date)

# set 'created_at' to POSIXlt
posts$created_at <- as.character(posts$created_at)
options(digits.secs = 3)
posts$created_at <- strptime(posts$created_at, "%Y-%m-%dT%H:%M:%S", tz = "America/Los_Angeles")
str(posts$created_at)

# set 'name' and 'tagline' to characters
posts$name <- as.character(posts$name)
posts$tagline <- as.character(posts$tagline)

# create a new variable called 'day' that is the day of the week the product was posted
posts$day <- "day"
posts$day <- weekdays(posts$date)
str(posts$day)
posts$day <- as.factor(posts$day)
str(posts$day)

# create a new variable 'time_of_day' that is the approximate time of day the product was posted
posts$time_of_day <- "time_of_day"
for(i in 1:nrow(posts)) {
  if(hour(posts$created_at[i]) >= 5 && hour(posts$created_at[i]) <= 11) {
    posts$time_of_day[i] <- "Morning"
  }
  else if(hour(posts$created_at[i]) >= 12 && hour(posts$created_at[i]) <= 16) {
    posts$time_of_day[i] <- "Afternoon"
  }
  else if(hour(posts$created_at[i]) >= 17 && hour(posts$created_at[i]) <= 20) {
    posts$time_of_day[i] <- "Evening"
  }
  else if(hour(posts$created_at[i]) >= 21 || hour(posts$created_at[i]) <= 4) {
    posts$time_of_day[i] <- "Night"
  }
}
posts$time_of_day <- as.factor(posts$time_of_day)
str(posts$time_of_day)

# rearrange the columns
names(posts)
refcols <- c("id", "date", "day", "created_at", "time_of_day")
posts <- posts[, c(refcols, setdiff(names(posts), refcols))]
str(posts)

str(posts$votes_count)
names(posts)
# remove a week's worth of the most recent posts,
# because they haven't been given a lot of time for votes
older_posts <- posts[150:nrow(posts),]

# write out some CSVs with current changes
write.csv(posts, "./AllPosts.csv", row.names = FALSE)
write.csv(older_posts, "./PostsForAnalysis.csv", row.names = FALSE)

# hitting limit on data.world, so replace all "FALSE" with NULL
older_posts_with_nulls <- older_posts
for (j in 13:313) {
  for (i in 1:nrow(older_posts_with_nulls)){
    if(older_posts_with_nulls[i,j] == "false") {
      older_posts_with_nulls[i,j] <- ""
    }
  }
}
write.csv(older_posts_with_nulls, "./PostsForAnalysis2.csv", na = "", row.names = FALSE)

summary(older_posts$tech)
summary(older_posts_with_nulls$tech)

lm <- train(votes_count~., data=older_posts, method=lm)


posts$time_of_day2 <- "time_of_day"
for(i in 1:nrow(posts)) {
  if(hour(posts$created_at[i]) >= 5 && hour(posts$created_at[i]) <= 7) {
    posts$time_of_day2[i] <- "EarlyMorning"
  }
  else if(hour(posts$created_at[i]) >= 8 && hour(posts$created_at[i]) <= 10) {
    posts$time_of_day2[i] <- "Morning"
  }
  else if(hour(posts$created_at[i]) == 11) {
    posts$time_of_day2[i] <- "LateMorning"
  }
  else if(hour(posts$created_at[i]) == 12) {
    posts$time_of_day2[i] <- "MidDay"
  }
  else if(hour(posts$created_at[i]) == 13) {
    posts$time_of_day2[i] <- "EarlyAfternoon"
  }
  else if(hour(posts$created_at[i]) == 14 || hour(posts$created_at[i]) == 15) {
    posts$time_of_day2[i] <- "Afternoon"
  }
  else if(hour(posts$created_at[i]) == 16) {
    posts$time_of_day2[i] <- "LateAfternoon"
  }
  else if(hour(posts$created_at[i]) == 17 || hour(posts$created_at[i]) == 18) {
    posts$time_of_day2[i] <- "EarlyEvening"
  }
  else if(hour(posts$created_at[i]) == 19 || hour(posts$created_at[i]) == 20) {
    posts$time_of_day2[i] <- "Evening"
  }
  else if(hour(posts$created_at[i]) >= 21 || hour(posts$created_at[i]) <= 4) {
    posts$time_of_day2[i] <- "Night"
  }
}