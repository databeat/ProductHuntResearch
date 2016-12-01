install.packages(c("httr","jsonlite","lubridate"), dependencies = TRUE)

library(httr)
library(jsonlite)
library(lubridate)

options(stringsAsFactors = FALSE)
  # When working with APIs, turn this off.
  # Disables turning character strings automatically into factor variables

url  <- "http://api.epdb.eu"
path <- "eurlex/directory_code"

raw.result <- GET(url = url, path = path)
names(raw.result)
# [1] "url"         "status_code" "headers"     "all_headers" "cookies"     "content"    
# [7] "date"        "times"       "request"     "handle" 
  
  # 1) status_code that tells us if the call worked network-wise. list of possible status
  # codes, see https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
  
  # 2) content the API’s answer in raw binary code, not text.
  # The answer could also be an image or a sound file.

raw.result$status_code
  # 200 means that the server received our request,
  # not if it was valid for the API or found any data

head(raw.result$content)
  # UNICODE
this.raw.content <- rawToChar(raw.result$content)
  # translated to chars
nchar(this.raw.content)
  # very big
substr(this.raw.content, 1, 100)
  # look at first 100 chars
  # This is the first 100 chars from a JSON file.
this.content <- fromJSON(this.raw.content)
  # Tell R to parse it into something R can work with
class(this.content)
  # It's a list
length(this.content)
  # It's a large list
this.content[[1]]
  # The first element
this.content[[2]]
 # The second element

  # So, apparently R makes a list out of it, with one element per classifier. Each element has:
    # the directory code document classifier
    # a URL where one can retrieve more details
    # the number of documents with that classifier
    # another URL with yet more details

this.content.df <- do.call(what = "rbind", args = lapply(this.content, as.data.frame))
  # Turn the list of lists into a data frame

class(this.content.df)
  # a single data frame
dim(this.content.df)
  # with 462 rows and 4 variables
head(this.content.df)
  # can work with this

## Need to find the classifiers for energy topics, which start with 12
headClass <- substr(x = this.content.df[, "directory_code"], start = 1, stop  = 2)
  # headClass is now just a character vector containing the first two characters of the
  # directory_code for each of the 462 different classifiers.
length(headClass)
  # 462 in the vector
head(headClass)
  # if these two characters equal 12, it's an energy topic
isEnergy <- headClass == "12" # note that 12 is in quote because it is a char vector
  # now a Logical vector of length 462
table(isEnergy)
  # 19 of the topic classifiers start with 12
relevant.df <- this.content.df[isEnergy, ]
  # pretty cool. pass isEnergy as the rows to subset the dataframe for only the relevant classifiers
relevant.dc <- relevant.df[, "directory_code"]
  # narrow it down to solely the document identifiers
    # relevant.dc is now a character vector with all the directory codes
    # that are relating to energy topics.
length(relevant.dc)
  # 19
relevant.dc

## Retrieving energy documents’ meta data
# We cannot pass all the relevant classifiers in a single call.
# Rather, we need to create 19 queries, one for each identified classifier. 
makeQuery <- function(classifier) {
  this.query <- list(classifier)
  names(this.query) <- "dc"
  return(this.query)
}
  # makeQuery() takes a single argument, classifier, and turns it into a single element
  # list and sets the name of that list’s single element to be dc

queries <- lapply(as.list(relevant.dc), makeQuery)
  # Now we have a list (queries) that is composed of all the individual queries that
  # result from our function.

this.raw.result <- GET(url = url, path = path, query = queries[[1]])
  # Start with first query
this.result <- fromJSON(rawToChar(this.raw.result$content))
  # What did we get? got back the meta data for all 11 documents that are classified
  # with our first relevant classifier (12.07.00.00)
names(this.result[[1]])

all.results <- vector(mode   = "list", length = length(relevant.dc))
for (i in 1:length(all.results)) {
  this.query       <- queries[[i]]
  this.raw.answer  <- GET(url = url, path = path, query = this.query)
  this.answer      <- fromJSON(rawToChar(this.raw.answer$content))
  all.results[[i]] <- this.answer
  message(".", appendLF = FALSE)
  Sys.sleep(time = 1)
}

parseAnswer <- function(answer) {
  this.form   <- answer$form
  this.date   <- answer$date
  this.effect <- answer$of_effect
  result <- data.frame(form   = this.form,
                       date   = this.date,
                       effect = this.effect)
  return(result)
}

parsedAnswers <- lapply(all.results, function(x) do.call("rbind", lapply(x, parseAnswer)))

finalResult <- do.call("rbind", parsedAnswers)
finalResult$date <- ymd(finalResult$date)
finalResult$effect <- ymd(finalResult$effect)

finalResult$effectDay <- wday(finalResult$effect, label = TRUE)
table(finalResult$effectDay) #Most documents went into effect on a Wednesday
