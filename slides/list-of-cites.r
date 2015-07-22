################################################################################
###
### Make a big list of citations from the spreadsheet of articles that use CINC
### scores in a dyadic analysis
###
################################################################################

library("dplyr")
library("stringr")

articles <- read.csv("articles.csv",
                     stringsAsFactors = FALSE)

## Only need authors and years for citations
articles <- select(articles, Authors, Year)

## Identify articles with more than one author
##
## Inconsistent comma use in spreadsheet -> can't really treat two authors
## separately
articles <- mutate(articles,
                   multiple = sapply(str_split(Authors, " and "), length) > 1)

## Extract first author's last name
articles <- mutate(articles,
                   lastname = sapply(str_split(Authors, ","), "[", 1))

## Construct Author (Year) citation
articles <- mutate(articles,
                   citation = paste0(lastname,
                                     ifelse(multiple, " et al", ""),
                                     " (",
                                     Year,
                                     ")"))

cat(paste(articles$citation, collapse = ",\n"), "\n",
    file = "list-of-cites.tex")
