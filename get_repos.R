library(dplyr)
library(gh)
library(lubridate)

# "Cite Andrew Leroux's code to get dates"
# start from 2008

date_start <- ymd("2008-01-01")        ## start date
day_inc  <- 14                       ## increment days by 14 at a time
dates <- c()
i <- 1
while(date_start < Sys.Date() - (day_inc+1)) {
  dates[[i]] <- c(rep(date_start,2) %m+% c(days(-1),days(day_inc+1)))
  date_start <- date_start + days(day_inc + 1)
  i <- i + 1
}


### NOTE: Need to create a personal access authentication token for using GET /seach/code!!!
### Do this here: https://github.com/settings/tokens
token <- readLines("githubtoken.txt")

dates_repos <- lapply(c(1:length(dates)), function(dates_num) {
  gh_date <- paste("created:", paste(dates[[dates_num]], collapse=".."), sep="")
  repos <- c()
  
  for (page_num in 1:10){
    
    repo_name <- paste0("GET /search/repositories?q=getting+and+cleaning+data+",
                        gh_date, "&per_page=100")
    x <- try(gh(repo_name, page = page_num, .token = token))
    
    if ("try-error" %in% class(x) == FALSE) {
      
      repos <- c(sapply(x[[3]], "[[", "full_name"),repos)
    }
    
  }
  print(dates[[dates_num]])
  Sys.sleep(60)
  return(repos)
})


save(dates_repos, file = "all_repos.rda")
