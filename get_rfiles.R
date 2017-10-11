
library(gh)

load("all_repos.rda") # load: dates_repos
repolength <- lapply(c(1:length(dates_repos)), function(x) {length(dates_repos[[x]])}) 
do.call(sum,repolength) # how many repos in total

repos <- dates_repos %>% unlist()
token <- readLines("githubtoken.txt")
rfile_list <- list()
for (i in 1:length(repos)){
  rfile_list[[i]] <- NA
}

numof_unfinished <- 0

# "Cite Stephen's code to use gh function getting repo names"
for (i in 1:length(repos)){
  i = 328
  string <- paste0("GET /search/code?q=repo:", repos[i],"+extension:r")
  res <- try(gh::gh(string, .token=token), silent = TRUE)
  
  if ("try-error"  %in% class(res) == FALSE) {
    # loop_path to get all .R files
    path <- try(res[[3]][[1]]$path, silent = TRUE)
    
    if ("try-error" %in% class(path) == FALSE) {
      print(i)
      code.url <- file.path("https://raw.githubusercontent.com",repos[[i]], "master", path)
      code.url <- gsub(" ","%20",code.url)
      
      tryCatch({code <- readLines(code.url, warn = FALSE)
                rfile_list[[i]] <- code},
               error = function(x) {message(x)}, 
               warning = function(x) {message(x)})
 
      Sys.sleep(5)
    }
    else {
      print(paste0("no r file found in the repo: ",repos[i]))
      numof_unfinished <- numof_unfinished +1}
  }
  
}


print(paste0("number of unfinished: ",numof_unfinished))
save(rfile_list, file = "rfile_list.rda")
