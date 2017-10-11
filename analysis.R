library(ggplot2)
library(gridExtra)
library(lattice)


load("rfile_list.rda")
sum(is.na(rfile_list)) #num of students who didn't complete the course
sum(is.na(rfile_list))/length(rfile_list) #ratio of students who didn't complete the course
rfile_list <- rfile_list[!is.na(rfile_list)]
rfile_length <- unlist(lapply(rfile_list,length))

rfile_length_filter <- rfile_length[rfile_length<300]

pure_text <- lapply(rfile_list, function(x) {x[!grepl("^#", x) & !grepl("\\ ^#", x) & x != "" & x != " "]})
pure_text_length <- unlist(lapply(pure_text,length))

pure_text_filter <- lapply(rfile_list[rfile_length<300], function(x) {x[!grepl("^#", x) & !grepl("\\ ^#", x) & x != "" & x != " "]})
pure_text_length_filter <- unlist(lapply(pure_text_filter,length))

rfile_puretext_dataframe <- data.frame(code_length = c(rfile_length,pure_text_length),
                                       type = rep(c("full code","pure code"), each = length(pure_text_length)))

rfile_puretext_filter_dataframe <- data.frame(code_length = c(rfile_length_filter,pure_text_length_filter),
                                       type = rep(c("full code","pure code"), each = length(pure_text_length_filter)))


p1 <- ggplot(data = rfile_puretext_dataframe, aes(x = type, y = code_length, color = type)) +
  #geom_jitter(aes(x = type, y = code_length, color = type),
  #            position = position_jitter(width = .1)) +
  geom_boxplot(alpha = 0.8, fill = NA) +
  ylim(c(-0.5,max(range(rfile_puretext_dataframe[,1]) + 0.5)))  + # set ranges for y axies
  # change x labels name 
  ggtitle(paste0("code length between full code length and pure code")) +
  ylab("code length") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.title = element_text(hjust = .5))
  
p1

p1_filter <- ggplot(data = rfile_puretext_filter_dataframe, aes(x = type, y = code_length, color = type)) +
  geom_boxplot(alpha = 0.8, fill = NA) +
  ylim(c(-0.5,max(range(rfile_puretext_filter_dataframe[,1]) + 0.5)))  + # set ranges for y axies
  # change x labels name 
  ggtitle(paste0("code length between full code length and pure code")) +
  ylab("code length") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.title = element_text(hjust = .5))

p1_filter



rfile_text <- unlist(pure_text)
library_usage <- rfile_text[grep("library\\(", rfile_text)]
names(library_usage) <- NULL

# 
library_usage <- sapply(library_usage, function(x) gsub("\'|\t|\"| |;","",x)) 

# handle suppressmessage()
suppressmessage_idx <- grep("suppressMessages",library_usage)
library_usage[suppressmessage_idx] <- sapply(library_usage[suppressmessage_idx], function(x) sub("suppressMessages\\(","",x)) 
library_usage[suppressmessage_idx] <- sapply(library_usage[suppressmessage_idx], function(x) sub(")","",x)) 

unique_library <- unique(library_usage)
unique_library_counts <- sapply(unique_library, function(x) {sum(library_usage %in% x)})
top_used_libraries <- sort(unique_library_counts, decreasing = TRUE)[1:25]       

library_dataframe <- data.frame(counts = top_used_libraries,
                                library = names(top_used_libraries))
p2 <- ggplot(library_dataframe, aes(x = reorder(library,counts), y = counts)) + 
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  xlab("library") +
  ggtitle(paste0("library usage")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),plot.title = element_text(hjust = .5)) 

p2


load("rfile_list.rda")
pure_text <- lapply(rfile_list, function(x) {x[!grepl("^#", x) & !grepl("\\ ^#", x) & x != "" & x != " "]})
rfile_text <- unlist(pure_text[!is.na(pure_text)])
Sys.setlocale("LC_ALL", "C")
pattern <- gregexpr("[[:alnum:]._ ]+\\(",rfile_text)
functions <- regmatches(rfile_text,pattern) %>% unlist()
functions <- gsub(" *|\\(","",functions)
unique_functions <- unique(functions[functions != "library" & functions != "function"])

function_usage <- sapply(unique_functions, function(x){sum(functions %in% x)}) 
self_defined_functions <- names(function_usage[which(function_usage == 1)])
top_used_functions <- sort(function_usage, decreasing = TRUE)[1:25]

function_dataframe <- data.frame(counts = top_used_functions,
                                functions = names(top_used_functions))
p3 <- ggplot(function_dataframe, aes(x = reorder(functions,counts), y = counts)) + 
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  xlab("function names") +
  ggtitle(paste0("top used functions")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),plot.title = element_text(hjust = .5)) 

p3


