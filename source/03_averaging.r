
#------------------------------
# library
#------------------------------

library(magrittr)
library(dplyr)

#------------------------------
# main
#------------------------------

# setting
dir_path <- file.path("..", "submission")
prj_name <- "submission_pre"

# stack filenames
print("==> Stack filenames")
filename_regex <- ".+_train(.+)_valid(.+)_seed(.+).csv"

result <- list.files(file.path("..", "submission", prj_name)) %>%
	data.frame(
		filenames=.,
		train=gsub(filename_regex, "\\1", .) %>% as.numeric,
		valid=gsub(filename_regex, "\\2", .) %>% as.numeric,
		seed=gsub(filename_regex, "\\3", .) %>% as.integer
		) %>%
	arrange(desc(valid)) %T>% 
	print

# number of input
num_model <- nrow(result)

# averaging
print("==> Load data")
data_all <- NULL
for(i in 1:num_model){
	target_file <- as.character(result$filename[i])
	cat("filename: ", target_file, "\n")

	datai <- read.csv(file.path(dir_path, prj_name, target_file))
	print(table(datai$Response))

	colnames(datai) <- c("Id", result$seed[i])

	if(i==1){
		data_all <- datai
	}else{
		data_all <- merge(data_all, datai, by="Id")
	}
}

# ensemble
print("==> Averaging")
threshold <- ceiling(num_model / 2)
ensemble <- as.integer(rowSums(data_all[,-1]) >= threshold)
cat("ensemble result\n")
print(table(ensemble))

submission <- data.frame(Id=data_all$Id, Response=ensemble)

# display scores
train_scores <- result$train[1:num_model]
valid_scores <- result$valid[1:num_model]
cat("train: ", sprintf("%.4f +- %.4f", mean(train_scores), sd(train_scores)), "\n")
cat("valid: ", sprintf("%.4f +- %.4f", mean(valid_scores), sd(valid_scores)), "\n")

# save output
meantrain <- mean(train_scores)
meanvalid <- mean(valid_scores)
st <- function(score) return(sprintf("%.3f", score))
filename <- paste0("submission_ensemble", num_model, "_mtrain", st(meantrain), "_mvalid", st(meanvalid), ".csv")
write.table(submission, file.path(dir_path, filename), row.names = FALSE, sep=",", quote = FALSE)
