
#------------------------------
# library
#------------------------------

library(data.table)
library(magrittr)

#------------------------------
# function
#------------------------------

max_without_na <- function(vec){
	if(all(is.na(vec))) return(0)
	max(vec, na.rm=TRUE)
}

min_without_na <- function(vec){
	if(all(is.na(vec))) return(0)
	min(vec, na.rm=TRUE)
}

duration <- function(vec){
	ma <- max_without_na(vec)
	mi <- min_without_na(vec)
	return(ma - mi)
}

date_features <- function(df){
	data.frame(
		Id = df$Id,
		date_mean	= rowMeans(df[,-1], na.rm=TRUE),
		date_max	= apply(df[,-1], 1, max_without_na),
		date_min	= apply(df[,-1], 1, min_without_na),
		date_dur	= apply(df[,-1], 1, duration),
		date_numna	= apply(df[,-1], 1, . %>% is.na %>% sum)
	)
}

#------------------------------
# main
#------------------------------

data_dir <- file.path("..", "..", "dataset")
input_dir <- file.path("..", "..", "input")

tic <- proc.time()

# load train date
cat("==> Load train date\n")
fread(file.path(data_dir, "train_date.csv"), data.table=FALSE) %>%
	date_features %>%
	fwrite(file.path(input_dir, "train_date_features.csv"), quote=FALSE, sep=",")

# load test date
cat("==> Load test date\n")
fread(file.path(data_dir, "test_date.csv"), data.table=FALSE) %>%
	date_features %>%
	fwrite(file.path(input_dir, "test_date_features.csv"), quote=FALSE, sep=",")

# display computational time
cat("==> Display computational time\n")
calc <- proc.time() - tic
print(calc)
