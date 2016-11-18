
#------------------------------
# library
#------------------------------

library(Matrix)
library(magrittr)
library(data.table)
library(dtplyr)
library(dplyr)

#------------------------------
# functions
#------------------------------

remove_response <- function(data, dataset="train"){
	if(dataset != "train") return(data)
	select(data, -Response)
}

load_numeric <- function(data, dataset="train"){

	cat("==> Load", dataset, "numeric\n")
	fread(file.path(data_dir, paste0(dataset, "_numeric.csv")), data.table=FALSE) %>%

	# remove unneeded columns
	select(-Id) %>%
	remove_response(dataset=dataset) %>%

	# convert to sparse matrix
	data.matrix %>%
	replace(.==0, 1e-10) %>%
	replace(is.na(.), 0) %>%
	Matrix(sparse=TRUE)
}
