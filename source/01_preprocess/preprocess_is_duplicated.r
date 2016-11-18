
#------------------------------
# library
#------------------------------

library(data.table)
library(magrittr)
library(dtplyr)
library(dplyr)
library(digest)

#------------------------------
# set seed
#------------------------------

seed <- as.integer(commandArgs(trailingOnly=TRUE)[1])
if(is.na(seed)) seed <- 1000
cat("seed: ", seed, "\n - - - - - - - - - -\n")

set.seed(seed)

#------------------------------
# function
#------------------------------

source("get_features_duplicated.r")

generate_output <- function(input, path){
	cat("==> Generate output\n")
	numeric_id <- fread(path, select="Id") %>% use_series(Id)
	data.frame(Id=numeric_id, is_duplicated=input)
}

remove_L3_S38 <- function(data){
	data[, -grep("^L3_S38_", colnames(data))]
}

#------------------------------
# main
#------------------------------

data_dir <- file.path("..", "..", "dataset")
input_dir <- file.path("..", "..", "input")

tic <- proc.time()

train_numeric_path <- file.path(data_dir, "train_numeric.csv")

fread(train_numeric_path, data.table=FALSE) %>%
	select(-Response) %>%
	select(-Id) %>%
	remove_L3_S38 %>%
	apply(1, digest) -> train_digest

names(train_digest) <- fread(train_numeric_path, select="Id") %>% use_series(Id)

test_numeric_path <- file.path(data_dir, "test_numeric.csv")

fread(test_numeric_path, data.table=FALSE) %>%
	select(-Id) %>%
	remove_L3_S38 %>%
	apply(1, digest) -> test_digest

names(test_digest) <- fread(test_numeric_path, select="Id") %>% use_series(Id)

joint_digest <- c(train_digest, test_digest)

joint_digest_order <- names(joint_digest) %>% as.integer %>% order
joint_digest <- joint_digest[joint_digest_order]

train_response <- fread(train_numeric_path, select="Response") %>% use_series(Response)
joint_response <- c(train_response, rep(NA, length(test_digest)))
joint_response <- joint_response[joint_digest_order]

train_date <- fread("../../input/train_date_features.csv") %>% use_series(date_mean)
test_date <- fread("../../input/test_date_features.csv") %>% use_series(date_mean)
joint_date <- c(train_date, test_date)
joint_date <- joint_date[joint_digest_order]

# gc -----

gc();gc();

# train --------------------

cat("==> Preprocess train data\n")

train_digest %>% get_features_duplicated %>%
	select(-Id) %>% as.matrix %>% inset(,, as.integer(.)) %>%
	generate_output(train_numeric_path) %>%
	write.table(file.path(input_dir, "train_is_duplicated.csv"), row.names=FALSE, quote=FALSE, sep=",")

# gc -----

gc();gc();

# test --------------------

cat("==> Preprocess test data\n")

test_digest %>% get_features_duplicated %>%
	select(-Id) %>% as.matrix %>% inset(,, as.integer(.)) %>%
	generate_output(test_numeric_path) %>%
	write.table(file.path(input_dir, "test_is_duplicated.csv"), row.names=FALSE, quote=FALSE, sep=",")

# display computational time -----

cat("==> Computational time\n")
proc.time() %>% subtract(tic) %>% print
