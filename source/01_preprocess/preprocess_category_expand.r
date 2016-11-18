
#------------------------------
# library
#------------------------------

library(data.table)
library(magrittr)
library(digest)
library(plyr)
library(dtplyr)
library(dplyr)

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

is_useful_category <- function(vec){
	length(unique(vec)) > 1
}

replace_NA <- function(vec){
	levels(vec) <- c(levels(vec), "NA")
	vec[is.na(vec)] <- "NA"
	vec
}

define_formula <- function(train_categorical){
	
	train_categorical_digests <- train_categorical %>% sapply(digest)
	train_categorical_useful <- train_categorical %>% sapply(is_useful_category)

	target <- names(train_categorical_digests)[!duplicated(train_categorical_digests) & train_categorical_useful]

	paste("~", paste(target, collapse=" + "), -1)
}

#------------------------------
# main
#------------------------------

data_dir <- file.path("..", "..", "dataset")
colClasses <- c("integer", rep("factor", 2140))

tic <- proc.time()

# train data ----------

train_categorical <- fread(file.path(data_dir, "train_categorical.csv"), data.table=FALSE,
							na.strings="", showProgress=TRUE, colClasses=colClasses, drop="Id")

print("==> Replace NA")
for(i in seq_along(train_categorical)){
	train_categorical[,i] <- replace_NA(train_categorical[,i])
}

print("==> Define formula")
f <- define_formula(train_categorical)

train_categorical_levels <- lapply(train_categorical, levels)

print("==> Expand data")
train_categorical_expand <- model.matrix(as.formula(f), data=train_categorical)

rm(train_categorical)
gc();gc();

train_categorical_expand <- data.frame(train_categorical_expand)

print("==> Get colnames")
expand_colnames <- train_categorical_expand[FALSE,]

train_categorical_id <- fread(file.path(data_dir, "train_categorical.csv"), select="Id") %>% use_series(Id)
train_categorical_expand <- cbind(Id=train_categorical_id, train_categorical_expand)

fwrite(train_categorical_expand, "../../input/train_categorical_expand.csv", quote=FALSE, sep=",")

# save(f, expand_colnames, train_categorical_levels, file="categorical_schema.robj")
# load(file="categorical_schema.robj")

# test data ----------

test_categorical <- fread(file.path(data_dir, "test_categorical.csv"), data.table=FALSE,
							na.strings="", showProgress=TRUE, colClasses=colClasses, drop="Id")

print("==> Update levels")
for(i in seq_along(test_categorical)){
	levels(test_categorical[,i]) <- union(levels(test_categorical[,i]), train_categorical_levels[[i]])
}

print("==> Replace NA")
for(i in seq_along(test_categorical)){
	test_categorical[,i] <- replace_NA(test_categorical[,i])
}

print("==> Expand data")
contrasts <- function(x) return(attr(x, "contrasts"))
test_categorical_expand <- model.matrix(as.formula(f), data=test_categorical)

rm(test_categorical)
gc();gc();

test_categorical_expand <- data.frame(test_categorical_expand)

print("==> Select columns")
use_col <- intersect(colnames(expand_colnames), colnames(test_categorical_expand))
test_categorical_expand <- rbind.fill(as.data.frame(expand_colnames), test_categorical_expand[,use_col])

test_categorical_expand <- replace(test_categorical_expand, is.na(test_categorical_expand), 0)

test_categorical_id <- fread(file.path(data_dir, "test_categorical.csv"), select="Id") %>% use_series(Id)
test_categorical_expand <- cbind(Id=test_categorical_id, test_categorical_expand)

fwrite(test_categorical_expand, "../../input/test_categorical_expand.csv", quote=FALSE, sep=",")

# display computational time
cat("==> Display computational time\n")
proc.time() %>% subtract(tic) %>% print
