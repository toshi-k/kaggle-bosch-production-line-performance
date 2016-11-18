
#------------------------------
# library
#------------------------------

library(data.table)
library(xgboost)
library(Matrix)
library(magrittr)
library(dtplyr)
library(dplyr, warn.conflicts=FALSE)
library(Rcpp)

#------------------------------
# set seed
#------------------------------

seed <- as.integer(commandArgs(trailingOnly=TRUE)[1])
if(is.na(seed)) seed <- 1000
cat("seed: ", seed, "\n - - - - - - - - - -\n")

set.seed(seed)

#------------------------------
# functions
#------------------------------

sourceCpp("matthews_cc.cpp")

source("load_numeric.r")
source("add_features.r")

#------------------------------
# main
#------------------------------

prj_name <- "submission_pre"
data_dir <- file.path("..", "..", "dataset")
input_dir <- file.path("..", "..", "input")
submission_dir <- file.path("..", "..", "submission", prj_name)

tic <- proc.time()

# train ----------

{} %>% 

# load train numeric
load_numeric(dataset="train") %>%

# add features
add_date_features(dataset="train") %>%
add_category_expand(dataset="train") %>%
add_is_duplicated(dataset="train") %>%
add_id_features(dataset="train") %>%

{

	# format data
	cat("==> Format data\n")
	index_valid <- sample(1:nrow(.), as.integer(nrow(.)/10))

	train_label_path <- file.path(data_dir, "train_numeric.csv")
	train_label <- fread(train_label_path, select="Response") %>% use_series(Response)

	valid_label <- train_label[index_valid]
	train_label <- train_label[-index_valid]

	dtrain <- xgb.DMatrix(.[-index_valid,], label=train_label, missing=0)
	dvalid <- xgb.DMatrix(.[index_valid,], label=valid_label, missing=0)

	list(dtrain=dtrain, dvalid=dvalid, train_label=train_label, valid_label=valid_label, colnames=colnames(.))

} %T>% {

	# set parameters
	param <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "auc",
					colsample_bytree = 0.752,
					subsample = 0.949,
					eta = 0.013,
					max.depth = 5,
					max_delta_step = 4,
					base_score = 0.332)

	# train model
	cat("==> Train model\n")
	model <<- xgb.train(param=param, data=.$dtrain, nrounds=2875L,
						print.every.n = 20L,
						watchlist=list(train=.$dtrain, valid=.$dvalid))

	importance <- xgb.importance(.$colnames, model=model)
	print(head(importance, 20))

} %>% {

	# predict
	train_pred <- predict(model, .$dtrain)
	valid_pred <- predict(model, .$dvalid)

	thresholds <- quantile(valid_pred, probs=seq(0,1,length=2000))
	global_threshold <<- search_best_threshold(valid_pred, .$valid_label, thresholds)

	# calc scores
	valid_score <<- calc_mcc(valid_pred, .$valid_label, global_threshold)
	train_score <<- calc_mcc(train_pred, .$train_label, global_threshold)
}

gc();gc();

# test ----------

{} %>% 

# load test numeric
load_numeric(dataset="test") %>%

# add features
add_date_features(dataset="test") %>%
add_category_expand(dataset="test") %>%
add_is_duplicated(dataset="test") %>%
add_id_features(dataset="test") %>%

# format data
xgb.DMatrix(missing=0) %>% 

# predict test label
predict(model, .) %>%
is_greater_than(global_threshold) %>% as.integer %T>%

{
	cat("==> count test_label\n")
	print(table(.))

} %>% {

	# generate submission data
	test_numeric_path <- file.path(data_dir, "test_numeric.csv")
	test_numeric_id <- fread(test_numeric_path, select="Id") %>% use_series(Id)
	submission <- data.frame(Id=test_numeric_id, Response=.)

} %>% {

	# save file
	cat("==> Save submission file\n")
	st <- function(score) return(sprintf("%.3f", score))
	submission_filename <- paste0(prj_name, "_train", st(train_score) ,"_valid", st(valid_score), "_seed", seed, ".csv")

	dir.create(submission_dir, showWarnings=FALSE, recursive=TRUE)
	write.table(., file.path(submission_dir, submission_filename), row.names=FALSE, sep=",")
}

# display computational time
cat("==> Display computational time\n")
proc.time() %>% subtract(tic) %>% print
