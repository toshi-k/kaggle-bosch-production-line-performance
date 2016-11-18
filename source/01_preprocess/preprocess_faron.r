
# These features are motivated by faron's idea.
# Road-2-0.4+
# https://www.kaggle.com/mmueller/bosch-production-line-performance/road-2-0-4/code

#------------------------------
# library
#------------------------------

library(data.table)
library(magrittr)
library(dtplyr)
library(dplyr)

#------------------------------
# main
#------------------------------

data_dir <- file.path("..", "..", "dataset")
input_dir <- file.path("..", "..", "input")

tic <- proc.time()

# load data --------------------

train_numeric_pach <- file.path(data_dir, "train_numeric.csv") 
train_id <- fread(train_numeric_pach, data.table=FALSE, select="Id") %>% use_series(Id)

test_numeric_path <- file.path(data_dir, "test_numeric.csv")
test_id <- fread(test_numeric_path, data.table=FALSE, select="Id") %>% use_series(Id)

train_date_path <- file.path(input_dir, "train_date_features.csv")
train_date <- fread(train_date_path, data.table=FALSE, select=c("Id", "date_min"))

test_date_path <- file.path(input_dir, "test_date_features.csv")
test_date <- fread(test_date_path, data.table=FALSE, select=c("Id", "date_min"))

# joint data --------------------

joint_date <- rbind(train_date, test_date)

joint_id <- data.frame("Id" = c(train_id, test_id))
joint_id <- left_join(joint_id, joint_date, by="Id")

# generate faron features --------------------

joint_id$faron_feature_01 <- c(1, diff(joint_id$Id))
joint_id$faron_feature_02 <- c(rev(diff(rev(joint_id$Id))), -1)

joint_id <- joint_id %>% arrange(date_min, Id)

joint_id$faron_feature_03 <- c(1, diff(joint_id$Id))
joint_id$faron_feature_04 <- c(rev(diff(rev(joint_id$Id))), -1)

joint_id$date_min <- NULL

# save result --------------------

train <- data.frame("Id"=train_id) %>% left_join(joint_id, by="Id")
write.table(train, file.path(input_dir, "train_id_features.csv"), row.names=FALSE, quote=FALSE, sep=",")

test <- data.frame("Id"=test_id) %>% left_join(joint_id, by="Id")
write.table(test, file.path(input_dir, "test_id_features.csv"), row.names=FALSE, quote=FALSE, sep=",")

train$faron_feature_02[nrow(train)] <- -1
test$faron_feature_01[1] <- 1

# display computational time --------------------

cat("==> Display computational time\n")
calc <- proc.time() - tic
print(calc)
