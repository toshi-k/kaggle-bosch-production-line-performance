
#------------------------------
# library
#------------------------------

library(Matrix)
library(magrittr)

#------------------------------
# functions
#------------------------------

add_date_features <- function(data, dataset="train"){

	cat(paste("==> Load", dataset, "date\n"))
	date_features_path <- file.path(input_dir, paste0(dataset, "_date_features.csv"))
	date_features <- fread(date_features_path) %>% data.matrix

	cBind(data, date_features[,-1])
}

add_category_expand <- function(data, dataset="train"){

	cat(paste("==> Load", dataset, "category\n"))
	categorical_expand_filename <- paste0(dataset, "_categorical_expand.csv")
	categorical_expand_path <- file.path(input_dir, categorical_expand_filename)
	categorical_expand <- fread(categorical_expand_path) %>% data.matrix

	cBind(data, categorical_expand[,-1])
}

add_is_duplicated <- function(data, dataset="train"){

	cat(paste("==> Load", dataset, "is duplicated\n"))
	is_duplicated_path <- file.path(input_dir, paste0(dataset, "_is_duplicated.csv"))
	is_duplicated <- fread(is_duplicated_path) %>% data.matrix

	cBind(data, is_duplicated[,-1])
}

add_id_features <- function(data, dataset="train"){

	cat(paste("==> Load", dataset, "id features\n"))
	id_features_filename <- paste0(dataset, "_id_features.csv")
	id_features_path <- file.path(input_dir, id_features_filename)
	id_features <- fread(id_features_path) %>% data.matrix

	cBind(data, id_features[,-1])
}
