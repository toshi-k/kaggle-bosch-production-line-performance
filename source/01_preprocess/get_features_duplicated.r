
#------------------------------
# function
#------------------------------

count_num <- function(v){
	rep(length(v), length(v))
}

count_unique <- function(v){
	times <- joint_date[as.integer(names(v))]
	rep(length(unique(times)), length(v))
}

count_up <- function(v){

	if(length(v)==1) return(1)

	times <- joint_date[as.integer(names(v))]
	frank(times, ties.method="dense")
}

count_down <- function(v){

	if(length(v)==1) return(1)

	times <- joint_date[as.integer(names(v))]
	rev(frank(times, ties.method="dense"))
}

selfrank <- function(vec){

	names(vec) <- 1:length(vec)
	duplicated_count(vec, function(v)1:length(v), reset=FALSE)
}

get_selfrank <- function(v){

	if(length(v)==1) return(1)

	times <- joint_date[as.integer(names(v))]

	frank(times, ties.method="dense") %>% selfrank
}

get_response <- function(v, pos){

	if(length(v)==1) return(-1)

	times <- joint_date[as.integer(names(v))]

	if(length(unique(times)) < pos){
		f <- rep(-1, length(v))
		return(f)
	}

	times_rank <- frank(times, ties.method="dense")
	target_responses <- joint_response[names(v[times_rank == pos]) %>% as.integer]

	value <- as.integer(mean(target_responses, na.rm=TRUE) >= 0.5)

	r <- integer(length(v))

	for(i in seq_along(v)){
		if(times_rank[i] != pos){
			r[i] <- value
		}else{
			t <- times_rank == pos
			t[i] <- FALSE
			target_responsesi <- joint_response[names(v[t]) %>% as.integer]
			r[i] <- as.integer(mean(target_responsesi, na.rm=TRUE) >= 0.5)
		}
	}

	r[is.nan(r) | is.na(r)] <- -1
	r
}

keep_name_function <- function(vec, func, ...){

	global_count <<- global_count + 1
	if(global_count %% 100000 == 0) cat("\trow: ", sprintf("%d", global_count), "\n")

	ret <- func(vec, ...)
	names(ret) <- names(vec)
	ret
}

duplicated_count <- function(vec, func, reset=TRUE, ...){

	if(reset) global_count <<- 1

	count <- tapply(vec, vec, keep_name_function, func, simplify = FALSE, ...) %>% unlist
	index <- names(count) %>% strsplit("\\.",) %>% sapply(function(vec)vec[2]) %>% as.integer
	count[order(index)]
}

get_features_duplicated <- function(input){

	names_digest <- names(joint_digest)

	options(stringsAsFactors = FALSE)

	cat("get count_num feature\n")
	d1 <- data.frame(Id=names_digest, count_num=duplicated_count(joint_digest, count_num))
	cat("get count_unique feature\n")
	d2 <- data.frame(Id=names_digest, count_unique=duplicated_count(joint_digest, count_unique))
	cat("get count_up feature\n")
	d3 <- data.frame(Id=names_digest, count_up=duplicated_count(joint_digest, count_up))
	cat("get count_down feature\n")
	d4 <- data.frame(Id=names_digest, count_down=duplicated_count(joint_digest, count_down))

	cat("get selfrank\n")
	sr <- data.frame(Id=names_digest, self_rank=duplicated_count(joint_digest, get_selfrank))
	cat("get response_r1 feature\n")
	r1 <- data.frame(Id=names_digest, response_r1=duplicated_count(joint_digest, get_response, pos=1))
	cat("get response_r2 feature\n")
	r2 <- data.frame(Id=names_digest, response_r2=duplicated_count(joint_digest, get_response, pos=2))
	cat("get response_r3 feature\n")
	r3 <- data.frame(Id=names_digest, response_r3=duplicated_count(joint_digest, get_response, pos=3))

	options(stringsAsFactors = TRUE)

	data.frame(Id=names(input)) %>%
		left_join(d1) %>% left_join(d2) %>% left_join(d3) %>% left_join(d4) %>%
		left_join(sr) %>% left_join(r1) %>% left_join(r2) %>% left_join(r3)
}
