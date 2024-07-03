dsnv <-
function(..., by = NULL, stats = "mean", dec = 3) {
	# Check if 'rlang' is installed and install it if necessary
	if(!requireNamespace("rlang", quietly = TRUE)) {
		install.packages("rlang")
	}
	library(rlang)
	if(!requireNamespace("moments", quietly = TRUE)) {
		install.packages("moments")
	}
	library(moments)

	# Capture the data specification using 'rlang'
	args <- enquos(...)

	# Function to extract variables and standardize the input format
	extract_vars <- function(args) {
		vars <- list()
		var_names <- c()
		df_mode <- FALSE
		df <- NULL

		for(arg in args) {
			if(df_mode) {
				# Treat arg as a column name
				col_name <- as_label(arg)
				vars <- c(vars, list(df[[col_name]]))
				var_names <- c(var_names, col_name)
			} else {
				eval_arg <- eval_tidy(arg)
				if(is.data.frame(eval_arg) && !df_mode) {
					df <- eval_arg
					df_mode <- TRUE
				} else {
					vars <- c(vars, list(eval_arg))
					var_names <- c(var_names, as_label(arg))
				}
			}
		}

		if(df_mode && length(vars) == 0) {
			vars <- as.list(df)
			var_names <- names(df)
		}

		list(vars = vars, var_names = var_names)
	}

	# Function to compute stats
	compute_stats <- function(vars, var_names) {
		.N <- sapply(vars, function(x) sum(!is.na(x)))
		.MEAN <- sapply(vars, function(x) mean(x, na.rm = TRUE))
		.P50 <- sapply(vars, function(x) median(x, na.rm = TRUE))
		.VAR <- sapply(vars, function(x) var(x, na.rm = TRUE))
		.SD <- sapply(vars, function(x) sd(x, na.rm = TRUE))
		.SKEW <- sapply(vars, function(x) skewness(x, na.rm = TRUE))
		.KURT <- sapply(vars, function(x) kurtosis(x, na.rm = TRUE))
		stats_df <- data.frame(Variable = var_names,
							   n = .N,
							   Mean = round(.MEAN, 3),
							   Median = .P50,
							   Var = round(.VAR, 3),
							   SD = round(.SD, 3),
							   Skew = round(.SKEW, 3),
							   Kurt = round(.KURT, 3))
		rownames(stats_df) <- NULL
		stats_df
	}

	# Process input data specification and pass to computation function
	input <- extract_vars(args)
	results <- compute_stats(input$vars, input$var_names)

	return(results)
	# print(str(results))
}
