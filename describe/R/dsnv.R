dsnv <- function(data.frame, ..., by = NULL, stats = "n mean", dec = 2) {

	# Capture the list of names of numeric variables
	num.vars <- eval(substitute(alist(...)))

	# Convert the list of numeric variables into character vector
	numvar.names <- sapply(num.vars, deparse)

	# If grouping variable is specified, capture its name as a character string
	if (!missing(by)) {
		grouping_var <- deparse(substitute(by))
	} else {
		grouping_var <- NULL
	}

	# Split the stats spec string into individual components and convert to lowercase
	stats <- tolower(unlist(strsplit(stats, " ")))

	# Function to compute mean, standard deviation, and sample size
	compute_stats <- function(x, stats) {
		mean_value <- mean(x, na.rm = TRUE)
		sd_value <- sd(x, na.rm = TRUE)
		n_value <- sum(!is.na(x))
		result <- c()
		if ("n" %in% stats) {
			result <- c(result, n_value)
		}
		if ("mean" %in% stats) {
			result <- c(result, mean_value)
		}
		if ("sd" %in% stats) {
			result <- c(result, sd_value)
		}
		return(result)
	}

	# Determine the column names based on selected statistics
	col_names <- c("Levels")
	if ("n" %in% stats) {
		col_names <- c(col_names, "n")
	}
	if ("mean" %in% stats) {
		col_names <- c(col_names, "Mean")
	}
	if ("sd" %in% stats) {
		col_names <- c(col_names, "SD")
	}

	# Prepare output container
	output <- list()

	# Compute statistics
	for (var in numvar.names) {
		if (!is.null(grouping_var)) {
			levels <- as.character(unique(data.frame[[grouping_var]]))
			var_output <- matrix(nrow = length(levels), ncol = length(col_names))
			colnames(var_output) <- col_names
			rownames(var_output) <- levels
			for (i in seq_along(levels)) {
				level <- levels[i]
				subset_data <- data.frame[data.frame[[grouping_var]] == level, var, drop = FALSE]
				stats_values <- compute_stats(subset_data[[var]], stats)
				var_output[i, ] <- c(level, stats_values)
			}
			output[[var]] <- var_output
		} else {
			stats_values <- compute_stats(data.frame[[var]], stats)
			var_output <- matrix(stats_values, nrow = 1, ncol = length(col_names) - 1)
			colnames(var_output) <- col_names[-1]
			rownames(var_output) <- var
			output[[var]] <- var_output
		}
	}

	# Print the results
	print_output <- function(output, grouping_var, col_names, dec) {
		if (!is.null(grouping_var)) {
			for (var in names(output)) {
				cat(sprintf("%s\n", var))
				cat(sprintf("%-10s", "Levels"))
				for (name in col_names[-1]) {
					cat(sprintf(" %10s", name))
				}
				cat("\n")
				apply(output[[var]], 1, function(row) {
					cat(sprintf("%-10s", row[1]))
					for (j in 2:length(row)) {
						if (col_names[j] == "n") {
							cat(sprintf(" %10d", as.integer(row[j])))
						} else {
							cat(sprintf(paste0(" %10.", dec, "f"), as.numeric(row[j])))
						}
					}
					cat("\n")
				})
				cat("\n")
			}
		} else {
			cat(sprintf("%-15s", "Variable"))
			for (name in col_names[-1]) {
				cat(sprintf(" %10s", name))
			}
			cat("\n")
			for (var in names(output)) {
				row <- output[[var]]
				cat(sprintf("%-15s", var))
				for (j in 1:length(row[1, ])) {
					if (colnames(row)[j] == "n") {
						cat(sprintf(" %10d", as.integer(row[1, j])))
					} else {
						cat(sprintf(paste0(" %10.", dec, "f"), as.numeric(row[1, j])))
					}
				}
				cat("\n")
			}
			cat("\n")  # Added extra newline here
		}
	}

	print_output(output, grouping_var, col_names, dec)
}
