freqtab <-
function(data, cml = FALSE, countNA = FALSE, pct = FALSE, dec = 2) {

	# Next steps:
	# Check object types of input data
	# - Allowed: Atomic vector, factor, or column from data frame
	# Need to check for numeric vector or factor/ordered
	# - Stop/throw error if other
	# Display NA as "<NA>" in output
	# If observed values are numerical and some are negative, insert "+" before positive values

	var_name <- deparse(substitute(data))
	if (grepl("\\$", var_name)) {        # easier to check object type?
		# Split the string into data frame and variable names
		# -- How to handle [["x"]] format???
		parts <- strsplit(var_name, "$", fixed=TRUE)[[1]]
		df_name <- parts[1]
		col_name <- parts[2]
		# Output the data frame and variable names
		cat("Data frame:", df_name, "\n")
		cat("Variable:  ", col_name, "\n")
	} else {
		# Output the variable name if not from a data frame
		cat("Variable:", var_name, "\n")
	}

	# Convert input vector to factor (if not already a factor)
	if(is.factor(data)==FALSE) {
		data <- factor(data)
	}

	# Set column width for observed values (column 1 of output)
	# - Convert factor to character vector
	# - Remove missing values (if necessary)
	# - Get length of each string
	# - Get max length
	max_val_length <- max(nchar(na.omit(as.character(data))))
	val_col_width <- ifelse(max_val_length<6, 6, max_val_length) + 1   # plus buffer (1)

	# Create a basic frequency table object
	if(countNA) {
		ftab <- table(data, useNA="always")
	} else {
		ftab <- table(data, useNA="no")
	}

	# Output container
	output <- list()

	# Distinct values of the variable (column 1)
	output[["Values"]] <- names(ftab)
	# Frequencies (column 2)
	output[["f"]] <- as.integer(ftab)
	# Relative frequencies (column 3)
	output[["rf"]] <- (100^pct)*as.numeric(prop.table(ftab))
	# Cumulative frequencies and cumulative relative frequencies
	if(cml) {
		output[["cf"]] <- as.integer(cumsum(ftab))
		output[["crf"]] <- (100^pct)*as.numeric(cumsum(prop.table(ftab)))
	}

	col_names <- names(output)
	num_rows <- length(output[["Values"]])

	# Format specs for each column
	fmt_head <- paste0("%-", val_col_width, "s %4s %7s")
	fmt_head_cml <- "%6s %8s"
	fmt_body <- paste0("%-", val_col_width, "s %4i %7.", dec, "f")
	fmt_body_cml <- paste0("%6i %8.", dec, "f")

	# Header row (column titles)
	cat("\033[1m")
	cat(sprintf(fmt_head, "Values", "f", "rf"))
	if(cml) {
		cat(sprintf(fmt_head_cml, "cf", "crf"))
	}
	cat("\033[22m\n")

	# Table content
	for (i in 1:num_rows) {
		cat(sprintf(fmt_body, output[["Values"]][i], output[["f"]][i], output[["rf"]][i]))
		if(cml) {
			cat(sprintf(fmt_body_cml, output[["cf"]][i], output[["crf"]][i]))
		}
		cat("\n")
	}
	cat("\n")
	invisible(ftab)
}
