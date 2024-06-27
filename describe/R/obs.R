obs <-
function(data.obj, nonNA = FALSE) {
	if (is.data.frame(data.obj)) {
		if (nonNA) {
			counts <- lapply(data.obj, function(x) c(sum(!is.na(x)), sum(is.na(x))))
			cat(sprintf("%-10s %8s %8s", "Variable", "Non-NA", "NA"), "\n")
			for (varname in names(counts)) {
				cat(sprintf("%-10s %8i %8i", varname, counts[[varname]][1], counts[[varname]][2]), "\n")
			}
		} else {
			cat("n = ", nrow(data.obj), "\n", sep="")
		}
	} else if (is.vector(data.obj) || is.factor(data.obj)) {
		if (nonNA) {
			cat("n = ", sum(!is.na(data.obj)), "  (", sum(is.na(data.obj)), " NA)\n", sep="")
		} else {
			cat("n = ", length(data.obj), "\n", sep="")
		}
	} else {
		stop("This function works only with vectors, factors, and data frames.\n")
	}
	cat("\n")
}
