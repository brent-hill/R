distinct <-
function(x, show.vals = FALSE) {
	vals <- unique(na.omit(x))
	k <- length(vals)
	var_name <- deparse(substitute(x))
	cat("Variable:", var_name, "\n")
	cat("Number of distinct values:", k, "\n")
	if(show.vals) {
		cat("Values:", vals, "\n")
	}
	cat("\n")
}
