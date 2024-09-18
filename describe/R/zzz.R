.onLoad <- function(libname, pkgname) {
	required_packages <- c("moments", "rlang")
	for (pkg in required_packages) {
		if (!requireNamespace(pkg, quietly = TRUE)) {
			stop(paste("Package", pkg, "is needed but not installed. Please install it."))
		}
	}
}
