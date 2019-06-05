##' @importFrom tidyr %>%
fortify.bin <- function(x, ...) {
	# convert to data frame
	alnmat <- lapply(
		seq_along(x),
		function(i)
			as.character(x[[i]])
	) %>%
		do.call('rbind',. )
	df <- alnmat %>%
		as.data.frame
	row.names(df) <- names(x)
	
	fortify_alignment_matrix(df, ...)
}

##' @importFrom ggplot2 fortify
##' @method fortify DNAbin
##' @export
fortify.DNAbin <-
	fortify.bin

##' @importFrom ggplot2 fortify
##' @method fortify AAbin
##' @export
fortify.AAbin <- 
	fortify.bin
	
##' fortify alignment matrix
##'
##'
##' @title fortify_alignment_matrix
##' @param x alignment matrix
##' @param consensus either the name of the consensus sequence or the a character string of the consensus sequence or NA for no ceonsenus highlighting
##' @param remove.blank set TRUE to mark unknown, missing and gaps as NA (default: TRUE)
##' @param ... additional parameters passed to geom_tile
##' @return tibble
##' @importFrom tidyr gather
##' @importFrom tidyr %>%
##' @importFrom dplyr mutate
##' @export
##' @author Bradley R Jones
fortify_alignment_matrix <- function(
	x,
	consensus = NA,
	remove.blank = TRUE,
	...
) {
	df <- as.data.frame(x)
	
	# identify and apply consensus highlighting
	if (!is.na(consensus)[1]) {
		cons <- if (
			length(consensus) == 1 &&
			consensus %in% row.names(df)
		) {
			df[consensus, ] %>%
				unlist
		} else {
			if (length(consensus) == 1) {
				consensus %>%
					strsplit("") %>%
					unlist
			} else {
				consensus
			}
		}
		df <- apply(
			df,
			1,
			function(x) {
				x[toupper(x) == toupper(cons)] <- NA
				x
			}
		) %>%
			t %>%
			as.data.frame
	}
	
	# convert from wide format for easier plotting
	df$y <- row.names(df)
	gather(df, "x", "mut", -y) %>%
		mutate(
			x=gsub("V", "", x) %>%
				as.numeric,
			mut=if (remove.blank) {
				ifelse(
					mut %in% c("X", "x", "-", "?"),
					NA,
					mut
				)
			} else {
				mut
			}
		)
}