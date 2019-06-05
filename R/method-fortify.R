##' @importFrom tidyr gather
##' @importFrom tidyr %>%
##' @importFrom dplyr mutate
fortify.bin <- function(x, consensus=NA, remove.blank=T, ...) {
	# convert to data frame
	alnmat <- lapply(
		seq_along(x),
		function(i)
			toupper(as.character(x[[i]]))
	) %>%
		do.call('rbind',. )
	df <- alnmat %>%
		as.data.frame
	row.names(df) <- names(x)
	
	# identify and apply consensus highlighting
	if (!is.na(consensus)[1]) {
		cons <- if (
			length(consensus) == 1 &&
			consensus %in% row.names(df)
		) {
			df[consensus, ]
		} else {
			toupper(
				if (length(consensus) == 1) {
					consensus %>%
						strsplit("") %>%
						unlist
				} else {
					consensus
				}
			)
		}
		df <- apply(
			df,
			1,
			function(x) {
				x[x == cons] <- NA
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