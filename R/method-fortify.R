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
##' @param start start position to plot
##' @param end end position to plot
##' @param consensus either the name of the consensus sequence or e a character string of the consensus sequence or NA for no consensus highlighting
##' @param remove.blank character vector of characters to mark as NA (default: NULL)
##' @param logos add logos
##' @param ... additional parameters passed to geom_tile
##' @return tibble
##' @importFrom tidyr gather
##' @importFrom tidyr %>%
##' @importFrom dplyr mutate_all
##' @importFrom dplyr mutate
##' @importFrom dplyr as_tibble
##' @export
##' @author guangchuang yu, Bradley R Jones
fortify_alignment_matrix <- function(
	x,
	start = NULL,
	end = NULL,
	consensus = NA,
	remove.blank = NULL,
	logos = FALSE,
	...
) {
	wide.df <- as.data.frame(x)
	df.names <- row.names(wide.df)
	
	# identify and apply consensus highlighting
	if (!is.na(consensus)[1]) {
		cons <- if (
			length(consensus) == 1 &&
			consensus %in% df.names
		) {
			wide.df[consensus, ] %>%
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
		wide.df <- apply(
			wide.df,
			1,
			function(sequence) {
				sequence[toupper(sequence) == toupper(cons)] <- NA
				sequence
			}
		) %>%
			t %>%
			as.data.frame
	}
	
	wide.df <- mutate_all(wide.df, . %>% as.character %>% toupper)
	wide.df$y <- df.names
	
	# convert from wide format for easier plotting
	df <- gather(wide.df, "position", "character", -y) %>%
		mutate(
			y=factor(y, levels=rev(df.names)),
			position=gsub("V", "", position) %>%
				as.numeric,
			character=if (!is.null(remove.blank)) {
				ifelse(
					character %in% remove.blank,
					NA,
					character
				)
			} else {
				character
			}
		) %>%
		mutate(ypos=as.numeric(y))
	 
	# clip alignment
	if (!is.null(start))
		df <- subset(df, start <= position)
	if (!is.null(start))
		df <- subset(df, end >= position)
	
	df <- as_tibble(df)
	
	# save base type
	attr(df, "baseClass") <- class(x)
	
	if (logos)
		df <- infer_character_logos(df)
	
	df
}
