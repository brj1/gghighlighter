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
##' @param font character font
##' @param consensus either the name of the consensus sequence or the a character string of the consensus sequence or NA for no ceonsenus highlighting
##' @param remove.blank character vector of characters to mark as NA (default: NULL)
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
	font = 'helvetica_regular',
	consensus = NA,
	remove.blank = NULL,
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
	wide.df$name <- df.names
	
	# convert from wide format for easier plotting
	df <- gather(wide.df, "position", "character", -name) %>%
		mutate(
			name=factor(name, levels=rev(df.names)),
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
		mutate(ypos=as.numeric(name))
	 
	# clip alignment
	if (!is.null(start))
		df <- subset(df, start <= position)
	if (!is.null(start))
		df <- subset(df, end >= position)
	
	# get and combine character logos from ggseqlogo
	data_sp <- make.chars(unique(df$character), font)
	
	df <- lapply(1:nrow(df), function(i) {
		d <- df[i, ]
		dd <- data_sp[[d$character]]
		dd$x <- dd$x - min(dd$x) + d$position -.45
		if (d$character == '-') {
			dd$y <- dd$y - min(dd$y) + d$ypos - 0.1
		} else {
			
			dd$y <- dd$y - min(dd$y) + d$ypos -.45
		}
		dd$name <- d$name
		dd$position <- d$position
		dd$group <- paste0(d$position, d$ypos)
		dd$character <- d$character
		dd <- dd[order(dd$order),]
		return(dd)
	}) %>%
		do.call(rbind, .)
	
	df <- as_tibble(df)
	
	# save base type
	attr(df, "baseClass") <- class(x)
	
	df
}

make.chars <- function(chars, font) {
	data_sp <- lapply(chars, function(n) {
		if (is.na(n) || n == '-') {
			d <- data.frame(x = c(0.05, 0.95, 0.95, 0.05),
							y = c(0.05, 0.05, 0.2, 0.2),
							letter = n,
							position = 1,
							order = 1:4,
							seq_group=n)
			return(d)
		}
		d <- logo_data(seqs = n,
					  font = font,
					  seq_group = n,
					  seq_type = "auto")
		
		d$x <- d$x * .9/diff(range(d$x))
		d$y <- d$y * .9/diff(range(d$y))
		return(d)
	})
	
	names(data_sp) <- chars
	
	data_sp
}

##' @import ggseqlogo
logo_data <- getFromNamespace("logo_data", "ggseqlogo")