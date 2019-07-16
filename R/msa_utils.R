##' add logos to fortified alignment
##'
##'
##' @title infer_character_logos
##' @param y fortified alignment
##' @param font character font
##' @param ... params passed by other functions
##' @return tibble
##' @importFrom dplyr as_tibble
##' @export
##' @author guangchuang yu, Bradley R Jones
infer_character_logos <- function(y, font='helvetica_regular', ...) {
	data_sp <- make.chars(unique(y$character), font, attr(y, "baseClass"))
	
	y <- lapply(1:nrow(y), function(i) {
		d <- y[i, ]
		if (is.na(d$character)) {
			dd <- data.frame(x=NA,
							 y=NA,
							 x.logo=NA,
							 y.logo=NA,
							 order=1,
							 letter=NA,
							 seq_group=NA)
		} else {
			dd <- data_sp[[d$character]]
			dd$x.logo <- dd$x - min(dd$x) + d$position -.45
			if (d$character == '-') {
				dd$y.logo <- dd$y - min(dd$y) + d$ypos - 0.1
			} else {
				dd$y.logo <- dd$y - min(dd$y) + d$ypos -.45
			} 
		}
		dd$y <- d$y
		dd$position <- d$position
		dd$group <- paste0(d$position, d$ypos)
		dd$character <- d$character
		dd <- dd[, -1] # remove x column
		dd <- dd[order(dd$order),]
		return(dd)
	}) %>%
		do.call(rbind, .)
	
	y <- as_tibble(y)
	attr(y, "baseClass") <- attr(y, "baseClass")
	y
}

make.chars <- function(chars, font, base) {
	# Guess the seq type ourselves because Biotools::guessSerType
	# did not work for all the alignments tested
	seq_type <- if (base == 'DNAbin') {
		'DNA'
	} else if (base == 'AAbin') {
		'AA'
	} else {
		'auto'
	}
	
	chars <- chars[!is.na(chars)]
	
	data_sp <- lapply(chars, function(n) {
		if (n == '-') {
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
					   seq_type = seq_type)
		
		d$x <- d$x * .9/diff(range(d$x))
		d$y <- d$y * .9/diff(range(d$y))
		return(d)
	})
	
	names(data_sp) <- chars
	
	data_sp
}

##' @import ggseqlogo
logo_data <- getFromNamespace("logo_data", "ggseqlogo")
