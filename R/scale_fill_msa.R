##' Nucleotide and Amino acid colour scales
##'
##'
##' @title scale_fill_msa
##' @param palette name of msa colour palette
##' @param ... additional parameters passed to scale_fill_manual
##' @return ggplot fill scale
##' @importFrom ggplot2 scale_fill_manual
##' @export
##' @author guangchuang yu, Bradley R Jones
scale_fill_msa <- function(palette=NULL, ...) {
	if (is.null(palette))
		return(scale_fill_discrete(...))
	
	aa <- c(
		'H', 'D', 'E', 'K', 'N',
		'Q', 'R', 'M', 'I', 'L',
		'V', 'F', 'W', 'Y', 'C',
		'A', 'G', 'S', 'T', 'P'
	)
	
	na <- c(
		"A", "C", "G", "T", "U",
		"R", "Y", "K", "M", "S",
		"W", "B", "D", "H", "V",
		"N"
	)
	
	if (palette == 'lanl') {
		character.set <- c(aa, 'X', '*')
		values <- c(
			"#0000FF", # H
			rep("#302ECD", 2), # D, E
			rep("#23659B", 4), # L, N, Q, R
			"#2F9A2F", # M
			rep("#42FF00", 3), # I, L, V
			rep("#F900FF", 3), # F, W, Y
			"#CD2F2E", # C
			rep("#F9CE2E", 4), # A, G, S, T
			"#FBFF00", # P
			"#BEBEBE", # gap
			NA # other
		)
	} else {
		stop(paste0(palette, " not implemented."))
	}
	
	scale_fill_manual(breaks=character.set, limits=character.set, values=values, na.value=NA, ...)
}