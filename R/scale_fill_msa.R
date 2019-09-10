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
scale_fill_msa <- function(palette = NULL, ...) {
	if (grepl("AA", color)) {
		values <- color_scheme_AA[[color]]
		character.set <- color_scheme_AA[,1]  ##  The name of aminio acid.
	} else {
		values <- color_scheme_nucle[[color]]
		character.set <- color_scheme_nucle[,1] ## The name of bases.
	}
	
	scale_fill_manual(breaks=character.set, limits=character.set, values=values, na.value=NA, ...)
}
