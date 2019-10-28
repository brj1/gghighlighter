##' Nucleotide and Amino acid colour scales
##'
##'
##' @title scale_fill_highlighter
##' @param palette name of highlighter colour palette
##' @param ... additional parameters passed to scale_fill_manual
##' @return ggplot fill scale
##' @importFrom ggplot2 scale_fill_manual
##' @export
##' @author Bradley R Jones, guangchuang yu
scale_fill_highlighter <- function(
	palette = c(
		"Chemistry_AA","Shapely_AA","Zappo_AA","Taylor_AA", "LANL_AA",
		"Chemistry_Nucle","Shapely_Nucle","Zappo_Nucle","Taylor_Nucle"
	),
	...
) {
	palette <- match.arg(palette)
	
	if (grepl("AA", palette)) {
		values <- color_scheme_AA[[palette]]
		character.set <- color_scheme_AA[,1]  ##  The name of aminio acid.
	} else {
		values <- color_scheme_nucle[[palette]]
		character.set <- color_scheme_nucle[,1] ## The name of bases.
	}
	
	scale_fill_manual(breaks=character.set, limits=character.set, values=values, na.value=NA, ...)
}
