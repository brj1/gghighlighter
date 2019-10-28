##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title gghighlighter
##' @param fasta aligned fasta file
##' @param start start position to plot
##' @param end end position to plot
##' @param palette name of highlighter colour scheme (5 amino acid color schemes, 4 nucleic acid color schemes.)
##' @param consensus either the name of the consensus sequence or e a character string of the consensus sequence or NA for no consensus highlighting
##' @param highlight.diff TRUE to highlight differences from consesnsus and FALSE to highlight similarities  (default: TRUE)
##' @param remove.blank character vector of characters to mark as NA (default: NULL)
##' @param ... additional parameters passed to fortify and ggplot
##' @return ggplot object
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 scale_fill_discrete
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 expand_scale
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 coord_fixed
##' @importFrom tidyr %>%
##' @export
##' @author Bradley R Jones, guangchuang yu
gghighlighter <- function(
	fasta,
	start=NULL,
	end=NULL,
	palette=c(
		"Chemistry_AA","Shapely_AA","Zappo_AA","Taylor_AA", "LANL_AA",
		"Chemistry_Nucle","Shapely_Nucle","Zappo_Nucle","Taylor_Nucle"
	),
	...
) {
	palette <- match.arg(palette)
	
    aln <- read.fasta(fasta)
    
    mapping <- aes_(x=~position, y=~y, fill=~character, label=~character)
    
    aln.f <- fortify(aln, start, end, ...)
    
    ggplot(aln.f, mapping=mapping, ...) +
		geom_highlighter() +
		scale_fill_highlighter(palette=palette) +
		theme_minimal() +
		theme(legend.position='none') +
    	scale_x_continuous(name="Position", expand=expand_scale(add=0)) +
    	ylab(NULL)
}