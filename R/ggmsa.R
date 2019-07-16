##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title ggmsa
##' @param fasta aligned fasta file
##' @param start start position to plot
##' @param end end position to plot
##' @param font character font
##' @param consensus either the name of the consensus sequence or e a character string of the consensus sequence or NA for no consensus highlighting
##' @param remove.blank character vector of characters to mark as NA (default: NULL)
##' @param palette name of msa colour palette
##' @param ... additional parameters passed to fortify and ggplot
##' @return ggplot object
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 scale_fill_discrete
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 coord_fixed
##' @importFrom tidyr %>%
##' @export
##' @author guangchuang yu, Bradley R Jones
ggmsa <- function(
	fasta,
	start = NULL,
	end = NULL,
	font = 'helvetica_regular',
	...,
	palette = NULL
) {
    aln <- read.fasta(fasta)
    
    mapping <- aes_(x=~position, y=~y, fill=~character, label=~character)
    mapping2 <- aes_(x=~x.logo, y=~y.logo, group=~factor(group))
    
    aln.f <- fortify(aln, start, end, logos=TRUE, ...)
    
    ggplot(aln.f, mapping=mapping, ...) +
		geom_msa(colour="grey") +
    	geom_seqlab(mapping=mapping2, fill="black") +
		scale_fill_msa(palette=palette) +
		theme_minimal() +
		theme(legend.position='none') +
    	xlab(NULL) +
    	ylab(NULL) +
    	coord_fixed()
}
