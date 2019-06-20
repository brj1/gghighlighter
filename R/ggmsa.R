##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title ggmsa
##' @param fasta aligned fasta file
##' @param fill.scale fill scale
##' @param palette name of msa colour palette
##' @param consensus either the name of the consensus sequence or the a character string of the consensus sequence or NA for no ceonsenus highlighting
##' @param remove.blank character vector of characters to mark as NA (default: NULL)
##' @param ... additional parameters passed to ggplot
##' @return ggplot object
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 scale_fill_discrete
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_discrete
##' @export
##' @author guangchuang yu, Bradley R Jones
ggmsa <- function(
	fasta,
	palette = NULL,
	...
) {
    aln <- read.fasta(fasta)
    
    mapping <- aes_(x=~x, y=~y, fill=~mut, label=~mut)
    	
    ggplot(aln, mapping=mapping, ...) +
		geom_msa() +
		scale_fill_msa(palette=palette) +
		theme_minimal() +
		theme(legend.position='none') +
		scale_x_continuous(name="Position", expand=c(0, 0)) +
		scale_y_discrete(name="Sequence ID")
}