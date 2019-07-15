##' add msa layer
##'
##'
##' @title geom_msa
##' @param mapping aesthetic mapping
##' @param data data
##' @param ... additional parameters passed to geom_tile
##' @return  ggplot msa layer
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 aes_
##' @export
##' @author guangchuang yu, Bradley R Jones
geom_msa <- function(mapping = NULL, data = NULL, ...) {
	default_aes <- aes_(x=~position, y=~name, fill=~character)
	
	if (is.null(mapping)) {
		mapping <- default_aes
	} else {
		mapping <- modifyList(default_aes, mapping)
	}
	
	geom_tile(mapping=mapping, data=data, ...)
}