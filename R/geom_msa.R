##' add msa layer
##'
##'
##' @title geom_msa
##' @param mapping aesthetic mapping
##' @param data data
##' @param ... additional parameter passed to geom_tile
##' @return msa layer
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 aes_
##' @export
##' @author Bradley R Jones
geom_msa <- function(mapping=NULL, data=NULL, ...) {
	default_aes <- aes_(x=~x, y=~y, fill=~mut)
	
	if (is.null(mapping)) {
		mapping <- default_aes
	} else {
		mapping <- modifyList(default_aes, mapping)
	}
	
	geom_tile(mapping=mapping, data=data, ...)
}