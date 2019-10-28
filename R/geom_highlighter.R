##' add msa layer
##'
##'
##' @title geom_highlighter
##' @param mapping aesthetic mapping
##' @param data data
##' @param ... additional parameters passed to geom_tile
##' @return  ggplot msa layer
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 aes_
##' @export
##' @author Bradley R Jones
geom_highlighter <- function(mapping = NULL, data = NULL, ...) {
	default_aes <- aes_(x=~position, y=~y, fill=~character)
	
	if (is.null(mapping)) {
		mapping <- default_aes
	} else {
		mapping <- modifyList(default_aes, mapping)
	}
	
	geom_tile(mapping=mapping, data=data, ...)
}
