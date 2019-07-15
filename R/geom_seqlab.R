##' add sequence label layer
##'
##'
##' @title geom_msa
##' @param mapping aesthetic mapping
##' @param data data
##' @param ... additional parameters passed to geom_polygon
##' @return  ggplot sequence label layer
##' @importFrom ggplot2 geom_polygon
##' @importFrom ggplot2 aes_
##' @export
##' @author guangchuang yu, Bradley R Jones
geom_seqlab <- function(mapping = NULL, data = NULL, ...) {
	default_aes <- aes_(x=~x, y=~y, group=~factor(group))
	
	if (is.null(mapping)) {
		mapping <- default_aes
	} else {
		mapping <- modifyList(default_aes, mapping)
	}
	
	geom_polygon(mapping=mapping, data=data, ...)
}