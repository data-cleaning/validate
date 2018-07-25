#' Plot the validator object
#' 
#' The matrix of variables x rules is plotted, in which linear rules
#' are differently colored. 
#' The augmented matrix is returned, but can also be calculated using
#' \code{variables(x, as="matrix")}.
#' 
#' @export
#' @param x validator object with rules
#' @param y not used
#' @param use_blocks \code{logical} if \code{TRUE} the matrix is sorted
#' according to the connected sub sets of variables (aka blocks).
#' @param col \code{character} with color codes for plotting variables. 
#' @param ... passed to image
#' @return (invisible) the matrix
#' @seealso \code{\link{variables}}, \code{x$blocks()}e
#' @examples ../examples/plot.R
plot.validator <- function( x
                          , y
                          , use_blocks = TRUE
                          , col = c("#33CC33", "#5555CC")
                          , cex = 1
                          , ...
                          ){
  use_blocks <- isTRUE(use_blocks)
  
  blocks <- if (use_blocks){
    x$blocks()
  }
  
  A <- variables(x, as = "matrix")
  
  Z <- A
  Z[A] <- 1
  Z[A & x$is_linear()] <- 2
  is.na(Z) <- Z == 0
  
  if (use_blocks){
    # change row order, so blocks are identifyable
    rule_order <- unlist(blocks)
    
    var_order <- unlist(lapply(blocks, function(b){variables(x[b])}))
    Z <- Z[rule_order, var_order]
  }
  Z <- t(Z)
  
  graphics::image( x = seq_len(nrow(Z))
       , y = seq_len(ncol(Z))
       , z = Z
       , col = col
       , las = 1
       , xlab = "variables"
       , ylab= "rules"
       , xaxt = "n"
       , yaxt = "n"
       , ...
       )
  
  var_text <- which(Z > 0, arr.ind = TRUE)
  var_text <- data.frame(var_text)
  var_text$labels <- colnames(A)[var_text$variable]
  text( x = var_text$variable
      , y = var_text$rule
      , labels = var_text$labels
      , cex = cex
      )
  
  if (use_blocks){
    h <- sapply(x$blocks(), length)
    h <- cumsum(h)
    v <- lapply(blocks, function(b){variables(x[b])})
    v <- sapply(v, length)
    v <- cumsum(v)
    graphics::abline( h = 0.5 + h
          , v = 0.5 + v
          , lty = 2
          , col="gray")
  }
  invisible(list(Z = Z))
}

#' Plot a confrontation object
#' 
#' The plot function for the confrontation object is identical to the \code{\link{barplot}} 
#' method.
#' @param x a confrontation object.
#' @param y not used
#' @param ... passed to \code{barplot}
#' @export
#' @examples ../examples/plot.R
plot.confrontation <- function(x, y, ...){
  barplot(x, ...)
}
