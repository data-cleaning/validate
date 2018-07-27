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
#' @param cex size of the variables plotted.
#' @param show_legend should a legend explaining the colors be drawn?
#' @param ... passed to image
#' @return (invisible) the matrix
#' @seealso \code{\link{variables}}, \code{x$blocks()}e
#' @example ../examples/plot.R
setMethod("plot","validator"
  , function( x
    , y
    , use_blocks = TRUE
    , col = c("#b2df8a", "#a6cee3") # Colorbrewer "Paired"
    , cex = 1
    , show_legend = TRUE
    , ...
    ){
  use_blocks <- isTRUE(use_blocks)
  show_legend <- isTRUE(show_legend)
  
  if (length(x) < 1){
    message("No rules to be plotted")
    return(invisible())
  }
  
  blocks <- if (use_blocks){
    x$blocks()
  }
  
  A <- variables(x, as = "matrix")
  
  Z <- A
  Z[A] <- 2
  Z[A & x$is_linear()] <- 1
  
  is.na(Z) <- Z == 0
  
  if (use_blocks){
    # change row order, so blocks are identifyable
    rule_order <- unlist(blocks)
    
    var_order <- unlist(lapply(blocks, function(b){variables(x[b])}))
    Z <- Z[rule_order, var_order, drop = FALSE]
  }
  Z <- t(Z)
  
  ylim <- c(1, ncol(Z)) + c(-0.5, 0.5)
  if (show_legend){
    ylim[2] <- ylim[2] +  1 # needs extra space for legend
  }
  
  graphics::image( x = seq_len(nrow(Z))
       , y = seq_len(ncol(Z))
       , z = Z
       , col = col
       , las = 1
       , xlab = "variables"
       , ylab= "rules"
       , ylim= ylim
       , xaxt = "n"
       , yaxt = "n"
  #     , ...
       )
  # label the y-axis with rule names
  axis(2, at=seq_len(ncol(Z)), labels = colnames(Z), las=1)
  
  var_text <- which(Z > 0, arr.ind = TRUE)
  var_text <- data.frame(var_text)
  var_text$labels <- colnames(A)[var_text$variable]
  # variables
  text( x = var_text$variable
      , y = var_text$rule
      , labels = var_text$labels
      , cex = cex
      )
  
  if (use_blocks){
    h <- sapply(x$blocks(), length)
    h <- c(0,cumsum(h)) + 0.5
    v <- lapply(blocks, function(b){variables(x[b])})
    v <- sapply(v, length)
    v <- c(0,cumsum(v)) + 0.5

    graphics::rect( xleft = utils::head(v, -1)
                  , xright = utils::tail(v, -1)
                  , ybottom = utils::head(h, -1)
                  , ytop = utils::tail(h, -1)
                  , lty = 2
                  , border ="gray30")
  }
  
  if (show_legend){
    legend("topright", legend = c("linear rule", "other"), fill=col)
  }
  
  F <- factor(Z, levels=c(1,2), labels = c("linear", "other"))
  dim(F) <- dim(Z)
  dimnames(F) <- dimnames(Z)
  
  invisible(F)
})

#' Plot a confrontation object
#' 
#' The plot function for the confrontation object is identical to the \code{\link{barplot}} 
#' method.
#' @param x a confrontation object.
#' @param y not used
#' @param ... passed to \code{barplot}
#' @export
#' @example ../examples/plot.R
setMethod("plot","confrontation", function(x, y, ...){
  barplot(x, ...)
})
