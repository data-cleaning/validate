#' @include indicator.R
#' @include validator.R
NULL

# superclass for storing results of a verification activity
setRefClass("confrontation"
  ,fields = list(
      call  = "call"  # (user's) call that generated the object
    , value = "list"  # results of confrontation 
    , calls = "list"  # calls executed during confrontation
    , warn  = "list"  # list of 'warning' objects
    , error = "list"  # list of 'error' objects
  )
  , methods=list(
    show = function() .show_confrontation(.self)
  )
)

.show_confrontation <- function(.self){
  cat(sprintf("Reference object of class '%s'\n",class(.self)))
  cat(sprintf("Call:\n    ")); print(.self$call); cat('\n')
  cat(sprintf('Confrontations: %d\n', length(.self$calls)))
  cat(sprintf('Warnings      : %d\n',sum(sapply(.self$warn,function(w)!is.null(w)))))
  cat(sprintf('Errors        : %d\n',sum(sapply(.self$error,function(w)!is.null(w)))))
}


#' Confront data with a (set of) verifier(s)
#'
#' @param x An R object carrying verifications
#' @param y An R object carrying data
#' @param ... Arguments to be passed to other methods
#'
setGeneric("confront",
  def = function(x, y, ...) standardGeneric("confront")
)

setClassUnion('data',c("data.frame","list","environment"))


# indicators serve a different purpose than validations.
setRefClass("indicatorValue", contains = "confrontation")

#' @method confront data
#' @rdname confront
setMethod("confront",signature("indicator","data"),function(x,y,...){
  L <- lapply(x$calls,factory(eval), envir=y)
  new('indicatorValue',
      call = match.call()
      , calls = x$calls
      , value = lapply(L,"[[",1)
      , warn =  lapply(L,"[[",2)
      , error = lapply(L,"[[",3)     
  )  
})


setMethod('[',signature('confrontation'),function(x,i,j,...,drop=TRUE){
  new('confrontation',
      , call = x$call[i]
      , value = x$value[i]
      , warn = x$warn[i]
      , error  = x$error[i]
  )
})


# # indicators serve a different purpose than validations.
setRefClass("validatorValue", contains = "confrontation",
  fields = list(
        impact     = "list" # impact of mismatch on data
      , severity   = "list" # amount of mismatch between actual and desired score
    )
)


#' @method confront data
#' @rdname confront
setMethod("confront", signature("validator","data"),
  function(x, y
    , impact=c("none","Lp","rspa","FH")
    , severity=c("none","Lp","gower")
    , p=c(impact=2,severity=1), ...)
  {
    calls <- calls(x)
    L <- lapply(calls,factory(eval), envir=y,enclos=NULL)
    new('validatorValue',
        call = match.call(call=sys.call(1))
        , calls = calls
        , value = lapply(L,"[[",1)
        , warn =  lapply(L,"[[",2)
        , error = lapply(L,"[[",3)     
    )
  }
)
 
has_error <- function(x) !sapply(x$error,is.null)
has_warning <- function(x) !sapply(x$warn, is.null)
has_value <- function(x) sapply(x$value, function(a) !is.null(a))

passes <- function(x){
  sapply(x$value, function(a){
    ifelse( is.null(a), 0, sum(a,na.rm=TRUE)) 
  })
}

fails <- function(x){
  sapply(x$value, function(a){
    ifelse(is.null(a),0,sum(!a,na.rm=TRUE))
  })
}

nas <- function(x){
  sapply(x$value, function(a){
    ifelse(is.null(a),0,sum(is.na(a)))
  })
}

#' @method summary validatorValue
setMethod('summary',signature('validatorValue'),function(object,...){
  data.frame(
    validator = names(object$value)
    , confrontations = sapply(object$value,length)
    , passes = passes(object)
    , fails  = fails(object)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , call = sapply(object$calls,call2text)
  )  
})

#' Get values from object
#' 
#' @param x an R object
#' @param ... Arguments to pass to or from other methods
#'
#' @export
setGeneric('values',def=function(x,...) standardGeneric('values'))

#' @rdname values
setMethod('values',signature('confrontation'),function(x,...){
  x$value
})

#' @rdname values
setMethod('values',signature('validatorValue'),function(x,simplify=TRUE,...){
  if (!simplify ){
    return( getMethod(value,signature='confrontation')(x,...) )
  }
  values <- x$value[!has_error(x)]
  len <- sapply(values,length)
  lapply(unique(len), function(l){ 
    m <- sapply(values[len==l], Id)
    if ( l == 1 ) # edge case in sapply 
      m <- matrix(m,nrow=1,dimnames=list(NULL,names(m)))
    m
  })
})

#' @rdname calls
setMethod('calls',signature('confrontation'),function(x,...){
  x$calls
})

#' @rdname calls
setMethod('calls',signature('validatorValue'), function(x,simplify=TRUE,...){
  if (!simplify){
    return( getMethod(calls, signature='confrontation')(x,...) )
  }
  calls <- x$calls[!has_error(x)]
  len <- sapply(x$value[!has_error(x)],length)
  lapply(unique(len),function(l) sapply(calls[len==l],Id))
})



## TODO: decide whether we move all visualisations & reporting to a different package.

#' Create a visualisation
#' 
#' @param x an R object
#' @param ... parameters to be passed to \code{\link[graphics]{barplot}} but not 
#'  \code{height}, \code{horiz}, \code{border},\code{las}, \code{xlim}, and \code{las}.
#' @param add_legend Display legend?
#' @param add_calls Display rules?
#' @param colors Bar colors for validations yielding NA or a violation
#' @param topn If specified, plot only the top n most violated calls
#' @param order order the bars from most to least violated rules
#' 
#' 
#' @return A list, containing the bar locations as in \code{\link[graphics]{barplot}}
#'
#' @export 
setMethod('plot',signature('validatorValue'), 
  function(x, ..., order=TRUE, topn=Inf, add_legend=TRUE, add_calls=TRUE
           , colors=c("#A6CEE3CC", "#1F78B4CC") ){
    
    stopifnot(topn>0,is.logical(order),is.logical(add_legend),is.logical(add_calls))
    calls <- lapply(calls(x),sapply,call2text)
    val <- values(x)
    
    # values with different dimensionality are plotted in different row.
    par(mfrow=c(length(calls),1),xpd=TRUE)
    
    out <- lapply(seq_along(val), function(i){
      y <- val[[i]]
      count <- cbind(
        nna = colSums(is.na(y)),
        nvio = colSums(!y,na.rm=TRUE)
      )
      labels <- calls[[i]]

      # how to order
      if (order) count <- count[order(count[,'nvio']),,drop=FALSE]    
      if ( topn < Inf ){
        I <- order(count[,'nvio'],decreasing=TRUE)
        I <- 1:nrow(count) %in% I[1:min(topn,length(I))]
        count <- count[I,,drop=FALSE]
        labels <- calls[[i]][I]
      }
      # plot
      xlim = c(0, max(count))
      p = barplot(count[,'nna'],horiz=TRUE,border=NA,xlim=xlim,las=1,col=colors[1],...)
      for ( j in seq_along(p) ) drawbarat(p[j],count[j,'nvio'],0.4,col=colors[2])

      # labels & legend
      if (add_calls) text(0.1,p,labels,pos=4)
      if(add_legend){ 
        legend('topright'
         , legend = c('Missing','Violated')
         , fill=colors
         , border=colors
         , bty='n'
         , horiz=TRUE
         , inset=c(0,-0.1)
        )
      }
      p
    })
    par(xpd=FALSE)
    invisible(out)
})

drawbarat <- function(y,height,width,col){
  w <- width/2
  polygon(
    c(0,height,height,0)
    , c(y-w,y-w,y+w,y+w)
    ,col=col
    ,border=NA
  )
}




