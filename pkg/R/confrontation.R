#' @include validator.R
#' @include indicator.R
NULL

# superclass for storing results of a verification activity
setRefClass("confrontation"
  ,fields = list(
      ._call  = "call"  # (user's) call that generated the object
    , ._value = "list"  # results of confrontation 
    , ._calls = "list"  # calls executed during confrontation
    , ._warn  = "list"  # list of 'warning' objects
    , ._error = "list"  # list of 'error' objects
  )
  , methods=list(
    show = function() .show_confrontation(.self)
  )
)

.show_confrontation <- function(.self){
  cat(sprintf("Object of class '%s'\n",class(.self)))
  cat(sprintf("Call:\n    ")); print(.self$._call); cat('\n')
  cat(sprintf('Confrontations: %d\n', length(.self$._calls)))
  cat(sprintf('Warnings      : %d\n',sum(sapply(.self$._warn,function(w)!is.null(w)))))
  cat(sprintf('Errors        : %d\n',sum(sapply(.self$._error,function(w)!is.null(w)))))
}

#' @rdname select
#' @export 
setMethod('[',signature('confrontation'),function(x,i,j,...,drop=TRUE){
  new(class(x)
      , ._call = match.call(call=sys.call(sys.parent()))
      , ._calls = x$._calls[i]
      , ._value = x$._value[i]
      , ._warn = x$._warn[i]
      , ._error  = x$._error[i]
  )
})



#' Confront data with a (set of) expressionset(s)
#'
#' @aliases confrontation validation indication
#'
#' @param x An R object carrying verifications
#' @param dat An R object carrying data
#' @param ref Optionally, an R object carrying reference data. See examples for usage.
#' @param ... Options used at execution time (especially \code{'raise'}). See \code{\link{validate_options}}.
#' 
#' 
#' @section Using reference data:
#' When reference data sets are given, it is assumed that rows in the reference data
#' are ordered corresponding to the rows of \code{dat}, except when a \code{key} is specified.
#' In that case, all reference datasets are matched against the rows of \code{dat} using \code{key}
#' Nonmatching records are removed from datasets in \code{ref}. If there are records in \code{dat} 
#' that are not in \code{ref}, then datasets in \code{ref} are extended with records containing only \code{NA}.
#' In particular, this means that wen reference data is passed in an environment, those reference data sets
#' may altered by the call to \code{cofront}.
#'
#' Technically, reference data will be stored in an environment that is the parent of a (created) environment that
#' contains the columns of \code{dat}.
#' 
#' 
#' @seealso \code{\link{summary}}, \code{\link{aggregate,validation-method}}, \code{\link{sort,validation-method}}
#' 
#' @export 
#' 
#' @example ../examples/confront.R
setGeneric("confront",
  def = function(x, dat, ref, ...) standardGeneric("confront")
)


## The below function is a worker that assumes all relevant data is present in 
## an environment, possibly with a parent containing reference data. Most, if not
## all R-based methods will convert to this form and call the worker.
##
## x a validator object
## dat an environment
## key a character indicating a key.
##
confront_work <- function(x,dat,key=NULL,class='confrontation',...){
  calls <- x$calls(varlist=variables(dat))
  # merge options with clone of option manager 
  opts <- x$clone_options(...)
  L <- execute(calls,dat,opts)
  if (!is.null(key)) L <- add_names(L,x,dat,key)
  new(class,
      ._call = match.call(call=sys.call(sys.parent(2)))
      , ._calls = x$calls(expand_assignments=TRUE,varlist=variables(dat))
      , ._value = lapply(L,"[[",1)
      , ._warn =  lapply(L,"[[",2)
      , ._error = lapply(L,"[[",3)
  )
}



#' @rdname variables
setMethod('variables',signature('data.frame'), function(x,...) names(x))

#' @rdname variables
setMethod('variables',signature('list'), function(x,...) names(x))

#' @rdname variables
setMethod('variables',signature('environment'), function(x,...) ls(x))


# indicators serve a different purpose than validations.
setRefClass("indication", contains = "confrontation")

#' @rdname confront
setMethod("confront", signature("indicator","data.frame"), function(x,dat,key=NULL,...){
  dat <- list2env(dat)
  confront_work(x,dat,key,'indication',...)
})


#' @rdname validate-summary
#' @param object An R object
#' @param ... Currently unused
#'
#' @aliases validate-summary
#' @section Indication:
#' Some basic information per evaluated indicator is reported: the number of items to which the 
#' indicator was applied, the output \code{class}, some statistics (min, max, mean , number of NA)
#' and wether an exception occurred (warnings or errors). The evaluated expression is reported as well.
#' 
#' 
#' @export 
setMethod('summary',signature('indication'), function(object,...){
  data.frame(
    indicator = names(object$._value)
    , items = sapply(object$._value,length)
    , class = get_stat(object,class)
    , min = get_stat(object,min,na.rm=TRUE)
    , mean  = get_stat(object,mean,na.rm=TRUE)
    , max = get_stat(object,max,na.rm=TRUE)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , expression = sapply(object$._calls,call2text)
    , row.names=NULL
    , stringsAsFactors=FALSE
  )  
})


# # indicators serve a different purpose than validations.
setRefClass("validation", contains = "confrontation")

#' @rdname confront
#' @param key (optional) name of identifying variable in x.
setMethod("confront", signature("validator","data.frame"), function(x, dat, key=NULL, ...){
  dat <- list2env(dat)
  confront_work(x,dat,key,'validation',...)
})


#' @rdname confront
setMethod("confront",signature("validator","data.frame","environment"), function(x, dat, ref, key=NULL, ...){
  classes <- sapply( ls(ref), function(x) class(ref[[x]]) )
  if ( !all(class(dat) == classes)  )
    stop("Class of one or more elements in 'ref' differs from 'dat'")
  if (!is.null(key)) match_rows(of=ref, against=dat, using=key)
  dat <- list2env(dat,parent=ref)  
  confront_work(x,dat,key,...)
})

#' @rdname confront
setMethod("confront",signature("validator","data.frame","data.frame"),function(x,dat,ref, key=NULL,...){
  env <- new.env()
  env$ref <- ref
  if (!is.null(key)) match_rows(of=env, against=dat, using=key)
  dat <- list2env(dat, parent=env)
  confront_work(x, dat, key, ...)
})

#' @rdname confront
setMethod("confront",signature("validator","data.frame","list"),function(x,dat,ref,key=NULL,...){
  classes <- sapply(ref,class)
  if ( !all(class(dat) == classes)  )
    stop("Class of one or more elements in 'ref' differs from 'dat'")
  env <- list2env(ref)  
  if (!is.null(key)) match_rows(of=ref, against=dat, using=key)
  dat <- list2env(dat,parent=env)  
  confront_work(x,dat,key,...)  
})



# match rows; prepare for 'left join'.
# of     : an environment containing data.frames
# against: a reference data.frame to match againsty.
# using  : a key (character)
match_rows <- function(of, against, using){  
  key1 <- against[,using]
  for ( nm in ls(of) ){
    i <- match(key1, of[[nm]][,using], nomatch = nrow(of) + 1)
    of[[nm]] <- of[[nm]][i,,drop=FALSE]
  }
}


add_names <- function(L,x,y,key){
  keys <- y[[key]]
  nkey <- length(keys)
  L <- lapply(L,function(v){ 
    if ( length(v[[1]]) == nkey ) 
      v[[1]] <- setNames(v[[1]], keys)   
    v
  })
}  

# execute calls. 
# - Assignments are stored in a separate environment and forgotten afterwards.
# - Failed assignments yield a warning.
execute <- function(calls,env,opts){
  lapply(calls, function(g) 
    if ( g[[1]] == ":=" ){ 
      var <- as.character(left(g))
      if ( var %in% variables(env) ) 
        warning(sprintf("Locally overwriting variable '%s'",var))
        assign(var, tryCatch( eval(right(g), env), error=warning), envir=env)
    } else { 
      factory(eval,opts)(g, env)
    }
  )[!is.assignment(calls)]
}

# x inherits from 'confrontation'
has_error <- function(x) !sapply(x$._error,is.null)
has_warning <- function(x) !sapply(x$._warn, is.null)
has_value <- function(x) sapply(x$._value, function(a) !is.null(a))

passes <- function(x){
  sapply(x$._value, function(a){
    ifelse( is.null(a)
      , 0
      , ifelse( is.logical(a)
         , sum(a,na.rm=TRUE)      # case of regular rule
         , sum(a == seq_along(a)) # case of FD
        )
    )
  })
}

fails <- function(x){
  sapply(x$._value, function(a){
    ifelse( is.null(a)
      , 0
      , ifelse( is.logical(a)
          , sum(a,na.rm=TRUE)      # case of regular rule
          , sum(a != seq_along(a)) # case of FD
        )
    )
  })
}

nas <- function(x){
  sapply(x$._value, function(a){
    ifelse(is.null(a),0,sum(is.na(a)))
  })
}


#' @rdname validate-summary
#' @section Validation:
#' Some basic information per evaluated validation rule is reported: the number of items to which the 
#' rule was applied, the output \code{class}, some statistics (passes, fails, number of NA)
#' and wether an exception occurred (warnings or errors). The evaluated expression is reported as well.
#' 
setMethod('summary',signature('validation'),function(object,...){
  data.frame(
    rule = names(object$._value)
    , items = sapply(object$._value,length)
    , passes = passes(object)
    , fails  = fails(object)
    , nNA = nas(object)
    , error = has_error(object)
    , warning = has_warning(object)
    , expression = sapply(object$._calls,  call2text)
    , row.names=NULL
    , stringsAsFactors=FALSE
  )  
})


### VALUES ----------------------------------------------------------------------


#' Get values from object
#' 
#' @aliases severity, impact
#' 
#' @param x an R object
#' @param ... Arguments to pass to or from other methods
#'
#' @export
setGeneric('values',def=function(x,...) standardGeneric('values'))

#' @rdname values
setMethod('values',signature('confrontation'),function(x,...){
  x$._value
})

#' @rdname values
#' @param simplify Combine results with similar dimension structure into arrays?
#' @param drop if a single vector or array results, drop 'list' attribute?
setMethod('values',signature('validation'),function(x,simplify=TRUE,drop=TRUE,...){
  int_values(x,simplify,drop,...)
})

#' @rdname values
setMethod('values',signature('indication'),function(x,simplify=TRUE,drop=TRUE,...){
  int_values(x,simplify,drop,...)
})

int_values <- function(x,simplify,drop,...){
  out <- if ( simplify ){
    simplify_list(x$._value[!has_error(x)])
  } else {
    getMethod(values,signature='confrontation')(x,...)
  }
  if (drop && length(out) == 1) out[[1]] else out
}


simplify_list <- function(L){
  len <- sapply(L,num_result)
  lapply(unique(len), function(l){
    m <- sapply(L[len==l], get_result)
    if ( l == 1 )
      m <- matrix(m,nrow=1,dimnames=list(NULL,names(m)))
    m
  })
}


### WARNINGS AND ERRORRS -----------------------------------------------------

#' Get error messages from confrontation object
#' @param x An object of class \code{confrontation}
#' @param ... Arguments to be passed to other methods.
#' @export 
setGeneric("errors",def = function(x,...) standardGeneric("errors"))

#' @rdname errors
setMethod("errors","confrontation",function(x,...){
  i <- has_error(x)
  x$._error[i]
})


setGeneric("warnings")

#' @rdname errors
#' @export 
setMethod("warnings","confrontation",function(x,...){
  i <- has_warning(x)
  x$._warn[i]
})



### SORT AND VALIDATION -------------------------------------------------------
setGeneric('aggregate')

#' Aggregate validation results
#' 
#' Aggregate results of a validation.
#'  
#' @param x An object of class \code{\link{validation}}
#' @param by Report on violations per rule (default) or per record?
#' @param drop drop list attribute if the result has a single argument.
#' @param ... Arguments to be passed to or from other methods.
#'
#' @return By default, a \code{data.frame} with the following columns.
#' \tabular{ll}{
#'   \code{npass} \tab Number of items passed\cr
#'   \code{nfail} \tab Number of items failing\cr
#'   \code{nNA} \tab Number of items resulting in \code{NA}\cr
#'   \code{rel.pass} \tab Relative number of items passed\cr
#'   \code{rel.fail} \tab Relative number of items failing\cr
#'   \code{rel.NA} \tab Relative number of items resulting in \code{NA}
#' }
#' If \code{by='rule'} the relative numbers are computed with respect to the number 
#' of records for which the rule was evaluated. If \code{by='record'} the relative numbers
#' are computed with respect to the number of rules the record was tested agains. 
#'
#' When \code{by='record'} and not all validation results have the same dimension structure,
#' a list of \code{data.frames} is returned.
#' 
#' @seealso \code{\link{aggregate}}, \code{\link{summary}}
#'   
#' @export
setMethod('aggregate',signature('validation'), function(x,by=c('rule','record'), drop=TRUE,...){
  v <- values(x, drop=FALSE)
  by <- match.arg(by)
  aggr <- if ( by == 'rule') colSums else rowSums
  ntot <- if ( by == 'rule') nrow else ncol
  L <- lapply(v, function(y){
    s <- aggr(y,na.rm=TRUE)
    s <- s
    na <- aggr(is.na(y))
    N <- ntot(y)
    nfail = N - s - na
    data.frame( 
      npass = s
      , nfail = nfail 
      , nNA = na
      , rel.pass  = s/N
      , rel.fail  = nfail/N
      , rel.NA = na/N
    )
  })
  if ( length(L) == 1 && drop ) L <- L[[1]]
  if ( by== 'rule' && !is.data.frame(L) ) L <- do.call(rbind,L)
  L
})




setGeneric('sort')

#' Aggregate and sort the results of a validation.
#'   
#' @param x An object of class \code{\link{validation}}
#' @param by Report on violations per rule (default) or per record?
#' @param drop drop list attribute if the result has a single argument.
#' @param decreasing Sort by decreasing number of passes?
#' @param ... Arguments to be passed to or from other methods.
#' @return A \code{data.frame} with the following columns.
#' \tabular{ll}{
#'   \code{npass} \tab Number of items passed\cr
#'   \code{nfail} \tab Number of items failing\cr
#'   \code{nNA} \tab Number of items resulting in \code{NA}\cr
#'   \code{rel.pass} \tab Relative number of items passed\cr
#'   \code{rel.fail} \tab Relative number of items failing\cr
#'   \code{rel.NA} \tab Relative number of items resulting in \code{NA}
#' }
#' If \code{by='rule'} the relative numbers are computed with respect to the number 
#' of records for which the rule was evaluated. If \code{by='record'} the relative numbers
#' are computed with respect to the number of rules the record was tested agains. By default
#' the most failed validations and records with the most fails are on the top. 
#'
#' When \code{by='record'} and not all validation results have the same dimension structure,
#' a list of \code{data.frames} is returned.
#'
#' @seealso \code{\link{sort}}, \code{\link{summary}}
#'
#' @export 
setMethod('sort',signature('validation'),function(x, decreasing=FALSE, by=c('rule','record'), drop=TRUE,...){
  v <- values(x, drop=FALSE)
  by <- match.arg(by)
  aggr <- if ( by == 'rule') colSums else rowSums
  ntot <- if ( by == 'rule') nrow else ncol
  L <- lapply(v, function(y){
    s <- aggr(y,na.rm=TRUE)
    i <- order(s,decreasing=decreasing)
    s <- s[i]
    na <- aggr(is.na(y))[i]
    N <- ntot(y)
    nfail = N - s - na
    data.frame( 
       npass = s
       , nfail = nfail 
       , nNA = na
       , rel.pass  = s/N
       , rel.fail  = nfail/N
       , rel.NA = na/N
      )
    })
  if ( length(L) == 1 && drop ) L <- L[[1]]
  if ( by== 'rule' && !is.data.frame(L) ) L <- do.call(rbind,L)
  L
})





### currently obsolete stuff ----

# @rdname values
# @export
#setGeneric('severity',def=function(x,...) standardGeneric('severity'))

# @rdname values
# setMethod('severity', signature('validation'),function(x,...){
#   values <- x$._value[!has_error(x)]
#   lists <- sapply(values,is.list)
#   L <- lapply(values[lists],function(x) x$severity) 
#   simplify_list(L)
# })

# @rdname values
# @export
#setGeneric('impact',def=function(x,...) standardGeneric('impact'))

# @rdname values
# setMethod('impact', signature('validation'),function(x,...){
#   values <- x$value[!has_error(x)]
#   lists <- sapply(values,is.list)
#   L <- lapply(values[lists],function(x) x$impact) 
#   simplify_list(L)
# })
