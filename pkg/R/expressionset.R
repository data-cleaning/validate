#' @include parse.R
#' @include sugar.R
#' @include rule.R
NULL


#### EXPRESSIONSET OBJECT -----------------------------------------------------


#' Superclass for storing a set of rich expressions.
#'
#' @section Details:
#' This class is aimed at developers of this package or packages depending on
#' it, not at users. It is the parent object of both the \code{\link{validator}}
#' and the \code{\link{indicator}} class.
#' 
#' 
#' An \code{expressionset} is a reference class storing a list of
#' \code{\link{rule}}s. It contains a number of methods that are not exported
#' and may change or dissapear without notice. We strongly encourage developers
#' to use the exported S4 generics to set or extract variables
#' 
#' @section Exported S4 methods for \code{expressionset}:
#' \itemize{
#'  \item{\code{\link{variables}}}
#'  \item{\code{\link{names}}}
#'  \item{\code{\link{length,expressionset-method}}}
#'  \item{\code{\link{created}}}
#'  \item{\code{\link{origin}}}
#'  \item{\code{\link{labels}}}
#'  \item{\code{\link{description}}}
#'  \item{\code{\link{[,expressionset-method}}}
#'  \item{\code{\link{[[,expressionset-method}}}
#'  \item{\code{\link{summary,expressionset-method}}}
#' }
#' 
#' 
#' @section Private S4 methods for \code{expressionset}:
#' \itemize{
#'  \item{validating}
#'  \item{linear}
#'  \item{is_tran_assign}
#' }
#' 
#' 
#' @section See also:
#' \itemize{
#'  \item{\code{\link{rule}}}
#'  \item{\code{\link{validator}}}
#'  \item{\code{\link{indicator}}}
#' } 
#'
#' 
#' @keywords internal
expressionset <- setRefClass("expressionset"
  , fields = list(
        rules = "list"
      , ._options = "function"
  )
  , methods= list(
        show          = function() .show_expressionset(.self)
      , exprs         = function(...) .get_exprs(.self,...)
      , blocks        = function() .blocks_expressionset(.self)
      , options       = function(...) .self$._options(...)
      , clone_options = function(...) settings::clone_and_merge(.self$._options,...)
  )
)


#' Service for filling an expressionset from commandline
#'
#' @section Details:
#' This function is aimed at developers importing the package and 
#' not at direct users of \pkg{validate}.
#'
#' @param obj An expressionset object (or an object inheriting from expressionset).
#' @param ... Comma-separated list of expressions
#' @param .prefix Prefix to use in default names.
#'
#' @export
#' @rdname validate_extend
#' @keywords internal
.ini_expressionset_cli <- function(obj, ..., .prefix="R"){
  L <- as.list(substitute(list(...))[-1])
  nm <- extract_names(L, prefix = .prefix)
  cr <- Sys.time()
  R <- vector(length(L), mode="list")
  # note: we cannot set the description or the label when constructing
  # from the commandline.
  for ( i in seq_along(L) ){
    R[[i]] <- rule(
        expr = L[[i]]
      , name = nm[i]
      , origin="command-line"
      , created = cr
      )
  }
  obj$rules <- R
}

#' @param obj An expressionset object (or an object inheriting from expressionset).
#' @param dat a data.frame 
#' 
#' @rdname validate_extend
#' @keywords internal
.ini_expressionset_df <- function(obj, dat, .prefix="R"){
  n <- nrow(dat)
  R <- vector(n, mode="list")
  cr = Sys.time()
  if ( is.null(dat[["name"]]) ){
    npos <- npos(nrow(dat))
    fmt <- paste0("%s%",npos,"d")
    dat$name <- sprintf(fmt, .prefix, seq_len(nrow(dat)))
  }
  if (is.null(dat[["description"]])){
    dat$description <- ""
  }
  if (is.null(dat[["label"]])){
    dat$label <- ""
  }
  if (is.null(dat[["origin"]])){
    dat$origin <- ""
  }
  if (is.null(dat[["rule"]])){
    stop("No column called 'rule' found")
  }
  dat$name <- as.character(dat$name)
  dat$label <- as.character(dat$label)
  dat$rule <- as.character(dat$rule)
  dat$description <- as.character(dat$description)
  for ( i in seq_len(n)){
    R[[i]] <- rule(
      expr = parse(text=dat$rule[i])[[1]]
      , name = dat$name[i]
      , origin = dat$origin[i]
      , label = dat$label[i]
      , description = dat$description[i]
      , created = cr
    )
  }
  obj$rules <- R
}


#' @param obj An expressionset object (or an object inheriting from expressionset).
#' @param file a filename
#' @param .prefix Prefix to use in default names.
#'
#' @export
#' @rdname validate_extend
#' @keywords internal
.ini_expressionset_yml <- function(obj, file, .prefix="R"){
  S <- get_filestack_yml(file)
  R <- list()
  for ( fl in S )
    R <- c(R, rules_from_yrf_file(fl,prefix=.prefix))
  obj$rules <- R
  obj$._options <- .PKGOPT
  # options only from the 'including' file (not from included)
  local_opt <- options_from_yml(file)
  if ( length(local_opt) > 0 )
    do.call(obj$options, local_opt)
}


rules_from_block <- function(block, origin){

  # helper functions.
  rules_from_freeform <- function(string, origin){
    S <- tryCatch(parse(text=string), error = function(e){
      stop(sprintf("parsing freeform block. Parser returned:\n  %s", e$msg))
    })
    lapply(S,function(s) rule(expr=s, origin=origin, created=now))
  }
  
  rules_from_yrf <- function(block, origin){  
    lapply(block$rules, function(x){
      rule(
        expr = parse(text=x$expr)[[1]]
        , name = as.character(x$name)
        , label = as.character(x$label)
        , description = as.character(x$description)
        , origin = origin
        , created = now
      )  
    })
  }
  
  now <- Sys.time()
    
  type <- yrf_block_type(block)
  if ( identical(type,"free") ){
    rules_from_freeform(block, origin=origin)
  } else if (identical(type, "yrf")){
    rules_from_yrf(block, origin=origin)
  }
  
}


rules_from_yrf_file <- function(file,prefix="V"){

  lines <- .readlines_utf8(file)
  blocks <- yaml_blocks(lines)
  rules <- unlist(lapply(blocks, rules_from_block, origin=file))
  
  
  # set generic name if needed.
  npos <- max(1,ceiling(log10(length(rules)+1)))
  fmt <- paste0("%s%0",npos,"d")
  generic <- sprintf(fmt,prefix,seq_along(rules))
  for ( i in seq_along(rules) ){
    if ( identical(rules[[i]]@name,character(0)) ) {
      rules[[i]]@name <- generic[i]
    }
  }
  
  rules
}

options_from_yml <- function(file){
  lines <- .readlines_utf8(file)
  .parse_yrf_options(lines)
}

# Get sequence of files to be processed from include statements.
# the filestack is returned reversely depth-first, e.g.
#
# ROOT
#  - CHILD1
#  - CHILD2
#    - CHILD3
# is returnd in the order CHILD1 CHILD3 CHILD2 CHILD1 ROOT
#
get_filestack_yml <- function(file){

  f <- function(fl, det=character(0)){
    det <- c(fl,det)
    if ( fl %in% det[-1])
      stop(sprintf("Cyclic dependency detected in %s\n%s\n",fl,paste(rev(det),collapse="\n -> ")))
    L <- parse_yrf_include(fl)
    for ( x in L )
      f(x,det)
    filestack <<- c(filestack,fl)
  }
  filestack <- character(0)
  f(file)
  filestack
}





#' @param obj an expressionset object
#' @rdname validate_extend
#' @export
#' @keywords internal
.show_expressionset <- function(obj){
  nr <- length(obj)
  cat(sprintf(
    "Object of class '%s' with %s elements:\n",class(obj)[1], nr
  ))
  if (nr == 0) return(invisible(NULL))
  nam <- names(obj)
  lab <- label(obj)
  lab <- paste0(nam,ifelse(nchar(lab)>0,paste0(" [",lab,"]"),lab))
  n <- max(nchar(lab))
  lab <- paste0(" ",format(lab,width=n),": ",sapply(obj$exprs(expand_groups=FALSE
                                                , lin_eq_eps=0, lin_ineq_eps=0), call2text))
  cat(noquote(paste(lab,collapse="\n")))
  cat("\n")
  optstr <- "Rules are evaluated using locally defined options\n"
  cat(optstr[!identical(obj$._options,.PKGOPT)])
}






# from call to oneliner text
call2text <- function(x){
  gsub("[[:blank:]]+"," ",paste(deparse(x),collapse=" "))
}

npos <- function(n) max(1,ceiling(log10(n+1)))

# get names from a list, replacing empty names values with numbers
extract_names <- function(L,prefix="V"){
  npos <- npos(length(L)) 
  fmt <- paste0("%s%0",npos,"d")
  generic <- sprintf(fmt,prefix,seq_along(L))
  given <- names(L)
  if (is.null(given)) return(generic)
  igen <- given %in% c("", NA)
  given[igen] <- generic[igen]
  make.names(given, unique=T)
}

#' @rdname validate_extend
#' @param expand_assignments Substitute assignments?
#' @param expand_groups Expand groups?
#' @param varlist: a character vector of variables to search through 
#'    when groups are defined with regexps.
#' @param vectorize Vectorize if-statements?
#' @param replace_dollar Replace dollar with bracket index?
#' @param dat Optionally, a \code{data.frame} containing the data to which the
#'    expressions will be applied. When provided, the only equalities \code{A==B}
#'    that will be translated to \code{abs(A-B)<lin.eq.eps} are those where all 
#'    occurring variables are numeric in \code{dat}.
#' @export
#' @keywords internal
.get_exprs <- function(x, ...
    , expand_assignments=FALSE
    , expand_groups=TRUE
    , vectorize=TRUE
    , replace_dollar=TRUE
    , lin_eq_eps = x$options('lin.eq.eps')
    , lin_ineq_eps = x$options('lin.ineq.eps')
    , dat=NULL
){
  exprs <- setNames(lapply(x$rules, expr ),names(x))
  if ( expand_assignments )  exprs <- expand_assignments(exprs)
  if ( expand_groups ) exprs <- expand_groups(exprs)
  if ( vectorize ) exprs <- lapply(exprs, vectorize)
  if ( replace_dollar ) exprs <- lapply(exprs, replace_dollar)
  if (lin_eq_eps > 0) exprs <- lapply(exprs, replace_linear_restriction, eps=lin_eq_eps, dat=dat, op="==")
  if (lin_ineq_eps > 0) exprs <- lapply(exprs, replace_linear_restriction, eps=lin_eq_eps, dat=dat, op="<=")
  if (lin_ineq_eps > 0) exprs <- lapply(exprs, replace_linear_restriction, eps=lin_eq_eps, dat=dat, op=">=")
  exprs
}


#' @rdname validate_extend
#' @param x An expressionset object
#' @export
#' @keywords internal
.blocks_expressionset <- function(x){
  varblock <- function(v,vlist){
    sapply(vlist, function(x) any(v %in% x) | identical(v,character(0)) )
  }
  # variable x rule matrix
  V <- variables(x,as="matrix")
  # all connections 
  A <- V %*% t(V) > 0
  L <- lapply(1:nrow(A),function(i) which(A[i,]))
  
  blocks <- new.env()
  b <- 0
  while( length(L) > 0){
    b <- b+1
    i <- varblock(L[[1]],L)
    blocks[[paste0('block',b)]] <- unique(unlist(L[i]))
    L <- L[!i]
  }
  as.list(blocks)
}


# S4 GENERICS -----------------------------------------------------------------




#' Create a summary
#' @rdname validate-summary
#' @example ../examples/summary.R
setGeneric('summary')


#' Export to yaml file
#'
#' Translate an object to yaml format and write to file.
#'
#' Both \code{\link{validator}} and \code{\link{indicator}} objects can be
#' exported.
#'
#' @param x An R object
#' @param file A file location or connection (passed to \code{base::\link[base]{write}}).
#' @param ... Options passed to \code{yaml::\link[yaml]{as.yaml}}
#' 
#' 
#' @example ../examples/export_yaml.R
#' 
#' @export
setGeneric("export_yaml",function(x,file,...) standardGeneric("export_yaml"))

#' @rdname export_yaml
#' @export
setGeneric("as_yaml", function(x,...) standardGeneric("as_yaml"))

# S4 IMPLEMENTATIONS ----------------------------------------------------------

#' @describeIn  variables Variables occuring in \code{x} either as a single list, or per rule.
#' @param as how to return variables: 
#' \itemize{
#'   \item{\code{'vector'}} Return the uniqe vector of variables occurring in \code{x}.
#'   \item{\code{'matrix'}} Return a boolean matrix, each row representing a rule, each column representing a variable.
#'   \item{\code{'list'}} Return a named \code{list}, each entry containing a character vector with variable names.
#' }
#' @param dummy Also retrieve transient variables set with the \code{:=} operator.
#'
#'
#' @example ../examples/variables.R
setMethod("variables", "expressionset",  function(x, as=c('vector','matrix','list'), dummy=FALSE, ...){ 
  as <- match.arg(as)
  vars <- lapply(x$exprs(expand_assignments=!dummy),var_from_call)
  u <- unique(unlist(vars))
  
  switch(as
   , 'vector' = u
   , 'list'   = vars
   , 'matrix' = {  
     a <- array(FALSE,dim=c(length(vars),length(u)),dimnames=list(rule=names(vars),variable=u) )
     for (i in seq_along(vars)) a[i,vars[[i]]] <- TRUE
     a
   })
})



#' @rdname voptions
setMethod('voptions','expressionset',function(x=NULL,...){
  if (settings::is_setting(...)){
    x$._options <- clone_and_merge(x$._options,...)
  } else {
    x$._options(...)
  }
})

#' @rdname voptions
setMethod('reset','expressionset',function(x=NULL){
  settings::reset(x$._options)
})

#' @describeIn origin Origin of every rule in \code{x}
setMethod("origin", "expressionset", function(x,...) sapply(x$rules,origin))


#' @describeIn  label label description of every rule in \code{x}
setMethod("label","expressionset",function(x,...) unlist(sapply(x$rules, label)))

#' @describeIn description description description of every rule in \code{x}
setMethod("description", "expressionset", function(x,...) unlist(sapply(x$rules, description)))


#' @describeIn created Creation time of every rule in \code{x}
setMethod("created", "expressionset", function(x,...){ 
  # obj. of class POSIXct; sapply strips the POSIXct class attribute
  cr <- rep(Sys.time(),length(x))
  for ( i in seq_along(x)){
    cr[i] <- created(x$rules[[i]])
  }
  cr
})



#' Extract names
#' 
#' @param x An R object
#'
#' @return A \code{character} with names of rules occurring in \code{x}
#' @export
#' @example ../examples/properties.R
setMethod("names","expressionset",function(x){
  sapply(x$rules, function(rule) rule@name)
})

# recycle x over y
recycle <- function(x,y){
  m <- length(x)
  n <- length(y)
  remainder <- n %% m
  times <- n %/% m
  if (remainder > 0){
    warning(gettext("longer object length is not a multiple of shorter object length"))
    times <- times + 1
  }
  rep(x,times=times)[seq_len(n)]
}


#' Set names
#'
#' Names are recycled and made unique with \code{\link{make.names}}
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setReplaceMethod("names",c("expressionset","character"),function(x,value){
  value <- make.names(recycle(value,x),unique=TRUE)
  for ( i in seq_len(length(x))){
    names(x$rules[[i]]) <- value[i]
  }
  x
})

#' Set origins
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setReplaceMethod("origin",c("expressionset","character"), function(x,value){
  value <- recycle(value, x)
  for ( i in seq_len(length(x))){
    origin(x$rules[[i]]) <- value[i]
  }
  x
})

#' Set labels
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setReplaceMethod("label",c("expressionset","character"),function(x,value){
  value <- recycle(value,x)
  for ( i in seq_len(length(x))){
    label(x$rules[[i]]) <- value[i]
  }
  x
})

#' Set descriptions
#' 
#' 
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setReplaceMethod("description",c("expressionset","character"),function(x,value){
  value <- recycle(value,x)
  for ( i in seq_len(length(x))){
    description(x$rules[[i]]) <- value[i]
  }
  x
})

#' Set timestamps
#'
#' @param x Object
#' @param value Value to set
#' @example ../examples/properties.R
#' @export 
setReplaceMethod("created",c("expressionset","POSIXct"),function(x,value){
  value <- recycle(value, x)
  for ( i in seq_len(length(x))){
    created(x$rules[[i]]) <- value[i]
  }
  x
})

setMethod("validating", "expressionset", function(x,...){
  if (length(x) == 0) return(logical(0))
  sapply(x$rules, validating)
})



setMethod("linear","expressionset", function(x,...){
  if(length(x)==0) return(logical(0))
  sapply(x$rules, linear)
})

#' @section Validator and indicator objects:
#' For these objects, the ruleset is split into subsets (blocks) that are disjunct in the
#' sense that they do not share any variables. For each bloch the number of variables, the number 
#' of rules and the number of rules that are linear are reported.
#' 
#' @return A \code{data.frame} with the information mentioned below is returned.
#' 
#' @rdname validate-summary
setMethod('summary',signature('expressionset'),function(object,...){
  b <- object$blocks()
  data.frame(
    block = seq_along(b)
    , nvar  = sapply(b,function(i) length(variables(object[i])))
    , rules = sapply(b,length)
    , linear = sapply(b,function(i) sum(object[i]$is_linear()))
    , row.names=NULL
  )
})

#' Determine the number of elements in an object.
#' 
#' @param x An R object
#' @rdname length 
#' @aliases length,expressionset-method 
#' @export
setMethod("length","expressionset",function(x) length(x$rules))


#' Select a subset
#' 
#' @section Details:
#' The \code{options} attribute will be cloned
#' 
#' @param x An R object
#' @param i an index (numeric, boolean, character)
#' @param j not implemented
#' @param drop not implemented
#' @param ... Arguments to be passed to other methods
#' 
#' @return An new object, of the same class as \code{x} subsetted according to \code{i}.
#' @rdname select
#' @aliases [,expressionset-method
#' 
#' @export
setMethod("[",signature("expressionset"), function(x,i,j,...,drop=TRUE){
  if (is.character(i)){
    i <- match(i,names(x))
  }
  out <- new(class(x))
  out$rules <- x$rules[i]
  out$._options = clone_and_merge(x$._options)
  out
})

#' @param exact Not implemented
#' @rdname select
#' @aliases [[,expressionset-method
setMethod("[[",signature("expressionset"), function(x,i,j,...,exact=TRUE){
  if ( is.character(i) ){
    i <- which(i %in% names(x))
  }
    x$rules[[i]]
})


setMethod("is_tran_assign","expressionset",function(x,...){
  if (length(x)==0) return(logical(0))
  sapply(x$rules,is_tran_assign)
})


#' @rdname export_yaml
setMethod("export_yaml","expressionset", function(x, file,...){
  write(x = as_yaml(x,...), file=file)
})

#' @rdname export_yaml
setMethod("as_yaml","expressionset",function(x,...){
  option_string <- ""
  if (!identical(x$._options,.PKGOPT)){ # export options when set.
    option_string <- paste0("---\n",yaml::as.yaml(list(options=x$options()),...),"---\n")
  }
  rule_string <- yaml::as.yaml(rapply(as.list.expressionset(x), f=function(y) paste0("",y),how="replace"),...)
  paste0(option_string,rule_string)
})




as.list.expressionset <- function(x, expr_as_text=TRUE, ...){
  list(
    rules = lapply(x$rules, as.list.rule, expr_as_text = expr_as_text, ...)
  )
}



#' Coerce to \code{data.frame}
#'
#' @param x Object to coerce
#' @param ... arguments passed to other methods
#' @param optional ignored
#' @param row.names ignored
#'
#' @export
setGeneric("as.data.frame")


#' Translate an expressionset to data.frame
#' 
#' Expressions are deparsed and combined in a \code{data.frame} with (some
#' of) their metadata. Observe that some information may be lost (e.g. options
#' local to the object).
#'
#'
#' @inheritParams as.data.frame
#' @param expand_assignments Toggle substitution of `:=` assignments.
#' 
#' 
#' @return A \code{data.frame} with elements \code{rule}, \code{name},
#'  \code{label}, \code{origin}, \code{description}, and \code{created}.
#' @export
setMethod("as.data.frame","expressionset", function(x, expand_assignments=TRUE, ...){
  rules <- sapply(x$exprs(expand_assignments=expand_assignments,...),call2text)
  x <- x[names(rules)]
  data.frame(
   rule = rules
   , name = names(x)
   , label = label(x)
   , origin = origin(x)
   , description = description(x)
   , created = created(x)
   , row.names=NULL
   , stringsAsFactors=FALSE
  )
})



