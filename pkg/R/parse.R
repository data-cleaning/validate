##-------------------------------------------------------------------------
# default symbols allowed to define rules or restrictions
.onLoad <- function(libname,pkgname){
  options(validationSymbols = c(
    '<','<=','==','>','>=', '!=', '%in%', ":"
    , 'identical', 'all','any' 
    , '!', '|', '||', '&', '&&', 'xor'
  ))
}

.onUnload <- function(libpath){
  options(validationSymbols=NULL) 
}



read_resfile <- function(file){
  L <- tryCatch(parse(file=file)
         , error = function(e){
           cat('Parsing failure at', file,"\n")
           e
       })
  names(L) <- extract_names(L)
  lapply(L,as.call)
}


# Extract user-defined names from an object returned by 'parse'
#  extract_names <- function(L,labeltext='label'){
#   labeltext <- paste0('@',labeltext)
#   
#   i <- getSrcLocation(L)
#   d <- getParseData(L)
#   
#   names <- paste0(rep("000",length(L)),seq_along(L))
#   names <- paste0("R",substring(names,nchar(names)-2))
#   if ( !is.null(names(L)) ){
#     iname <- names(L) != ""
#     names[iname] <- names(L)[iname]
#   }
#   # select comments
#   d <- d[d$token == "COMMENT",c('line1','text')]
#   # select name definitions
#   d <- d[grep(labeltext,d$text), ]
#   srcloc <- d$line1 + 1
#   
#   srclab <- gsub(paste0("^.+",labeltext," "),"",d$text)
#   srclab <- gsub("\\s+.+$","",srclab)
#   
#   # merge names and definitions
#   j <- match(i,srcloc,nomatch=0)
#   names[which(i %in% srcloc)] <- srclab[j]
#   names  
# }


is.validating <- function(x, allowed=getOption('validationSymbols'),...){
  sym <- deparse(x[[1]])
  sym %in% allowed || grepl("^is\\.",sym) || ( sym == 'if' && is.validating(x[[2]]) && is.validating(x[[3]]) ) 
}


###############################################################################
# Translate colon notation 'x : <classname>' to class(x) == "<classname>"
extract_datamodel <- function(x){
  if ( x[[1]] == ":" ){
    parse(text=paste0("inherits(",deparse(x[[2]]),", '",deparse(x[[3]]),"')"))[[1]]
  } else {
    x
  }
}


###############################################################################
# vectorization functions
not <- function(x) parse(text=paste0("!(",deparse(x),")"))[[1]]

`%or%` <- function(x,y){
  parse(text=paste(deparse(x),'|',deparse(y)))[[1]]
} 

vectorize <- function(x){
  if ( x[[1]] == 'if' ){
    not(x[[2]]) %or% x[[3]]
  } else {
    x
  }
}




