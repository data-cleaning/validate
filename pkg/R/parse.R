

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





