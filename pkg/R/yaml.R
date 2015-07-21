# parse yaml rule files


readlines_utf8 <- function(file, encoding="unknown"){
  lines <- readLines(con=file, encoding=encoding)
  enc2utf8(lines)
}

filter_yrf_options <- function(lines){
  index <- grep("^---[[:blank:]]*$",lines)

  if (length(index) < 2 || index[1] == index[2] ){ 
    NULL
  } else {
    if ( index[1]==1 ){
      lines[(index[1]+1):(index[2]-1)]
    } else {
      NULL
    }
  }
}

# detect whether a string starts with a drive letter, tilde (home), "\\" or
is_full_path <- function(string){
  grepl("(^[[:alpha:]^.]+:(/|\\\\))|(^\\\\)|(^//)|(^~/)|(^~\\\\).+",string) 
}



parse_yrf_options <- function(lines){
  option_lines <- filter_yrf_options(lines)
  L <- yaml::yaml.load(string = paste0(option_lines,collapse="\n"))
  L$options
}

parse_yrf_include <- function(file){
  lines <- readlines_utf8(file)
  option_lines <- filter_yrf_options(lines)
  L <- yaml::yaml.load(string = paste0(option_lines,collapse="\n"))
  paths <- L$include
  rel_path <- !is_full_path(paths)
  paths[rel_path] <- file.path(dirname(file),paths[rel_path])
  paths
}



yrf_block_type <- function(block){
  if ( is.null(block) ){
    NULL
  } else if ( any(c("options","include") %in% names(block)) ){
    "options"
  } else if ( identical(names(block),"rules") ){
    "yrf"
  } else {
    "free"
  }
}

valid_yaml <- function(string){
  root <- names(yaml::yaml.load(string))
  keys <- c("options","include","rules")
  valid <-  length(root) > 0 && all(root %in% keys)
  if ( !valid & length(root) > 0 ){
    warning(
     sprintf("Found invalid keys: %s\n", paste0(root[!root %in% keys],collapse=", "))
    )
  }
  valid
}

is_yaml <- function(string){
  out <- tryCatch(yaml::yaml.load(string),error = function(e) FALSE)
  !identical(out,FALSE)
}

is_r <- function(string){
  out <- tryCatch(parse(text=string),error = function(e) FALSE)
  !identical(out,FALSE)
}

# find yaml documents and parse them
yaml_blocks <- function(lines){
  S <- strsplit(x = paste0(lines,collapse="\n"), split="---[[:blank:]]*\\n?")[[1]]
  S <- Filter(function(x) nchar(x)>0,S)
  lapply(S, function(s){ 
    if ( is_yaml(s)  && valid_yaml(s) ){ 
      yaml::yaml.load(s)
    } else if ( is_r(s) ){
      s
    } else {
      cat(sprintf("\nThe following invalid block is skipped:\n %s\n",s))
      warning("Blocks containing invalid yaml or R syntax detected")
      NULL
    }
  })
}


# lines <- readlines_utf8("tmp/test.yml")
# blocks <- yaml_blocks(lines)  
  