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


parse_yrf_options <- function(lines){
  option_lines <- filter_yrf_options(lines)
  L <- yaml::yaml.load(string = paste0(option_lines,collapse="\n"))
  L$options
}

parse_yrf_incude <- function(lines){
  option_lines <- filter_yrf_options(lines)
  L <- yaml::yaml.load(string = paste0(option_lines,collapse="\n"))
  L$include  
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


# find yaml documents and parse them
yaml_blocks <- function(lines){
  S <- strsplit(x = paste0(lines,collapse="\n"), split="(^|\\n*)---[[:blank:]]*\\n")[[1]]
  lapply(S, function(s){ 
    yml_block <- yaml::yaml.load(s)
    # free-form rules are not parsed by yaml.
    if ( identical(yrf_block_type(yml_block), "free") ){
      s
    } else {
      yml_block
    }
  })
}


# lines <- readlines_utf8("tmp/test.yml")
# blocks <- yaml_blocks(lines)  
  