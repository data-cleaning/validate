

# A rule is a call object endowed with extra attributes
setClass("rule",
  slots = c(
    call         = "expression"
    , name       = "character"
    , short_note = "character"
    , long_note  = "character"
    , origin     = "character"
    )
  , prototype = list(
    call         = expression(TRUE)
    , name       = ""
    , short_note = ""
    , long_not   = ""
    , origin     = ""
    )
  , sealed = TRUE
)

setMethod("show", "rule", function(object){
  cat(sprintf("%-5s: %s (%s)"
        , object@name 
        , deparse(object@call[[1]])
        , object@short_note)
      )
})

