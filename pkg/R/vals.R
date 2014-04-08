# implementation of the VALS syntax
# VALS version: 0.13.09


# vals object
setRefClass('vals', contains= 'verifier'
  , methods = list(
    initialize = function(...,files=NULL) .vals(...,files=files)
    )
)





