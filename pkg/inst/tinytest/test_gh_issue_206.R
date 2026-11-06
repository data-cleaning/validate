# Thanks to Jan van der Laan for reporting 
v <- validator(weight > 0)
expect_error(confront(women, v, raise="error"), pattern="^Invalid")

