validator(male==TRUE)

# create a set of validation rules
v <- validator(age > 21, gender %in% c("male","female"))
# what variables does it contain?
variables(v)
# which rules are linear?
is_linear(v)
