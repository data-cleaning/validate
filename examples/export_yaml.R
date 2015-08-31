
v <- validator(x > 0, y > 0, x + y == z)
txt <- as_yaml(v)
cat(txt)


# NOTE: you can safely run the code below. It is enclosed in 'not run'
# statements to prevent the code from being run at test-time on CRAN
\dontrun{
export_yaml(v, file="my_rules.txt")
}

