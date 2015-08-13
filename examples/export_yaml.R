
v <- validator(x > 0, y > 0, x + y == z)
txt <- as_yaml(v)
cat(txt)

\dontrun{
export_yaml(v, file="my_rules.txt")
}

