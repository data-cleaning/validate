
v <- validator(
  height>0
  ,weight>0
  ,heigt < 1.5*mean(height)
)
cf <- confront(v,women)
summary(cf)

