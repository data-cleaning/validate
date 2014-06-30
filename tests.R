

fn <- c('expressionset.R','indicator.R','validator.R',
        'confrontation.R','compare.R','parse.R','factory.R'
        ,'sugar.R','functions.R')
dmp <-lapply(file.path('pkg/R',fn),source)


v <- validator(
  NUM : {Sepal.Width; Sepal.Length; Petal.Length; Petal.Width}
  , isnum = is.numeric(NUM)
  , ispos = NUM > 0
  , ratio = if(Species == 'setosa') Sepal.Length/Sepal.Width < 1.7
  , I := Species == 'setosa'
  , means = mean(Sepal.Width[I]) > mean(Sepal.Width[!I])
)

v

cf <- confront(v,iris)

#

un <- read.table("~/projects/rcourse/src/data/UnitedNations.txt")
v <- validator(
  lifeMale > 50
  , lifeFemale > 50
  )

cf <- confront(v,un)
barplot(cf)








