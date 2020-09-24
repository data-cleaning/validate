
years   <- 2014:2018
qrts    <- sprintf("Q%d",1:4)
vars    <- c("import","export","balance") # balance is exp-imp


region <- c(
          "Samplonia"  # country
        , "Agria"      # province
        , "Newbay"     # new district
        , "Greenham"   # veg, fruit district   
        , "Wheaton"    # cattle district 
        , "Induston"   # province
        , "Oakdale"    # retiree district 
        , "Crowdon"    # commuter district 
        , "Mudwater"   # industry district  
        , "Smokely"    # industry district 
        )




make_year <- function(total, year, outvar){
  regional <- c(
      "Samplonia"  = 1.0
    , "Agria"      = 0.30
    , "Newbay"     = 0.08
    , "Greenham"   = 0.12
    , "Wheaton"    = 0.10
    , "Induston"   = 0.70
    , "Oakdale"    = 0.01
    , "Crowdon"    = 0.04
    , "Mudwater"   = 0.20
    , "Smokely"    = 0.45
  )

  seasonal <- list(
     Agria    = c(Q1 = 0.1, Q2 = 0.2, Q3 = 0.5, Q4 = 0.2)
   , Induston = c(Q1 = 0.2, Q2 = 0.3, Q3 = 0.3, Q4 = 0.2)
  )



  Agria    <- c("Newbay","Greenham","Wheaton")
  Induston <- c("Oakdale","Crowdon","Mudwater","Smokely")

  year_totals <- total * regional
  agria_qrtrs <- sapply(seasonal$Agria, function(x) x* year_totals[Agria])
  induston_qrtrs <- sapply(seasonal$Induston, function(x) x* year_totals[Induston])

  agria_qrtrs    <- addmargins(agria_qrtrs,1)
  induston_qrtrs <- addmargins(induston_qrtrs,1)

  rownames(agria_qrtrs)[4] <- "Agria"
  rownames(induston_qrtrs)[5] <- "Induston"

  provinces <- rbind(agria_qrtrs, induston_qrtrs)
  country   <- rbind(provinces, colSums(provinces)/2)
  rownames(country)[nrow(country)] <- "Samplonia"
  country <- addmargins(country,2)
  colnames(country)[ncol(country)] <- year

  colkeys      <- colnames(country)
  colkeys[1:4] <- paste0(year, colkeys[1:4])
  rowkeys      <- rownames(country)

  df <- expand.grid(region = rowkeys, period = colkeys)
  df[outvar] <- as.numeric(country)
  df
}



gdp    <- 200 * 10000 # 10000 inhabitants
import <- 0.35 * gdp
export <- 0.37 * gdp
L <- list()
set.seed(1)
i <- 1
for (year in 2014:2019){
  df  <- make_year(gdp, year, "gdp")
  df  <- merge(df, make_year(import, year, "import"))
  df  <- merge(df, make_year(export, year, "export"))
  L[[i]] <- df
  gdp    <- round((1 + sample(-3:3,1)/100) * gdp/10000)*10000
  import <- round((1 + sample(-5:5,1)/100) * import/10000)*10000
  export <- round((1 + sample(-5:5,1)/100) * export/10000)*10000
  i <- i+1
}

out <- do.call(rbind, L)

out$balance <- out$export - out$import

out <- as.data.frame(tidyr::pivot_longer(out,gdp:balance))
names(out)[3:4] <- c("measure","value")

out$freq <- ifelse(grepl("Q",out$period),"Q", "A")


write.csv(out[c(1,5,2,3,4)], "economy_samplonia_clean.csv", row.names=FALSE)

