createPop <- function( seed = NULL,
                       expension = 1,
                       dispersion = 0,
                       data)
{
  if (is.null(seed)) {
    seed <- rbinom(n = 1, size = 100, prob = runif(1))
  }
  set.seed(seed)
  #library(laeken)
  # a_0 <- readRDS(file = "./Data/AUT_adm0.rds")
  # a_1 <- readRDS(file = "./Data/AUT_adm1.rds")
  
  load(file = "./Data/shape_austria_dis.rds")
  a_2 <- shape_austria_dis
  a2_ids <- (cbind(a_2$ST, a_2$BL, a_2$disChar))
  eusilcP <- NULL
  
  
  my_eusilc <- data[data$main, ]
  my_eusilc$eqIncome <- as.numeric(my_eusilc$eqIncome)
  map_tab <- data.frame(deutsch = c("Burgenland",
                                    "Kärnten",
                                    "Niederösterreich",
                                    "Oberösterreich",
                                    "Salzburg",        
                                    "Steiermark",
                                    "Tirol",
                                    "Vorarlberg",
                                    "Wien"),
                        englisch = c("Burgenland",
                                     "Carinthia",
                                     "Lower Austria",
                                     "Upper Austria",
                                     "Salzburg",
                                     "Styria",
                                     "Tyrol",
                                     "Vorarlberg",
                                     "Vienna")
  )
  my_eusilc <- merge(x = my_eusilc, y = map_tab, by.x = "region", 
                     by.y = "englisch", all.x = T)
  
  if (expension > 1) {
    expnd  <- sample(x = seq_len(nrow(my_eusilc)), replace = TRUE, size = trunc(nrow(my_eusilc) * expension))
    my_eusilc <- my_eusilc[expnd, ]
  }
  
  full_smps <- table(my_eusilc$region)
  
  source("./R/percentages.R")
  regions <- getPercentages()
  
  regions <-  regions[order(regions$State.y, 
                            regions$netPerCapita, 
                            decreasing = T), ] 
  rangOrder <- regions$District
  regions$state <- as.character(regions$State.x)
  # runif(n = nrow(regions)))
  
  dom_sizes <-  function(size, name) {
    dom_s <- quantile(seq_len(size), probs = 
                        cumsum(regions$percentage[regions$state == name]), 
                      type = 1)
    dom_s <- c(dom_s[1], diff(dom_s))
    names(dom_s) <- regions$District[regions$state == name]
    dom_s
  }
  
  sub_doms <- mapply(FUN = dom_sizes, full_smps, names(full_smps))
  sub_doms <- lapply(FUN = function(X, state) { data.frame( district = names(X), count = X)},
                     sub_doms)
  for (i in seq_along(sub_doms)) {
    sub_doms[[i]][["state"]] <- names(sub_doms)[i]
  }
  sub_doms <- do.call(rbind, sub_doms)
  rownames(sub_doms) <- NULL
  
  my_eusilc$region <- as.character(my_eusilc$region)
  
  incomeOrder <- my_eusilc$eqIncome
  
  if (is.numeric(dispersion) && dispersion != 0)
  {
    incomeOrder <- incomeOrder + rnorm(n = length(incomeOrder),
                                       mean = 0,
                                       sd = sd(incomeOrder) * dispersion)
  }
  
  my_eusilc <- my_eusilc[order(my_eusilc$region, incomeOrder, decreasing = T),]
  sub_doms <- sub_doms[order(sub_doms$state, decreasing = T),]
  sum(sub_doms$count)
  nrow(my_eusilc)
  
  my_eusilc$sub_2 <- rep(sub_doms$district, sub_doms$count)
  
  return( list(seed = seed, 
               expension = expension, 
               dispersion = dispersion, 
               eusilcHHpop = my_eusilc,
               targetOrder = rangOrder)
  )
}


