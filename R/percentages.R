getPercentages <- function() {
  
  regionsNew <- NULL
  load("./Data/probabilities_districtsNew.RData")
 
  stateSize <- tapply(regionsNew$Population.Census.2011, regionsNew$State.x, sum)
  
  regionsNew$StateTotalEst <- stateSize[match(regionsNew$State.x, names(stateSize))]
  
  regionsNew$percentage <- regionsNew$Population.Census.2011 / regionsNew$StateTotalEst
  
  return(regionsNew)
}