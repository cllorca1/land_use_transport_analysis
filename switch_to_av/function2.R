calculateSwitchToAutonomousVehicleProbabilities = function(income, year, intercept, betaRatio, betaIncome, initialYear, equalPriceYear) {
  
  if (year < initialYear){
    ratio = 999999 #/high ratio to avoid the probability of switching to autonomous before 2020
  } else {
    ratio = 10 + (year - initialYear)/(equalPriceYear - initialYear)*(1-10) 
  }
  
  ratio = max(c(ratio, 1))
  
  utility = intercept + (betaRatio * ratio) + (betaIncome * income)
  return(1 - 1 / (1+exp(utility)))
}