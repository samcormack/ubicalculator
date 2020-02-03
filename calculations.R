## Calculation functions for ubicalculator app

calculateTax = function(income,tax.bracket) {
  ## income: vector containing before-tax incomes
  ## tax.bracket: dataframe with fields:
  ##    threshold: lower bound for tax bracket
  ##    rate: marginal tax rate applying to bracket
  tax = numeric(length(income))
  for (ii in 1:nrow(tax.bracket)) {
    if (ii == nrow(tax.bracket)) {
      upper.bound = Inf
    }
    else {
      upper.bound = tax.bracket$threshold[ii+1]
    }
    income.in.band = pmin(upper.bound,income)
    income.in.band = pmax(0,income.in.band - tax.bracket$threshold[ii])
    tax = tax + tax.bracket$rate[ii] * income.in.band
  }
  return(tax)
}