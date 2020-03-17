ROUNDING_ERROR = 0.0000001

fill_unused_strats = function(eq, used_strats, num_strats) {
  ret = numeric(num_strats)
  ret[used_strats] = eq
  return(ret)
}

eq_with_given_strats = function(payouts, used_strats) {
  num_strats = length(used_strats)
  
  # If there is only one strategy, the optimal solution must be to choose it 100% of the time.
  if (num_strats == 1) {
    return(fill_unused_strats(1, used_strats, nrow(payouts)))
  }
  
  # If there are no strategies, there is nothing to solve, so say the first strategy is chosen
  # 100% of the time
  if (num_strats == 0) {
    return(fill_unused_strats(1, 1, nrow(payouts)))
  }
  
  coeffs = payouts[used_strats, used_strats]
  
  # If there is a nash equilibrium for the exact given set of strategies, the payouts for each
  # strategy will be the same. Therefore, the payoff for the first strategy subtracted from
  # the payoffs for each other strategy will be 0. As well, the proportions of the time each
  # strategy is chosen must sum to 1.
  for (i in 2:num_strats) {
    coeffs[i, ]  = coeffs[i, ] - coeffs[1, ]
  }
  coeffs[1, ] = 1
  totals = numeric(num_strats)
  totals[1] = 1
  
  # If there is no unique solution to the system of equations, fall back to the strategy of using
  # the first strategy 100% of the time.
  mixed_strategy = numeric(num_strats)
  mixed_strategy[1] = 1
  
  # Try to find a unique solution to the system of equations. Any unique solution will be a nash
  # equilibrium for the chosen set of available strategies.
  tryCatch(
    {
      mixed_strategy = solve(coeffs, totals)
    }, error=function(e) {
      # we already have the fallback as mentioned above
    }
  )
  
  return(fill_unused_strats(mixed_strategy, used_strats, nrow(payouts)))
}

is_valid_eq = function(payouts, eq) {
  # Any solution with one or zero strategies must be valid.
  if (length(eq) <= 1) {
    return(TRUE)
  }
  
  num_strats = nrow(payouts)
  expected_payout = numeric(num_strats)
  
  for (i in 1:num_strats) {
    expected_payout[i] = sum(eq * payouts[i, ])
  }
  
  # To be a valid equilibrium solution, the sum of the proportions of the time each strategy is
  # chosen must add up to 1; no solution may be used less than 0% of the time; and all used
  # strategies' payouts must be equal to the maximum payout.
  all_add_up_to_1 = abs(sum(eq) - 1) < ROUNDING_ERROR
  none_below_0 = all(eq >= -ROUNDING_ERROR)
  all_used_have_max_payout = all(expected_payout[eq > 0] >= max(expected_payout) - ROUNDING_ERROR)
  
  return(all_add_up_to_1 & none_below_0 & all_used_have_max_payout)
}

find_equilibrium = function(payouts) {
  find_eq = function(payouts, up_to = nrow(payouts), include = NULL) {
    current_eq = eq_with_given_strats(payouts, include)
    best_payout = sum(payouts[1, ] * current_eq) + ROUNDING_ERROR
    if (!is.null(include)) {
      best_payout = sum(payouts[include[1], ] * current_eq) + ROUNDING_ERROR
    }
    
    for (i in 1:up_to) {
      new_payout = sum(payouts[i, ] * current_eq)
      if ((new_payout > best_payout) | !is_valid_eq(payouts[include, include], current_eq[include])) {
        if (i == 1) {
          current_eq = eq_with_given_strats(payouts, c(i, include))
        } else {
          current_eq = find_eq(payouts, i - 1, c(i, include))
        }
        
        best_payout = sum(payouts[match(TRUE, current_eq > 0), ] * current_eq) + ROUNDING_ERROR
      }
    }
    
    return(current_eq)
  }
  
  find_eq(payouts)
}