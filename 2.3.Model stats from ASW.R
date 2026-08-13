## Different model checks from ASW:

library(bayestestR)

p_direction(inv_recovermodr)
p_direction(inv_resilience24_modr)
p_direction(inv_resistmodr)

p_direction(nat_recovermodr)
p_direction(nat_resilience24_modr)
p_direction(nat_resistmodr)

p_direction(recov_modr)
p_direction(resil_modr)
p_direction(resist_modr)




# Calculate exact percent change for each coefficient usiing the medians from Figure 3
# They are all on log response ratios, so this will calculate % change:

## YOU HAVE TO RUN FIG 3A FIRST THOUGH, since I pull that df, "posteriors":
posteriors$percentchange <- (exp(posteriors$.value) - 1) * 100

