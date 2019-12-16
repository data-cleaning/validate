
staff >= 0
turnover >= 0
other.rev >= 0

turnover + other.rev == total.rev

staff.costs >= 0

## The number of staff is conisidered a reliable variable, so we have a few
## rules comparing financial amounts with the number of staff.


# We do not expect employees to earn more than 1M for the  establishment
other.rev <= 1000 * staff
turnover  <= 1000 * staff
total.rev <= 1000 * staff
# note: profit can be negative, so we need both rules (2nd holds automatically 
# when profit is positive, first holds automatically when profit is negative).
 profit   <= 250 * staff
-profit   <= 250 * staff

 profit   <= 0.6 * turnover
-profit   <= 0.6 * turnover 
 
# We do not expect staff to make more than 100k / year
staff.costs <= 100 * staff

# We expect financial amounts to be within a reasonable factor of VAT
other.rev <= 0.5 * vat
turnover  <= 2 * vat
total.rev <= 2 * vat



total.costs >= staff.costs

profit == total.rev - total.costs

