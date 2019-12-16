
# unit of measure errors
if ( other.rev   >= 500*vat) other.rev <- other.rev/1000
if ( turnover    >= 500*vat) turnover  <- turnover/1000
if ( total.rev   >= 500*vat) total.rev <- total.rev/1000

if ( abs(profit) >= 100*vat) profit    <- profit/1000
if ( staff.costs   >= 25*vat) staff.costs <- staff.costs/1000
if ( total.costs   >= 25*vat) total.costs <- total.costs/1000

if ( other.rev   >= 500*staff) other.rev <- other.rev/1000
if ( turnover    >= 500*staff) turnover  <- turnover/1000
if ( total.rev   >= 500*staff) total.rev <- total.rev/1000
if ( abs(profit) >= 500*staff) profit    <- profit/1000

if ( staff.costs   >= 500*staff) staff.costs <- staff.costs/1000
if ( total.costs   >= 500*staff) total.costs <- total.costs/1000
