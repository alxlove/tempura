*machtei@unc.edu
program define dvpaul
  version 8
  syntax varlist(min=2 ts) [if] [in], TIME(varname) ID(varname) ///
	[CLuster(varname) ROBust] 
 preserve
  tokenize "varlist"
  local varall "`varlist' `id' `time'"
    keep `varall'
	  if "`cluster'"!="" {
    local copt "cluster(`cluster')"
  }
    egen any_missing = rowmiss(`varall')
    drop if any_missing
    bysort `id' (`time'): egen mint = min(`time')
    bysort `id' (`time'): gen t = `time' - mint
    gen t_squared = t^2
    gen t_cubed = t^3
  reg `varlist' t t_squared t_cubed `if' `in', `robust' `copt' nocons tsscons
restore
  end
