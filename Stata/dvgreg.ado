
* 1.0.1 dvg Kåre Vernby & Karl-Oskar Lindgren, 03april 2009

program define dvgreg, eclass sortpreserve
  
	version 8
	
	syntax varlist(min=2 ts) [if] [in], TIME(varname) ID(varname) ///
	[rhomin(real .1) rhomax(real .9) rhostep(real .1) CLuster(varname) ROBust]

	tokenize `varlist'
	local Y `1'
	macro shift
	local XVEC `*'
	
	local LOOP `rhomin'(`rhostep')`rhomax'
  
  tempvar seq seq2 invtime s T touse
  
  tempname maxr2
  scalar `maxr2'=-99
 
  
  if "`cluster'"!="" {
    local copt "cluster(`cluster')"
  }

quietly {  

//Generate the variables s and T that keep track of the length of gaps
  bysort `id' (`time'): egen `seq'=seq() if ~mi(`Y') 
  gen `seq2'=`seq'
  gen `invtime'=-`time'
  
  tsset `id' `time'
  replace `seq2'=L.`seq2' if mi(`seq2')
  bysort `id' `seq2' (`invtime'): gen `s'=_n
  replace `s'=0 if ~mi(`Y')
  
  replace `seq2'=`seq'-1 if `s'==0

  tsset `id' `seq'
  gen `T'=d.`time' if ~mi(`Y')
  
  tempvar Ys w Xmiss
  gen `Ys'=.
  gen `w'=.
  gen `Xmiss'=.
  
  foreach var of varlist `XVEC' {
      
      local q=`q'+1
      tempvar X`var' grdel`q'
      
      gen `X`var''=.
      
      //The next two lines are used to identify missing values in X(i,t-s)
      replace `Xmiss'=1 if mi(`var')
      bysort `id' `seq2' (`time'): egen `grdel`q''=count(`Xmiss')
  }    

  tempvar constant
  gen double `constant'=1
   
 //Loop over the numlist holding the rhos 
  foreach a of numlist `LOOP' { 
    tsset `id' `seq' 
    local j=`j'+1
    local q=0

//Generate the dependent variable    
    replace `Ys'=`Y'-((1-`a')^`T')*L.`Y' if ~mi(`Y')
  
//Generate independent variables and weights   
    foreach var of varlist `XVEC' {
      local q=`q'+1
      replace `X`var''=(1-`a')^`s'*`var'
      bysort `id' `seq2' (`time'): replace `X`var''=sum(`a'*`X`var'')
      replace `X`var''=. if mi(`Y') | `seq'==1
      
//Set X(i,t) to missing if any of the previous Xs in this `group' are missing
      replace `X`var''=. if `grdel`q''!=0  
    } 
    
    replace `w'=(1-`a')^`s'
    bysort `id' `seq2' (`time'): replace `w'=sum(`w')
    replace `w'=1/`w'
    replace `w'=. if mi(`Y') | `seq'==1
  
//Weigh dependent and independent variables
    replace `Ys'=`w'*`Ys'
   
    foreach var of varlist `XVEC' {
      replace `X`var''=`w'*`X`var''
      local WXVEC`j' `WXVEC`j'' `X`var''  
    }

/*Run the regressions from rhomin to rhomax in increments of rhostep and grab the 
  model with the highest adjusted R-square.*/                                          
     
    replace `constant'=`a'
  
    reg `Ys' `WXVEC`j'' `constant' `if' `in', depname("`Y'") `robust' `copt' nocons tsscons
    
    if e(r2)>`maxr2' {
    
      //Save and store estimates to be redisplayed later
      matrix Mb=e(b)
      matrix colnames Mb=`XVEC' _cons
      matrix B=Mb
      ereturn repost b=Mb, rename
      matrix VB=e(V)
      
      tempname N r2 r2_a ll ll_0 df_m df_r rmse mss rss rho 
      
      scalar `maxr2'=e(r2)
      scalar `N'    =e(N)
      scalar `r2'   =e(r2)
      scalar `r2_a' =e(r2_a)
      scalar `ll'   =e(ll)
      scalar `df_m' =e(df_m)
      scalar `df_r' =e(df_r)
      scalar `rmse' =e(rmse)
      scalar `mss'  =e(mss)
      scalar `rss'  =e(rss)
      scalar `rho'  =`a'
      
      capture drop `touse'
      gen `touse'=e(sample)
      
      estimates store bestfit,title(rho=`a')
      capture drop _est_model* 
    }  
  }
}

//Display the result for the model with the best fit.                                  
 ereturn clear
 ereturn post B VB, esample(`touse') 
 
 ereturn scalar rho   =`rho' 
 ereturn scalar rss   =`rss' 
 ereturn scalar mss   =`mss'
 ereturn scalar rmse  =`rmse'
 ereturn scalar df_r  =`df_r'
 ereturn scalar df_m  =`df_m'
 ereturn scalar ll    =`ll'
 ereturn scalar r2_a  =`r2_a'
 ereturn scalar r2    =`r2' 
 ereturn scalar N     =`N'   
 ereturn local cmd "dvgreg"
 ereturn local depvar "`Y'" 
 
 estimates replay bestfit
 qui capture drop _est_bestfit*

end


