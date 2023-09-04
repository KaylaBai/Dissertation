cd /Users/kerubai/Desktop/Stata_Analysis 


use data, clear
gen RD2=RD^2


*=============================================
*            Descriptive Statistics
*=============================================

sum2docx LNGRPpc RD RD2 LNFDI LNINN URB using descriptive.docx, replace ///
		 stats(N mean(%9.3f) sd min(%9.3f) max(%9.3f))  ///
		 title("Table 1 Descriptinve Data Analysis")
		 
*==========================
*     Moran'I Index
*==========================
spatwmat using 相邻权重, name(w1) standardize
spatwmat using 地理距离权重, name(w2) standardize
spatwmat using 经济距离权重, name(w3) standardize

mat Moran=J(13, 5, 0)
local j=1
forvalues i=2005/2017 {
       preserve
       keep if year==`i'
       spatgsa RD, weights(w1) moran 
       restore
       mat M=r(Moran)
       mat Moran[`j++', 1]=(M)
}
matrix rownames Moran = 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017  //  Define row names
matrix colnames Moran = I E(I) sd(I) Z P-value    //  Define column names
matlist Moran, format(%4.3f) title(" Global Moran's I Result ")  


*==========================
*     Tests for Models
*==========================
*============Fixed or Random
xtset id year
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(both) nolog effects
est store fe
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, re model(sdm) wmat(w1) type(both) nolog effects
est store re
hausman fe re

*============Region, Time or Both Fixed
xtset id year
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(ind) nolog effects
est store fe1
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(time) nolog effects
est store fe2
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(both) nolog effects
est store fe3

lrtest fe3 fe1, df(12)
lrtest fe3 fe2, df(12)       //Support Two-way Fixed Model

*============SDM Back to sem、sar（WALD Test）

xtset id year
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(both) nolog effects

test [Wx]RD=[Wx]RD2=[Wx]LNFDI=[Wx]LNINN=[Wx]URB=0        //Wald Test

testnl([Wx]RD=-[Spatial]rho*[Main]RD)([Wx]RD2=-[Spatial]rho*[Main]RD2)  ///
([Wx]LNFDI=-[Spatial]rho*[Main]LNFDI)([Wx]LNINN=-[Spatial]rho*[Main]LNINN)    ///
([Wx]URB=-[Spatial]rho*[Main]URB)


*============SDM Back to sem、sar（LR Test）
xtset id year
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sem) emat(w1) type(both) nolog effects
est store fe1
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sar) wmat(w1) type(both) nolog effects
est store fe2
xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w1) type(both) nolog effects
est store fe3

lrtest fe3 fe1, df(12)
lrtest fe3 fe2, df(12)       

*==========================
*   Model Estimation
*==========================
preserve
xtset id year


xsmle LNGRPpc RD RD2 LNFDI LNINN URB, fe model(sdm) wmat(w3) type(both, leeyu) nolog effects   
est store sdm1

restore
*Output to the word
esttab sdm1 using myresult.rtf, scalar(r2_w N) compress ///
star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.3f) nogaps replace

		 







