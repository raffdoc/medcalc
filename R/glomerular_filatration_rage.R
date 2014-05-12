gfr <- function(gender="female|male",scr=NULL,scr.u="mg/dl"|"µmol/l",age=NULL, race="black|white"){
  ## from A New Equation to Estimate Glomerular Filtration Rate
  ## Model should be reweritten as in article - Sun May  4 21:33:42 CEST 2014
  ## Creatinine levesl must be converted from mg/dL to µmol/l and viceversa with facor of mg/dl  88.4	µmol/l
        if (scr.u=="µmol/l") { 
                if (race=="black"){
                        if (gender=="female") {
                                if (scr <= 62) {
                                        gfr <- 166*((scr/0.7)^-0.329)*((0.993)^age); return(gfr)
                                } else  gfr <- 166*((scr/0.7)^-1.209)*((0.993)^age); return(gfr)
                        } else if (scr <= 80){
                                gfr <- 163*((scr/0.9)^-0.411)*((0.993)^age) ; return(gfr)
                        } else  gfr <- 163*((scr/0.9)^-1.209)*((0.993)^age) ; return(gfr)
                } else if (gender=="female") {
                        if (scr <= 62) {
                                gfr <- 144*((scr/0.7)^-0.329)*((0.993)^age); return(gfr)
                        } else  gfr <- 144*((scr/0.7)^-1.209)*((0.993)^age); return(gfr)
                } else if (scr <= 80){
                        gfr <- 141*((scr/0.9)^-0.411)*((0.993)^age); return(gfr)
                } else  gfr <- 141*((scr/0.9)^-1.209)*((0.993)^age); return(gfr)
        } else  
  return(gfr)
}