gfr <- function(gender=NULL,scr=NULL,scr.u="mg/dl"|"µmol/l",age=NULL){
  ## from A New Equation to Estimate Glomerular Filtration Rate
  ## Model should be reweritten as in article - Sun May  4 21:33:42 CEST 2014
  ## Creatinine levesl must be converted from mg/dL to µmol/l and viceversa with facor of mg/dl  88.4	µmol/l
  
  
  k <- NULL
  k <- ifelse(gender=="male",0.9,0.7)
  alph <- NULL
  alph <- ifelse(gender=="male",-0.411,-0.329)
  iffm <- NULL
  #iffm <- ifelse(gender="female",1,I(^0))
  gfr <- 141*min((scr/k)^alph)*(0.993^age)#*1.018()
  return(gfr)
}