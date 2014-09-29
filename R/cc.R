#' Cockcroft–Gault formula for creatinin clearance calculation
#' @param age is the anagraphical age of the patients expressed in years
#' @param weight is the patients' mass expressed in kilograms
#' @param sex is the gender of the patients expresed as 0 for Male and 1 for Female
#' @param scre is the serum creatinin expressed in mg/dL units.
#' @return cc.out is vector of the creatinine clearance.
#' @export

cc <- function(age,weight,sex,scre){
        if((length(age)==length(weight))==(length(sex)==length(scre))){
                cc.out <- NULL
                for(i in 1:length(age))
                        if(sex[i]==0){
                                cc.out[i] <- ((140-age[i])*weight[i])/72*scre[i]
                        }
                else{
                        cc.out[i] <- (((140-age[i])*weight[i])*0.85)/72*scre[i]
                }
        }
        else{
                stop("All variables must be same lenght")
        }
        return(cc.out)
}