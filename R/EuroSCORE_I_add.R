#' EuroScore is a mortality estimation model used in cardiovascular medicine, particularly in cardiac surgery.
#' @param x is datframe with all variables included in orginal score from 1999
#' @return returns additive score added othe data frame wiht new variabel of additive EuroScore.
#' @export


EuroScore_I_add <- function(x,...) {
        if (is.null(x)) 
                stop("Dataframe must be specified", call. = FALSE)
        if (!is.data.frame(x)) {
                stop("Data must be a dataframe", call. = FALSE)
        }
        x$a.es <- NULL
        # age variable scoring by age groups
        x$phi.age <- NULL
        for(i in seq(along=x$age)) { 
                if (x$age[i]>94.99) { x$phi.age[i] <- 8 } else { 
                        if (x$age[i]>89.99) { x$phi.age[i] <- 7 } else { 
                                if (x$age[i]>84.99) { x$phi.age[i] <- 6 } else {
                                        if (x$age[i]>79.99) { x$phi.age[i] <- 5 } else {
                                                if (x$age[i]>74.99) { x$phi.age[i] <- 4 } else {
                                                        if (x$age[i]>69.99) { x$phi.age[i] <- 3 } else {
                                                                if (x$age[i]>64.99) { x$phi.age[i] <- 2 } else {
                                                                        if (x$age[i]>59.99) { x$phi.age[i] <- 1 } else {
                                                                                x$phi.age[i] <- 0}}}}}}}}
        }
        #sex variable
        x$phi.sex <- NULL
        for (i in seq(along=x$sex)) {
                if(x$sex[i]==1){x$phi.sex[i]<-1} else {x$phi.sex[i]<- 0}
        }
        
        #Chronic Pulmonary disease (Long term use of bronchodilators or steroids for lung disease)
        x$phi.cpd <- NULL
        for (i in seq(along=x$cpd)) {
                if(x$cpd[i]==1){x$phi.cpd[i]<-1} else {x$phi.cpd[i]<- 0}
        }
        # Extracardiac arteriopathy (One or more of claudication, carotid occlusion  or >50% stenosis, previous or planned intervention on the abdominal aorta, limb arteries or carotids)
        x$phi.eca <- NULL
        for (i in seq(along=x$eca)) {
                if(x$eca[i]==1){x$phi.eca[i]<-2} else {x$phi.eca[i]<- 0}
        }
        
        # Neurological disfunction (Disease severely affecting ambulation or day-to-day functioning)
        x$phi.nd <- NULL
        for (i in seq(along=x$nd)) {
                if(x$nd[i]==1){x$phi.nd[i]<-2} else {x$phi.nd[i]<- 0}
        }
        # Previous Cardiac Surgery
        x$phi.pcs <- NULL
        for (i in seq(along=x$pcs)) {
                if(x$pcs[i]==1){x$phi.pcs[i]<-3} else {x$phi.pcs[i]<- 0}
        }
        # Creatinin level > 200 µmol/ L
        x$phi.creat <- NULL
        for (i in seq(along=x$creat)) {
                if(x$creat[i]==1){x$phi.creat[i]<-2} else {x$phi.creat[i]<- 0}
        }
        # Active endocardities (Patient still on antibiotic treatment for endocarditis at time of surgery)
        x$phi.ae <- NULL
        for (i in seq(along=x$ae)) {
                if(x$ae[i]==1){x$phi.ae[i]<-3} else {x$phi.ae[i]<- 0}
        }
        # Critical perioperative state (Ventricular Tachycardia / Ventricular Fibrillation or aborted sudden death, preoperative cardiac massage, preoperative ventilation before anaesthetic room, preoperative inotropes or IABP, preoperative Acute Renal Failure (anuria or oliguria <10ml/hr))
        x$phi.cps <- NULL
        for (i in seq(along=x$cps)) {
                if(x$cps[i]==1){x$phi.cps[i]<-3} else {x$phi.cps[i]<- 0}
        }
        # Anstable angina ( Rest angina requiring i.v. nitrates until arrival in anaesthetic room)
        x$phi.ua <- NULL
        for (i in seq(along=x$ua)) {
                if(x$ua[i]==1){x$phi.ua[i]<-2} else {x$phi.ua[i]<- 0}
        }
        # LV function espresed as EF
        x$phi.lv.ef <- NULL
        for(i in seq(along=x$lv.ef)) {
                if (x$lv.ef[i]>50) { x$phi.lv.ef[i] <- 0 } else { 
                        if (x$lv.ef[i]>30) { x$phi.lv.ef[i] <- 1 } else { x$phi.lv.ef[i] <- 3}
                }}
        # Recent myocardial infarction (Myocardial infarction within 90 days)
        x$phi.rmi <- NULL
        for (i in seq(along=x$rmi)) {
                if(x$rmi[i]==1){x$phi.rmi[i]<-2} else {x$phi.rmi[i]<- 0}
        }
        # Pulmonary hypertension (Systolic pulmonary artery pressure >60mmHg)
        x$phi.ph <- NULL
        for (i in seq(along=x$ph)) {
                if(x$ph[i]==1){x$phi.ph[i]<-2} else {x$phi.ph[i]<- 0}
        }
        # Emergency (Operation before beginning of next working day)
        x$phi.em <- NULL
        for (i in seq(along=x$em)) {
                if(x$em[i]==1){x$phi.em[i]<-2} else {x$phi.em[i]<- 0}
        }
        # Other then isolated CABG 
        x$phi.ot.icabg <- NULL
        for (i in seq(along=x$ot.icabg)) {
                if(x$ot.icabg[i]==1){x$phi.ot.icabg[i]<-2} else {x$phi.ot.icabg[i]<- 0}
        }
        # Surgery on thoracic aorta
        x$phi.sta <- NULL
        for (i in seq(along=x$sta)) {
                if(x$sta[i]==1){x$phi.sta[i]<-3} else {x$phi.sta[i]<- 0}
        }
        # Post infactual septal rupture
        x$phi.pisr <- NULL
        for (i in seq(along=x$pisr)) {
                if(x$pisr[i]==1){x$phi.pisr[i]<-4} else {x$phi.pisr[i]<- 0}
        }
        # addative score output
        for (i in seq(along=x$phi.age)){
                x$a.es[i] <- x$phi.age[i]+x$phi.sex[i]+x$phi.cpd[i]+x$phi.eca[i]+x$phi.nd[i]+x$phi.pcs[i]+ x$phi.creat[i]+x$phi.ae[i]+x$phi.cps[i]+x$phi.ua[i]+x$phi.lv.ef[i]+x$phi.rmi[i]+x$phi.ph[i]+x$phi.em[i]+x$phi.ot.icabg[i]+x$phi.sta[i]+x$phi.pisr[i]
        }
        #x<- data.frame(x,x$a.es)
        x.out <- subset(x,select=c(age,sex,cpd,eca,nd,pcs,creat,ae,cps,ua,lv.ef,rmi,ph,em,ot.icabg,sta,pisr,a.es))
        # return dataframe
        x.out <- data.frame(x.out)
        #return(x.out)
        assign("x.out",x.out,pos=.GlobalEnv)
}
