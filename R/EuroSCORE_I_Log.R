#' EuroScore is a mortality estimation model used in cardiovascular medicine, particularly in cardiac surgery.
#' @param x is datframe with all variables included in orginal score from 1999
#' @return returns additive score added othe data frame wiht new variabel of Logistic EuroScore.
#' @export


EuroScore_I_Log  <- function(x,...) {
        if (is.null(x)) 
                stop("Dataframe must be specified", call. = FALSE)
        if (!is.data.frame(x)) {
                stop("Data must be a dataframe", call. = FALSE)
        }
        x$log.es <- NULL
        # age variable scoring by age groups
        x$log.age <- NULL
        for(i in seq(along=x$age)) { 
                if (x$age[i]< 59) { x$log.age[i] <- 0.0666354 } 
                else {x$log.age[i] <- 0.0666354*(x$age[i]-58)}
        }
        #sex variable
        x$log.sex <- NULL
        for (i in seq(along=x$sex)) {
                if(x$sex[i]==1){x$log.sex[i]<-0.3304052} else {x$log.sex[i]<- 0}
        }
        
        #Chronic Pulmonary disease (Long term use of bronchodilators or steroids for lung disease)
        x$log.cpd <- NULL
        for (i in seq(along=x$cpd)) {
                if(x$cpd[i]==1){x$log.cpd[i]<-0.4931341} else {x$log.cpd[i]<- 0}
        }
        # Extracardiac arteriopathy (One or more of claudication, carotid occlusion  
        # or >50% stenosis, previous or planned intervention on the abdominal aorta, 
        #limb arteries or carotids)
        x$log.eca <- NULL
        for (i in seq(along=x$eca)) {
                if(x$eca[i]==1){x$log.eca[i]<-0.6558917} else {x$log.eca[i]<- 0}
        }
        
        # Neurological disfunction (Disease severely affecting ambulation or day-to-day functioning)
        x$log.nd <- NULL
        for (i in seq(along=x$nd)) {
                if(x$nd[i]==1){x$log.nd[i]<-0.6558917} else {x$log.nd[i]<- 0}
        }
        # Previous Cardiac Surgery
        x$log.pcs <- NULL
        for (i in seq(along=x$pcs)) {
                if(x$pcs[i]==1){x$log.pcs[i]<-1.002625} else {x$log.pcs[i]<- 0}
        }
        # Creatinin level > 200 µmol/ L
        x$log.creat <- NULL
        for (i in seq(along=x$creat)) {
                if(x$creat[i]==1){x$log.creat[i]<-0.6521653} else {x$log.creat[i]<- 0}
        }
        # Active endocardities (Patient still on antibiotic treatment for endocarditis at time of surgery)
        x$log.ae <- NULL
        for (i in seq(along=x$ae)) {
                if(x$ae[i]==1){x$log.ae[i]<-1.101265} else {x$log.ae[i]<- 0}
        }
        # Critical perioperative state (Ventricular Tachycardia / Ventricular Fibrillation or aborted sudden death, preoperative cardiac massage, preoperative ventilation before anaesthetic room, preoperative inotropes or IABP, preoperative Acute Renal Failure (anuria or oliguria <10ml/hr))
        x$log.cps <- NULL
        for (i in seq(along=x$cps)) {
                if(x$cps[i]==1){x$log.cps[i]<-0.9058132} else {x$log.cps[i]<- 0}
        }
        # Anstable angina ( Rest angina requiring i.v. nitrates until arrival in anaesthetic room)
        x$log.ua <- NULL
        for (i in seq(along=x$ua)) {
                if(x$ua[i]==1){x$log.ua[i]<-0.9058132} else {x$log.ua[i]<- 0}
        }
        # LV function espresed as EF
        x$log.lv.ef <- NULL
        for(i in seq(along=x$lv.ef)) {
                if (x$lv.ef[i]>50) { x$log.lv.ef[i] <- 0 } else { 
                        if (x$lv.ef[i]>30) { x$log.lv.ef[i] <- 0.4191643 } else { x$log.lv.ef[i] <- 1.094443}
                }}
        # Recent myocardial infarction (Myocardial infarction within 90 days)
        x$log.rmi <- NULL
        for (i in seq(along=x$rmi)) {
                if(x$rmi[i]==1){x$log.rmi[i]<-0.5460218} else {x$log.rmi[i]<- 0}
        }
        # Pulmonary hypertension (Systolic pulmonary artery pressure >60mmHg)
        x$log.ph <- NULL
        for (i in seq(along=x$ph)) {
                if(x$ph[i]==1){x$log.ph[i]<-0.7676924} else {x$log.ph[i]<- 0}
        }
        # Emergency (Operation before beginning of next working day)
        x$log.em <- NULL
        for (i in seq(along=x$em)) {
                if(x$em[i]==1){x$log.em[i]<-0.7127953} else {x$log.em[i]<- 0}
        }
        # Other then isolated CABG 
        x$log.ot.icabg <- NULL
        for (i in seq(along=x$ot.icabg)) {
                if(x$ot.icabg[i]==1){x$log.ot.icabg[i]<-0.5420364} else {x$log.ot.icabg[i]<- 0}
        }
        # Surgery on thoracic aorta
        x$log.sta <- NULL
        for (i in seq(along=x$sta)) {
                if(x$sta[i]==1){x$log.sta[i]<-1.159787} else {x$log.sta[i]<- 0}
        }
        # Post infactual septal rupture
        x$log.pisr <- NULL
        for (i in seq(along=x$pisr)) {
                if(x$pisr[i]==1){x$log.pisr[i]<- 1.159787} else {x$log.pisr[i]<- 0}
        }
        # addative score output
        for (i in seq(along=x$log.age)){
                x$log.es[i] <- (exp(-4.789594+x$log.age[i]+x$log.sex[i]+x$log.cpd[i]+x$log.eca[i]+x$log.nd[i]+x$log.pcs[i]+ x$log.creat[i]+x$log.ae[i]+x$log.cps[i]+x$log.ua[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.ot.icabg[i]+x$log.sta[i]+x$log.pisr[i]
                )/(1+exp(-4.789594+x$log.age[i]+x$log.sex[i]+x$log.cpd[i]+x$log.eca[i]+x$log.nd[i]+x$log.pcs[i]+ x$log.creat[i]+x$log.ae[i]+x$log.cps[i]+x$log.ua[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.ot.icabg[i]+x$log.sta[i]+x$log.pisr[i]
                )))*100}
        #x<- data.frame(x,x$log.es)
        x.out <- subset(x,select=c(age,sex,cpd,eca,nd,pcs,creat,ae,cps,ua,lv.ef,rmi,ph,em,ot.icabg,sta,pisr,log.es))
        # return dataframe
        x.out <- data.frame(x.out)
        assign("x.out",x.out,pos=.GlobalEnv)
}
