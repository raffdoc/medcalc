# #' @param y is continious variable.
# #' @param x is a character which will become factor inside the fuction with discret levels
# #' @return plot in base graphics
# #' @export
# 
# 
# myboxplot <- function(formula=y~x,...){
#         source("~/Dropbox/R_Scripts/glob_r_scr/print_pval.R")
#         y <- round(y, digits = 1)
#         x.f <- factor(x)
#         lab<-as.character(levels(x.f))
#         oldpar <- par()
#         lab.n <- c(paste("n=",table(x)[1]),paste("n=",table(x)[2]))
#         par(mar=c(5, 1.5, 2, 1), mgp=c(2, 0.3, 0), tcl=0.3, xaxs="r", pty="s")
#         boxplot(y~x,ylab=expression(paste("Y name", "[units]")),frame=FALSE,
#                 ylim=c(min(y),max(y)),main="",xlim=c(min(as.numeric(levels(x.f))-1),max(as.numeric(levels(x.f))+1)), 
#                 outline=FALSE, whisklty=1,srt=45, xaxt = "n",axes=FALSE)
#         axis(2, at=seq(min(y),max(y),max(y)/5),las=1)
#         axis(1, at=1:max(levels(x.f)), lwd = 0,lab=FALSE)
#         text(1:max(as.integer(levels(x.f))),par("usr")[3] - max(y)/20, srt=45, adj=0.85, labels=lab, xpd=T, cex=0.7)
#         text(1:max(as.integer(levels(x.f))),par("usr")[3] - max(y)/10, srt=0, adj=0.85, labels=lab.n, xpd=T, cex=0.7)
#         mtext("A", side=2, line=2,font=2,las=1,padj=-12,cex=1) 
#         text(1.5, 95, print.pval.plot(wilcox.test(y~x)$p.value))
#         #text(1,105,"n = 6",col="blue")
#         #text(2,105, "n = 3",col="red")
#         segments(x0=1, y0=max(y)-max(y)/20, x1=1, y1=max(y)-max(y)/10)
#         segments(x0=1, y0=max(y)-max(y)/10, x1=2, y1=max(y)-max(y)/10)
#         segments(x0=2, y0=max(y)-max(y)/20, x1=2, y1=max(y)-max(y)/20)
# }
# 
# dat <- NULL
# y <- rnorm(100,mean = 50, sd=10)
# x <- rep(x = 1:2,times=50)