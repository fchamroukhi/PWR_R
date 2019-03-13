show_PWR_results = function (t, y, PWR, yaxislim){

t=x
PWR=pwr

couleur = rainbow(K)

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

X11(title='Time series, PWR regimes, and segmentation')
plot(t, y, col = "black", type = "l", lty = "solid", xlab = "Time", ylab = "y",main='Time series, PWR regimes, and segmentation')


if (missing(yaxislim)) { yaxislim = c(mean(y)-2*sd(y), mean(y)+2*sd(y)) }

  if (ncol(y) != 1) y=t(y)
  if (ncol(t) != 1) t=t(t)

n= nrow(y)
K = ncol(PWR$param$betak)

gammak = pwr$param$gammak    # index of the transition time points (ou t(gammak): the time points)

## time series, regressors, and segmentation

for (k in 1:K){
    model_k = pwr$stats$regressors[,k]
    
    active_model_k = model_k[pwr$stats$klas==k] 
    active_period_model_k = t[pwr$stats$klas==k]
    
    inactive_model_k = model_k[pwr$stats$klas != k] 
    inactive_period_model_k = t[pwr$stats$klas != k] 
   
    if (!is.null(active_model_k)){
        par(new=TRUE)
        lines(inactive_period_model_k,inactive_model_k,col = couleur[k],type = "p", lty = 3, lwd = .5,pch=20, cex=0.1)
        par(new=TRUE)
        lines(active_period_model_k, active_model_k,col = couleur[k],lty = "solid", lwd = 4)
    }
}


## time series, estimated regression function, and optimal segmentation

X11(title="Time series, PWR function, and segmentation")
plot(t, y, col = "black", type = "l", lty = "solid", xlab = "Time", ylab = "y",lwd=1,main="Time series, PWR function, and segmentation")
gammak=c(0, gammak, n)
par(new=TRUE) 

for (k in 1:K){
    Ik = (gammak[k]+1):gammak[k+1]
    segmentk= pwr$stats$mean_function[Ik]
    par(new=TRUE) 
    lines(t[Ik],segmentk,col = 'red',type = "l", lty = "solid", lwd = 2)
}
par(new=TRUE) 
for (i in 1:length(t[pwr$param$gammak])){
  par(new=TRUE) 
  abline(v=t[pwr$param$gammak][i], lty = "dotted", col = 'black', lwd = 2)
  }

}