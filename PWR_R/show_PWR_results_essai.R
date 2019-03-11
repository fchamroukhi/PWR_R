show_PWR_results_essai = function (t, y, PWR, yaxislim){

#install.packages("rapportools")
  #pour la fonction is.empty
  
# set(0,'defaultaxesfontsize',14);
  
  #G = length(solution$param$alpha_g)

couleur = rainbow(K)
#par(mfrow=c(round(sqrt(G + 1)), round(sqrt(G + 1))))

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

plot(t, y, col = "black", type = "l", lty = "solid", xlab = "Time", ylab = "y")
#title(main = "Initial dataset")    

if (missing(yaxislim)) { yaxislim = c(mean(y)-2*sd(y), mean(y)+2*sd(y)) }

  if (ncol(y) != 1) y=t(y)
  if (ncol(t) != 1) t=t(t)

n= nrow(y)
K = ncol(PWR$param$betak)

gammak = pwr$param$gammak    # index of the transition time points (ou t(gammak): the time points)

#comment faire ca ?
#set(0,'defaultaxesfontsize',14);

## time series, regressors, and segmentation

#scrsz = get(0,'ScreenSize'); retourne la resolution de l'ecran
#figure('Position',[50 scrsz(4) 560 scrsz(4)/2]);crée une fenetre de plot

for (k in 1:K){
    model_k = pwr$stats$regressors[,k]
    
    active_model_k = model_k[pwr$stats$klas==k] 
    active_period_model_k = t[pwr$stats$klas==k]
    
    inactive_model_k = model_k[pwr$stats$klas != k] 
    inactive_period_model_k = t[pwr$stats$klas != k] 
   
    if (!is.null(active_model_k)){
        par(new=TRUE)
        lines(inactive_period_model_k,inactive_model_k,col = couleur[k],type = "l", lty = "dotted", lwd = 0.5)
        par(new=TRUE)
        lines(active_period_model_k, active_model_k,col = couleur[k],lty = "solid", lwd = 2)
    }
}

title('Time series, PWR regimes, and segmentation')

#ylim(yaxislim);

## time series, estimated regression function, and optimal segmentation
par(mfrow = c(1, 1))

#figure('Position',[scrsz(4)/1.2 scrsz(4) 560 scrsz(4)/2]);
plot(t, y, col = "black", type = "l", lty = "solid", xlab = "Time", ylab = "y",lwd=1)
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
  #plot(rbind(t[pwr$param$gammak],t[pwr$param$gammak]),cbind(min(y)-3*sd(y), max(y)+3*sd(y)),col = 'black', lwd = 2)
  par(new=TRUE) 
  abline(v=t[pwr$param$gammak][i], lty = "dotted", col = 'black', lwd = 2)
  }
title('Time series, PWR function, and segmentation')

}