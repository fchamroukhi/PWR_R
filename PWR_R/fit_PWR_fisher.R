library(matlib)
fit_PWR_fisher = function(x, y, K, p) {
  ##################################################################
  # Algorithme de fisher de regression par morceaux pour la paramétrisation
  # des signaux de manoeuvre d'aiguillages en utilisant la programmation dynamique.
  # les parametres à estimer sont:
  
  # 1. les temps de changement de différentes phases du signal
  #
  # Une fois la partition estimée, on calcule les coefficients de regression
  # associés à chaque segment ainsi que la variance du bruit sur chaque segment.
  # 2. les parametres de regression de chaque phase du signal
  # 3. les variances du bruit additif sur chaque phase
  #  La méthode d'estimation est le maximum de vraisemblance.
  #
  #
  #
  # Faicel Chamroukhi Decembre 2008.
  ##################################################################
  
  
  if (ncol(y) != 1) {
    y = as.numeric(t(y))
  }
  if (ncol(x) != 1) {
    x = as.numeric(t(x))
  }
  Lmin = p + 1
  
  n = length(y) 
  
  #A voir
  # warning off
  
  X = designmatrix(x,p)
  ### Initialisation : calcul de J_1;
  
  start_time <- Sys.time()
  ### matrice "coÃ»t"
  
  
  C1 = cost_matrix_PPWR(x, y, p, Lmin) #OK
  
  ### dynamic programming
  solution = dynamic_prog(C1, K)  #oK
  Ck = solution$J             #OK
  t_est = solution$t_est          #oK
  
  gammak = c(0,t_est[nrow(t_est),])  # change points
  
  # estimation of the corresponding regression coefficients
  mean_function = matrix(0 , nrow = n , ncol = 1)
  
  ##################################################################
  #Initialisation
  #Aline CANARD 2/2/2019
  ##################################################################
  betak = matrix(0 , nrow = p + 1 , ncol = K)
  sigma2k = c(rep(0,K))
  ##################################################################
  
  for (k in 1:K) {
    i = gammak[k] + 1
    j = gammak[k + 1]
    nk = j - i + 1
    yij = y[i:j]
    X_ij = X[i:j,]
    betak[,k] = solve(crossprod(X_ij, X_ij)) %*% crossprod(X_ij, yij)
    z = yij - X_ij %*% betak[,k]
    sigma2k[k] = crossprod(z, z) / nk            #variances
    mean_function[i:j,] = X_ij %*% betak[,k]
  }
  
  # classes estimees:
  
  klas = rep(0, n)
  Zik  = matrix(0 , nrow = n , ncol = K)
  
  for (k in 1:K)  {
    i = gammak[k] + 1
    j = gammak[k + 1]
    klas [i:j] = k
    Zik[i:j,k] = 1
  }
  
  PWR = list()
  PWR$param = list()
  PWR$stats = list()
  
  PWR$param$betak = betak
  PWR$param$sigma2k = sigma2k
  PWR$param$gammak = gammak[2:(length(gammak) - 1)]    #sans le 0 et le n
  PWR$param$parameter_vector = c(as.vector(PWR$param$gammak), as.vector(PWR$param$betak), as.vector(PWR$param$sigma2k))
  
  PWR$stats$klas = klas
  PWR$stats$mean_function = mean_function
  PWR$stats$regressors = X %*% PWR$param$betak
  PWR$stats$Zik = Zik
  PWR$stats$objective = Ck[length(Ck)]
  PWR$stats$cputime = Sys.time() - start_time
  
  return(PWR)
  
}
