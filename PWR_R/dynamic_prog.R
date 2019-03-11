dynamic_prog = function(matJ, K) {
  #################################################################################
  # [J, t_est]= progdyn(matJ,Kmax)
  # dynamic_prog : algorithme de programmation dynamique
  #   matJ(i,j) = sum_{t=i}^{t=j}(xt-mu)^2
  # avec mu = beta'*r_i : un polynome d'ordre p;
  # ici beta se calcule pour chaque (i,j).
  #
  # ENTREES:
  #
  #        matJ : matrice cout pour chaque couple (i,j) qui est J_1
  #               matJ(i,j) = sum_{t=i}^{t=j}(xt-mu)^2
  #        Kmax : nbre maximal de classes.
  #
  # SORTIES:
  #
  #        J : vecteur de dim [K x 1] qui contient la valeur du critere
  #        correspondant à chaque valeur du nombre de classes k qui varie de 1 jusqu'à K.
  #
  #
  #        J = min_k (sum_k sum_{t=i}^{t=j}(xt-muk)^2)
  #
  #        t_est : temps de changement (partition) estimés (estimée) pour
  #        chaque valeur de k. matrice de dim[Kmax,Kmax]
  #
  #
  #
  ###############################################################################
  
  
  n = nrow(matJ)
  I = matrix(Inf , nrow = K , ncol = n)
  t = matrix(0 , nrow = (K - 1) , ncol = n)
  
  
  I[1,] = matJ[1,]
  if (K > 2) {
    for (k in 2:(K - 1)) {
      for (L in 2:n) {
        I[k,L]   = min(I[k - 1,1:L - 1] + t(matJ[2:L,L]))
        t[k - 1,L] = which.min(I[k - 1,1:L - 1] + t(matJ[2:L,L]))
      }
    }
  }
  
  I[K,n]   = min(I[K - 1,1:n - 1] + t(matJ[2:n,n]))
  t[K - 1,n] = which.min(I[K - 1,1:n - 1] + t(matJ[2:n,n]))
  
  J = I[,n]
  
  # calculates the change point instants
  t_est = n * diag(K)
  
  for (K in 2:K) {
    for (k in (K - 1):1) {
      t_est[K,k] = t[k,t_est[K,k + 1]]
    }
  }
  
  
  
  solution = list()
  solution$t_est = t_est
  solution$J = J
  
  return(solution)
  
}
