library(matlib)

cost_matrix_PPWR = function(t, y, p, Lmin) {
  ###############################################################################
  # matJ = cost_matrix_PPWR(t, y, p, Lmin)                                      #
  # matrice_cout calcule la matrice coût de fisher pour                         #
  # la segmentation du signal                                                   #
  #   C1(a,b) = sum_{t=a}^{t=b}[log(sigma2)+(xt-mu)^2/sigma2]                   #  
  # avec mu = beta'*r_i : un polynome d'ordre p.                                #  
  # Ici beta se calcule pour chaque couple (a,b).                               #
  #                                                                             #
  # ENTREES:                                                                    #
  #                                                                             #
  #        y : signal de dim(nx1) (pour l'instant cette fonction                #
  #            n'est utilisable que pour des signaux monovariés)                #
  #        t : domaine temporel.                                                #
  #        p : ordre de regression                                              #
  #        Lmin : nbre de points minimum dans un segment (par defaut Lmin = 1)  #
  #                                                                             #
  # SORTIES:                                                                    #
  #                                                                             #
  #        C1 : matrice cout (partition en un seul segment) de dim [nxn]        #
  #                                                                             #
  #                                                                             #
  # Faicel Chamroukhi, 2008                                                     #
  ###############################################################################
  
  
  if (missing(Lmin)) {
    Lmin = 1
  }
  
  eps = .Machine$double.eps
  n = length(y)
  
  X = designmatrix(t,p)
  nl = n - Lmin + 1
  
  
  C1 = matrix(Inf , nrow = n , ncol = n)
  
  #Extract the lower-triangular portion of matrix C1
  
  #########################################################################
  # Aline CANARD                                                          #
  # introduction of a the function "tril" as this does not exist yet in R #
  #########################################################################
  
  C1 = tril(C1, Lmin - 2)
  
  for (a in 0:nl) {
    if ((a + 1 + Lmin) <= n) {
      ############################################################################
      # Condition added to handle the cases (a + 1 + Lmin) > n                   #
      ############################################################################
   
      for (b in (a + 1 + Lmin):n)  {
        # ici et dans ce qui suit a+1 car en matlab les indices commencent de 1
        
        yab = y[(a + 1):b]
        X_ab = X[(a + 1):b ,]
        nk = b - a                  #length(xab)
        beta = svd.inverse(crossprod(X_ab, X_ab)) %*% crossprod(X_ab, yab)
        z = yab - X_ab %*% beta
        sigma2 = crossprod(z, z) / nk
        C1[a + 1,b] = nk + nk * log(sigma2 + eps)      # + (z'*z)/sigma2
        
      }
    }
  }
  
  return(C1)
  
}
