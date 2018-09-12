require("igraph")
require("MASS")
require("Matrix")

generate_data <- 
  function(p = 100, n = 100, tau = 0.3,  model = "ER", umin = 0.5, umax = 1){
    
    if (model == "ER"){
      A = as_adjacency_matrix(sample_gnp(p, min(0.05, 5/p)), type = "both", sparse=FALSE)    
    }
    
    if (model == "SF"){
      A = as_adjacency_matrix(sample_pa(p, directed = FALSE), type = "both", sparse=FALSE)    
    }
    
    
    W = matrix(runif(p*p, min = umin, max = umax)*(2*rbinom(p*p, 1, 0.5) - 1), p, p)
    W1 = A*W
    W1 = W1*upper.tri(W1)
    
    
    W2 = W1
    Idx1 = which(W1!=0)
    Idx2 = sample(Idx1, floor(length(Idx1))*tau)
    W2[Idx2] = runif(length(Idx2), min = umin, max = umax)*(2*rbinom(length(Idx2), 1, 0.5) - 1)
    
    W1 = W1 + t(W1)
    W2 = W2 + t(W2)
    
    
    eigen.min = min(eigen(W1)$values,eigen(W2)$values)
    
    W1 = W1 + (abs(eigen.min) + 0.1)*diag(p)
    W2 = W2 + (abs(eigen.min) + 0.1)*diag(p)
    
    
    D1 = diag((diag(W1))^(-0.5))
    R1 = D1%*%W1%*%D1
    D2 = diag((diag(W2))^(-0.5))
    R2 = D2%*%W2%*%D2
    
    
    diag1 = rep(1,p)
    diag2 = rep(1,p)
    diag2[1:floor(0.2*p)] = 4

    Omega1 =  diag(sqrt(diag1))%*%R1%*%diag(sqrt(diag1))
    Omega2 =  diag(sqrt(diag2))%*%R2%*%diag(sqrt(diag2))
    
    
    X1 = mvrnorm(n, rep(0,p), solve(Omega1))
    X2 = mvrnorm(n, rep(0,p), solve(Omega2))
    
    X = rbind(X1, X2)
    group = c(rep(1,n), rep(2,n))
    
    Omega = list()
    Omega[[1]] = Omega1
    Omega[[2]] = Omega2
    
    
    rho = list()
    rho[[1]] = - diag((diag(Omega1))^(-1/2))%*%Omega1%*%diag((diag(Omega1))^(-1/2))
    rho[[2]] = - diag((diag(Omega2))^(-1/2))%*%Omega2%*%diag((diag(Omega2))^(-1/2))
    
    result = list(X = X, group = group, rho = rho, Omega = Omega)
    
    result
  }