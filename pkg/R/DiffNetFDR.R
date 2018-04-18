
lasso.estimator <- function(X, lam1 = seq(40,1)/20, parallel = F, nCpus = 4){

  # compute the numbers of samples and variables
  n = dim(X)[1]
  p = dim(X)[2]

  centered.X = scale(X, scale = FALSE)
  Sigma = cov(centered.X)
  DSigma = diag(Sigma)

  lam2 = sqrt(DSigma*log(p)/n)
  beta = array(0, dim=c(p,p,length(lam1)))
  res = array(0,dim=c(n,p,length(lam1)))

  wrapper = function(i){
    fit=glmnet(centered.X[,-i], centered.X[,i], lambda= lam1*lam2[i])
    fit$beta=as.matrix(fit$beta)
    if(ncol(fit$beta)<length(lam1)){
      tmp = matrix(0,nrow = nrow(fit$beta),ncol = length(lam1))
      tmp[,1:ncol(fit$beta)]=fit$beta
      tmp[,ncol(fit$beta):length(lam1)] = fit$beta[,ncol(fit$beta)]
      fit$beta = tmp
    }

    if(i==1){
      beta[2:p,i,]=fit$beta
    }else if(i==p){
      beta[1:(p-1),i,]=fit$beta
    }else{
      beta[1:(i-1),i,]=fit$beta[1:(i-1),]
      beta[(i+1):p,i,]=fit$beta[i:nrow(fit$beta),]
    }
    res[,i,] = matrix(rep(centered.X[,i],length(lam1)),ncol = length(lam1)) - centered.X[,-i]%*%fit$beta
    out = list(beta = beta[,i,], res = res[,i,])
    return(out)
  }


  if(parallel){
    fit =mclapply(1:p, wrapper, mc.cores=nCpus)
  }else{
    fit =lapply(1:p, wrapper)
  }


  for(i in 1:p){
    beta[,i,]=fit[[i]]$beta
    res[,i,]=fit[[i]]$res
  }


  r.tilde = array(0, dim=c(p,p,length(lam1)))
  r.hat = array(0, dim=c(p,p,length(lam1)))

  for(k in seq(length(lam1))){
    r.tilde[,,k] = cov(res[,,k])*(n-1)/(n)
    r.hat[,,k] =r.tilde[,,k] + diag(diag(r.tilde[,,k]))%*%beta[,,k] + t(beta[,,k])%*%diag(diag(r.tilde[,,k]))
  }

  out = list(beta = beta, res = res, r.tilde = r.tilde, r.hat = r.hat)
  return(out)
}



DiffNet.FDR = function(X, group, alpha = 0.2, test.type = "pcor", parallel = F, nCpus = 4){

  try(if (test.type %in% c("pcor","pmat") == FALSE) stop("The test type you provide is not include in this package."))


  X1 = X[group==unique(group)[1],]
  X2 = X[group==unique(group)[2],]



  if(test.type == "pcor"){
    out =  DiffPCorr.test(X1, X2, alpha = alpha, parallel = parallel, nCpus = nCpus)
  }

  if(test.type == "pmat"){
    out =  DiffPMat.test(X1, X2, alpha = alpha, parallel = parallel, nCpus = nCpus)
  }

  return(out)
}



DiffPCorr.test = function(X, Y, alpha = 0.2, parallel = F, nCpus = 4){

  n1 = dim(X)[1]
  n2 = dim(Y)[1]
  p = dim(X)[2]

  fit1 = lasso.estimator(X, lam1 = seq(40,1)/20, parallel, nCpus)
  fit2 = lasso.estimator(Y, lam1 = seq(40,1)/20, parallel, nCpus)

  WW = array(0, dim=c(p,p,40))
  score = rep(0,40)
  for(k in seq(40)){
    Dr1 = diag((diag(fit1$r.tilde[,,k]))^(-0.5))
    Dr2 = diag((diag(fit2$r.tilde[,,k]))^(-0.5))
    T1 = Dr1%*%fit1$r.hat[,,k]%*%Dr1
    T2 = Dr2%*%fit2$r.hat[,,k]%*%Dr2
    rho1 = T1*((abs(T1)>=2*sqrt(log(p)/n1))*1)
    rho2 = T2*((abs(T2)>=2*sqrt(log(p)/n2))*1)
    WW[,,k] = (T1-T2)/sqrt((((1-rho1^2)^2)/n1)+(((1-rho2^2)^2)/n2))
    diag(WW[,,k] ) = 0
    WW[,,k] = WW[,,k]*upper.tri(WW[,,k])
    for (l in seq(1,10)){
      score[k] = score[k] + (sum(abs(WW[,,k])>qnorm(1-l*(1-pnorm(sqrt(log(p))))/10))/(l*(p^2-p)*(1-pnorm(sqrt(log(p))))/10)-1)^2
    }
  }

  k.min = which(score==min(score))
  W = WW[,,k.min]
  diag(W) = 0

  M = 2000
  tt = (seq(M)/M)*2*sqrt(log(p))
  fdp = rep(NA,M)
  for (m in seq(M)){
    fdp[m] = (2-2*pnorm(tt[m]))*((p^2-p)/2)/max(sum(abs(W)>=tt[m]),1)
  }

  id.min = min(which(fdp<=alpha))
  t.hat = tt[id.min]
  W = W*upper.tri(W) + t(W*upper.tri(W))
  Diff.edge = (abs(W)>=t.hat)*1

  row.names(Diff.edge) = colnames(X)
  colnames(Diff.edge) = colnames(X)
  Degree.Diff = apply(Diff.edge, 1, sum)
  Diff.graph.connected = Diff.edge[Degree.Diff>0, Degree.Diff>0]
  Diff.net.connected  = graph_from_adjacency_matrix(Diff.graph.connected, mode = "undirected", weighted = TRUE, diag = FALSE)
  Diff.net = graph_from_adjacency_matrix(Diff.edge, mode = "undirected", weighted = TRUE, diag = FALSE)

  out = list(Diff.edge = Diff.edge, W = W, t = t.hat, Diff.net = Diff.net, Diff.net.connected = Diff.net.connected)

  return(out)
}




DiffPMat.test = function(X, Y, alpha = 0.2, parallel = F, nCpus = 4){

  n1 = dim(X)[1]
  n2 = dim(Y)[1]
  p = dim(X)[2]

  fit1 = lasso.estimator(X, lam1 = seq(40,1)/20, parallel, nCpus)
  fit2 = lasso.estimator(Y, lam1 = seq(40,1)/20, parallel, nCpus)

  score = rep(0,40)
  WW = array(0,dim=c(p, p, 40))
  for(k in seq(40)){

    D1=diag((diag(fit1$r.hat[,,k]))^(-1))
    D2=diag((diag(fit2$r.hat[,,k]))^(-1))
    T1 = D1%*%fit1$r.hat[,,k]%*%D1
    T2 = D2%*%fit2$r.hat[,,k]%*%D2
    Theta1 = (1+diag(diag(fit1$r.hat[,,k]))%*%(fit1$beta[,,k]^2)%*%D1)/(n1*diag(fit1$r.hat[,,k])%*%t(diag(fit1$r.hat[,,k])))
    Theta2 = (1+diag(diag(fit2$r.hat[,,k]))%*%(fit2$beta[,,k]^2)%*%D2)/(n2*diag(fit2$r.hat[,,k])%*%t(diag(fit2$r.hat[,,k])))

    WW[,,k] = (T1-T2)/((Theta1+Theta2)^(1/2))
    diag(WW[,,k]) = 0
    WW[,,k] = WW[,,k]*upper.tri(WW[,,k])

    for (l in seq(1,10)){
      score[k] = score[k] + (sum(abs(WW[,,k])>qnorm(1-l*(1-pnorm(sqrt(log(p))))/10))/(l*(p^2-p)*(1-pnorm(sqrt(log(p))))/10)-1)^2
    }

  }


  k.min = which(score==min(score))
  W = WW[,,k.min]
  diag(W) = 0

  M = 2000
  tt = (seq(M)/M)*2*sqrt(log(p))
  fdp = rep(NA, M)
  for (m in seq(M)){
    fdp[m] = (2-2*pnorm(tt[m]))*((p^2-p)/2)/max(sum(abs(W)>=tt[m]),1)
  }

  id.min = min(which(fdp<=alpha))
  t.hat = tt[id.min]
  W = W*upper.tri(W) + t(W*upper.tri(W))
  Diff.edge = (abs(W)>=t.hat)*1

  row.names(Diff.edge) = colnames(X)
  colnames(Diff.edge) = colnames(X)
  Degree.Diff = apply(Diff.edge, 1, sum)
  Diff.graph.connected = Diff.edge[Degree.Diff>0, Degree.Diff>0]
  Diff.net.connected  = graph_from_adjacency_matrix(Diff.graph.connected, mode = "undirected", weighted = TRUE, diag = FALSE)
  Diff.net = graph_from_adjacency_matrix(Diff.edge, mode = "undirected", weighted = TRUE, diag = FALSE)

  out = list(Diff.edge = Diff.edge, W = W, t = t.hat, Diff.net = Diff.net, Diff.net.connected = Diff.net.connected)
  return(out)
}

