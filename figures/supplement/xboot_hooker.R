### Residual bootstrap bias correction for subbagged decision trees. 
#
# First some context the following code
#
# 1. Creates a subbagged forrest with ntree trees built on subsamples 
#    of size subsize. 
#
# 2. Obtains out-of-bag residuals for the training data 
#
# 3. Re-runs the proceedure using a response that is obtained by a residual 
#    bootstrap each time. 

# One important detail is in 3:  do we use the same subsamples as in 1? Do we
# use the same number of trees as in 1? 
#
# In the below, the object copyboot controls this trade-off:
#
# If copyboot = k > 0, we replicate each subsample from 1. k times with 
# different bootstrap residuals each time.  
#     Note for k = 1, we just repeat the original forest, but with a different 
#     set of bootstrap residuals for each subsample. For larger k, we effectively
#     average over k bootstrap residuals for each subsample. 
#
# If copyboot = 0, we generate ntree.boot new subsamples. If we don't redefine
# ntree.boot, it is equal to ntree. 

xbootbias = function(X,Y,subsize,ntree=1000,nfold=1,folds=NULL,copyboot=0,ntree.boot=1000){
  
  N = length(Y)
  NN = nfold*floor(N/nfold)
  
  X = X[1:NN,]
  Y = Y[1:NN]
  
  if(is.null(folds)) {
    folds = matrix(sample(1:NN),NN/nfold,nfold)
  } else {
    folds = matrix(folds, ncol=1)
  }
  
  xpredm = rep(NA,length(Y))
  xpredstack = rep(NA,length(Y))
  xpred.bootm = rep(NA,length(Y))
  xpred.cor = rep(NA,length(Y))
  xpred.bootmrf = rep(NA,length(Y))
  xpred.corrf = rep(NA,length(Y))
  
  for(ss in 1:nfold){
    #print(ss)
    
    # Create data for ss'th fold
    x = X[-folds[,ss],]
    predx = X[folds[,ss],]
    y = Y[-folds[,ss]]
    
    npred = nrow(predx)
    nobs = nrow(x)
    
    if(subsize >= nobs){ RR = TRUE }else{ RR = FALSE }
    
    # Define subsamples for primary forest
    subsamps = matrix( 0,subsize,ntree)
    
    # A record for out of bag predictions. oob[i,k] = is sample i excluded in tree k?
    oob = matrix(TRUE,nobs,ntree)
    
    # Set up a matrix to record predictions from each tree for training data (fits)
    # and test data (preds)
    fits = matrix(0, nobs,ntree)    
    preds = matrix( 0, npred,ntree)
    
    # Assign a list to record trees
    trees = list(len=ntree)
    
    # Stacked trees and predictions
    stacktrees = list(len=ntree)
    stackpreds = matrix( 0, npred,ntree)
    
    # Build trees and record predictions
    for(i in 1:ntree){
      # Build tree
      subsamps[,i] = sample.int(nobs,size=subsize,replace=RR)
      #    trees[[i]] = rpart(y~.,data=data[subsamps[,i],],cp=0)
      
      trees[[i]] = randomForest(x[subsamps[,i],],y[subsamps[,i]],ntree=1,replace=FALSE,sampsize=subsize)
      
      # Predict
      preds[,i] = predict(trees[[i]],predx)
      fits[,i] = predict(trees[[i]],x)
      
      # Record out-of-bag-ness
      oob[subsamps[,i],i] = FALSE
    }
    
    # Out of bag predictions
    mfit = ((fits*oob)%*%rep(1,ntree))/(oob%*%rep(1,ntree))
    
    # and residuals
    res = y - mfit
    
    # Now straight up predictions
    
    mfit = fits%*%rep(1/ntree,ntree)
    sres = y - mfit
    # Stacked estimate  -- try to re-estimate residuals
    
    for(i in 1:ntree){
      stacktrees[[i]] = randomForest(x[subsamps[,i],],sres[subsamps[,i]],ntree=1,replace=FALSE,sampsize=subsize)
      
      # Predict
      stackpreds[,i] = predict(stacktrees[[i]],predx)
    }
    
    # Now a bootstrap
    
    if(copyboot>0){   # Replicate previous subsamples copyboot number of times
      sub.boot = matrix(1,1,copyboot)%x%subsamps
      ntree.boot = ntree*copyboot
    } else{   # Create ntree.boot new subsamples
      if(!exists("ntree.boot")){ ntree.boot = ntree } 
      sub.boot = matrix( 0,subsize,ntree.boot)
    }
    
    # Matrix for predictions on bootstrap trees, and vector for bootstrap trees
    pred.boot = matrix(0,npred,ntree.boot)
    pred.bootrf = matrix(0,npred,ntree.boot)
    tree.boot = list(len=ntree.boot)
    
    # Run the bootstrap
    for(i in 1:ntree.boot){
      if(copyboot==0){ sub.boot[,i] = sample.int(nobs,size=subsize,replace=RR) }
      # Create temporary data for the subsample
      tempdat = x[sub.boot[,i],]
      
      # Residual bootstrap -- predictions on subsample plus bootstrap residual
      tempy = mfit[sub.boot[,i],] + sample(res,size=subsize,replace=TRUE)
      
      # Buid trees, record predictions
      #    tree.boot[[i]] = rpart(y~.,data=tempdat,cp=0)
      tree.boot[[i]] = randomForest(tempdat,tempy,ntree=1,replace=FALSE,sampsize=subsize)
      pred.boot[,i] = predict(tree.boot[[i]],predx)
      
      #      rf.boot = randomForest(tempdat,tempy,ntree=ntree,replace=FALSE,sampsize=subsize)
      #      pred.bootrf[,i] = predict(rf.boot,predx)
    }
    
    # Original predictions from forest
    xpredm[folds[,ss]] = preds%*%rep(1/ntree,ntree)
    
    # Predictions on bootstrap forest
    xpred.bootm[folds[,ss]] = pred.boot%*%rep(1/ntree.boot,ntree.boot)
    xpred.bootmrf[folds[,ss]] = pred.bootrf%*%rep(1/ntree.boot,ntree.boot)
    
    # Bias-corrected predictions
    xpred.cor[folds[,ss]] = 2*xpredm[folds[,ss]] - xpred.bootm[folds[,ss]]
    xpred.corrf[folds[,ss]] = 2*xpredm[folds[,ss]] - xpred.bootmrf[folds[,ss]]
    
    # Stacked estimates
    xpredstack[folds[,ss]] = (preds+stackpreds)%*%rep(1/ntree,ntree)
  }
  
  #return( c( var(Y), mean( (Y-xpredm)^2 ), mean( (Y-xpred.cor)^2 ), mean( (Y-xpred.corrf)^2), mean( (Y-xpredstack)^2 ) ) )
  return(list("oos_pred" = xpredm[folds[,1]], "oos_corrected"=xpred.cor[folds[,1]]))
}

