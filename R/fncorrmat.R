# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# ---- the solution to using gather() on a matrix
gather.matrix <- function(mat) {
  if (is.null(dimnames(mat))) {
    grid <- expand.grid(seq.int(nrow(mat)), seq.int(ncol(mat)))
  } else {
    grid <- expand.grid(dimnames(mat))
  }
  cbind(grid, value = as.vector(mat))
}
# fncorrmat function -- rkrajcik 
# parameters:
#    data = data frame
#    returns: gathered correlation matrix as a data frame
fncorrmat <- function(data) {

# make sure variables are all num    
data <-  dplyr::select_if(data, is.numeric)  

# correlation matrix
cormat <- round(cor(data, use = "pairwise.complete.obs"),2)

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)

upper_tri <- get_upper_tri(cormat)

gather_cormat <- gather.matrix(upper_tri)

return(gather_cormat)
}