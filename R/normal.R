#'A Normalization tool of the association data
#'
#'This function normalizes the data first
#'@param df 
#'@export
#'@author Abhishek Kapoor
#'@return return the normalized value of the imagery data



normal<- function (df)
  
{
  
  if (is.null(df))
    return(NULL)
  
  rownames (df)<- df[,1]### putting rownames from first column 
  df1<- df[,-1]##removing 1st column
  
  rmean<- apply (df1,1,mean)
  df2<- cbind(df1,rmean)
  
  cmean<- apply (df2,2,mean)
  
  df3<-rbind (df2,cmean) 
  nr<- nrow (df1)
  nc<- ncol(df1)
  
  nr1<- nrow (df3)
  nc1<- ncol(df3)
  
  constant<- df3[nr1,nc1]
  
  matrix1<-matrix (nrow=nr,ncol=nc) 
  matrix2<-matrix (nrow=nr,ncol=nc) 
  
  for (i in 1: nc)
  {
    
    for (j in 1:nr){
      matrix1[j,i] <- (rmean[j] * cmean[i])/constant
      matrix2[j,i] <- df1[j,i]-matrix1[j,i]
    }}
  
  sd1<- sd(matrix2)
  
  matrix3<- matrix2/sd1
  
  df4<- as.data.frame(matrix3)
  colnames(df4) <- colnames(df1)
  rownames (df4)<- rownames (df)
  df5<- as.data.frame(df4)
  
}
