#'A graphical tool with better visualization of association data
#'
#'This function normalizes the data produced by the
#'@param df data frame for brand association
#'@export
#'@return data frame with classification in each column about Positive and Negative 
#'differentation 
#'@author Abhishek Kapoor
#'@details
#'This function return the positive and negative differentated imagery attributes
#'@importFrom SolarMaps normal 
#'@seealso \code{normal}

SolarMaps<- function (data){
  
  df<- normal (data)
  n<- ncol (df)
  for (i in 1:n){
    df[,i] <- ifelse(df[,i] >= 2, "Strongly Differentiated",
                     ifelse(df[,i] >= 1 & df[,i] <2, "Differentiated",
                            ifelse(df[,i] <= -2, "Strongly Negative",
                                   ifelse(df[,i] > -2 & df[,i] < -1, "Negative",
                                          "Neutral"))))
  }
  df1<-df 
}