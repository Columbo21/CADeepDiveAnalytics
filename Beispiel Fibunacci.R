
fibonacci <- function(x){
  
  
  
  ans <- rep(1,x)
  if (x > 2){
    for (k in 3:x){
      ans[k]<-ans[k-2]+ans[k-1]
    }
  }
  return(c(0,ans)[1:x])
  
}

fibonacci (5)

''' x <- 5
 vector <- NA
while (length(vector)<x+1){
a<-vector[length(vector)]
b<-vector[length(vector)-1]
vector = append(vector, a+b)

}'''


''' while (length(vector)<x+1){
    a<-vector[length(vector)]
b<-vector[length(vector)-1]
vector = append(vector, a+b)

}}

for (i < x+1){


}


return vector'''