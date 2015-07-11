movie_data <- read.csv(file="imdb25009.csv")
  
user_independent <- (movie_data[,!(names(movie_data) %in% c("user"))])

cosineSim <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

holder <- matrix(NA, nrow=ncol(user_independent),ncol=ncol(user_independent),dimnames=list(colnames(user_independent),colnames(user_independent)))
similar_movies <- as.data.frame(holder)

for(i in 1:ncol(user_independent)) {
  for(j in 1:ncol(user_independent)) {
    similar_movies[i,j]= cosineSim(user_independent[i],user_independent[j])
  }
}

write.csv(similar_movies,file="similar_movies.csv")

neighbouring_movies <- matrix(NA, nrow=ncol(similar_movies),ncol=11,dimnames=list(colnames(similar_movies)))

for(i in 1:ncol(user_independent)) 
{
  neighbouring_movies[i,] <- (t(head(n=11,rownames(similar_movies[order(similar_movies[,i],decreasing=TRUE),][i]))))
}

write.csv(file="neighbouring_movies.csv",x=neighbouring_movies[,-1])


prob <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

holder <- matrix(NA, nrow=nrow(movie_data),ncol=ncol(movie_data)-1,dimnames=list((movie_data$user),colnames(movie_data[-1])))

for(i in 1:nrow(holder)) 
{
  for(j in 1:ncol(holder)) 
  {
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    if(as.integer(movie_data[movie_data$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      top<-((head(n=11,(similar_movies[order(similar_movies[,product],decreasing=TRUE),][product]))))
      top_names <- as.character(rownames(top))
      top_similarities <- as.numeric(top[,1])
      
      top_similarities<-top_similarities[-1]
      top_names<-top_names[-1]
      
      top_purchases<- movie_data[,c("user",top_names)]
      top_userPurchases<-top_purchases[top_purchases$user==user,]
      top_userPurchases <- as.numeric(top_userPurchases[!(names(top_userPurchases) %in% c("user"))])
      
      holder[i,j]<-prob(similarities=top_similarities,history=top_userPurchases)
      
    }
  }   

}


final_probs <- holder
write.csv(file="final_probs.csv",final_probs)

final_probs_holder <- matrix(NA, nrow=nrow(final_probs),ncol=5,dimnames=list(rownames(final_probs)))
for(i in 1:nrow(final_probs)) 
{
  final_probs_holder[i,] <- names(head(n=5,(final_probs[,order(final_probs[i,],decreasing=TRUE)])[i,]))
}

write.csv(file="final_recommendations.csv",final_probs_holder)

