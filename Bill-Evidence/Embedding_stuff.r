library(dplyr)
library(data.table)
library(Matrix)
library(irlba)
library(Rtsne)

df <- read.csv("/Users/willnunn/Desktop/Assessment/ProcessedData.csv",
               row.names = 1)
df <- df[,c(1,2,4,18)]

orig_n <- data.frame(1:length(unique(df$orig_ip)), unique(df$orig_ip))
row.names(orig_n) <- unique(df$orig_ip)
head(orig_n)

resp_n <- data.frame(1:length(unique(df$resp_ip)), unique(df$resp_ip))
row.names(resp_n) <- unique(df$resp_ip)
head(resp_n)

mal_orig <- c()
l <- unique(df[which(df$conn_type == 1), 2])
for(i in 1:nrow(orig_n)){
  if(orig_n[i,2] %in% l){
    mal_orig <- c(mal_orig, "red")
  }
  else{
    mal_orig <- c(mal_orig, "black")
  }
}
rm(l)

mal_resp <- c()
l <- unique(df[which(df$conn_type == 1), 3])
for(i in 1:nrow(resp_n)){
  if(resp_n[i,2] %in% l){
    mal_resp <- c(mal_resp, "red")
  }
  else{
    mal_resp <- c(mal_resp, "black")
  }
}
rm(l)

df <- mutate(df, e1 = orig_n[orig_ip, 1])
df <- mutate(df, e2 = resp_n[resp_ip, 1])
head(df)

M = sparseMatrix(i = df$e1, j = df$e2)

d = 10
SVD = irlba(M, nv=d)

Y = SVD$v %*% diag(sqrt(SVD$d))

X = SVD$u %*% diag(sqrt(SVD$d))



set.seed(31)
tsne_out_Y <- Rtsne(Y, check_duplicates = F)
plot(tsne_out_Y$Y, pch=16, cex=.3, xaxt='n', yaxt='n', ann=F,
     col = mal_resp)


set.seed(31)
tsne_out_X <- Rtsne(X, check_duplicates = F)
plot(tsne_out_X$Y, pch=16, cex=.3, xaxt='n', yaxt='n', ann=F,
     col = mal_orig)



# We know that 






