# Final try at sorting the data hopefully.

library(data.table)
library(R.utils)
library(dplyr)

# fread is a beast.

df <- fread("http://www.secrepo.com/maccdc2012/conn.log.gz")

# Add column names, from the Bro variable name document.
colnames(df) <- c("ts","uid","orig_ip","orig_port","resp_ip","resp_port",
                  "proto","service","duration","orig_bytes","resp_bytes",
                  "conn_state","local_orig", "missed_bytes","history",
                  "orig_pkts","orig_ip_bytes","resp_pkts","resp_ip_bytes",
                  "tunnel_parents")

# Remove the empty columns. Can also remove UID given tunneling data is
# absent.
df <- df[,-c("uid", "local_orig", "tunnel_parents")]

# Set the minimum timestamp to zero and round to 2 decimal places.
df$ts <- round(df$ts - rep(min(df$ts), nrow(df)), digits=2)

# Order the frame by time.
df <- df[order(df$ts),]

# # Add row_identifier, this will be useful when attaching the snort logs.
# row_identifier <- 1:22694356
# df <- cbind(df, row_identifier)
# 
# rm(row_identifier)

# # Add a traffic_type column, full of 'benign' entries for now.
# traffic_type <- rep("benign", 22694356)
# df <- cbind(df, traffic_type)
# 
# rm(traffic_type)

# Check to see if it looks like everything worked!
head(df)

# After some hard fought EDA I found 3 ranges of indexes which
# exhibit qualitatively different behavior.

rows1 <- c((2-1)*22000 + 1, (6+1)*22000)
rows2 <- c((62-1)*22000 + 1, (66+1)*22000)
rows3 <- c((388-1)*22000 + 1, (392+1)*22000)

df1 <- df[rows1[1]:rows1[2],]
df2 <- df[rows2[1]:rows2[2],]
df3 <- df[rows3[1]:rows3[2],]

rm(df, rows1, rows2, rows3)

# We add a column of identifiers to each, this will help with the
# attaching of the Snort data.

row_identifier <- 1:132000
df1 <- cbind(df1, row_identifier)
df2 <- cbind(df2, row_identifier)
df3 <- cbind(df3, row_identifier)
rm(row_identifier)

# Now we get the bastard Snort logs.

# We use the "Fast" logs because the "Full" logs contain
# extraneous information.
# I downloaded, unzipped, and renamed the files, please adapt the path
# and prefix, suffix accordingly.

# Set path here.
path <- "/Users/willnunn/Desktop/FastSnort"
setwd(path)

# My logs are called "FastSnort1.txt", "FastSnort2.txt",... hence:
prefix <- "FastSnort"
suffix <- ".txt"

# We read the first slog into a dataframe.
fast_slogs <- paste(prefix, as.symbol(1), suffix, sep="") %>%
  fread(sep="\t",header=F)

# Read the remaining, remove slog 6 because its empty.
for(i in c(2:5, 7:17)){
  l <- paste(prefix, as.symbol(i), suffix, sep="") %>%
    fread(sep="\t", header=F)
  fast_slogs <- rbind(fast_slogs, l)
}
rm(i,l)
rm(suffix, prefix, path)

# We write a function to derive the time in each log in seconds 
# after the start time.
convert_time <- function(string){
  day <- 86400 * (as.numeric(substr(string, 4, 5)) - 16)
  hour <- 3600 * (as.numeric(substr(string, 7, 8)) - 7)
  minute <- 60 * (as.numeric(substr(string, 10, 11))- 30)
  second <- as.numeric(substr(string, 13, 18))
  return(day + hour + minute + second)
}

# Now add snort column label a column of timestamps and order by ts.
colnames(fast_slogs) <- c("snort")
fast_slogs <- fast_slogs %>% mutate(ts = convert_time(substr(snort, 1, 21)))
fast_slogs <- fast_slogs[order(fast_slogs$ts),]

head(fast_slogs)

# Great we now attain snort logs for each of our three frames.
# By some more ropey EDA, I found that the first Snort of traffic
# appears at most 0.1 seconds after arriving on the Bro log.
# Hence define:
slog1 <- fast_slogs[which(between(fast_slogs$ts, df1[1,1], df1[132000,1]+0.1))]
slog2 <- fast_slogs[which(between(fast_slogs$ts, df2[1,1], df2[132000,1]+0.1))]
slog3 <- fast_slogs[which(between(fast_slogs$ts, df3[1,1], df3[132000,1]+0.1))]
rm(fast_slogs)

# Observe the difference in the size of these slogs.
# ==============================================================

# Now we (try) to attach slog1 to df1.

x <- c()
y <- c()

for(i in 1:nrow(slog1)){
  
  reduced <- df1[which(between(df1$ts, slog1[i,2]-0.1, slog1[i,2])),]
  starter <- nrow(reduced)
  search_start <- nrow(reduced)
  
  while(!grepl(as.character(reduced[search_start,2]), slog1[i,1])|
        !grepl(as.character(reduced[search_start,3]), slog1[i,1])|
        !grepl(as.character(reduced[search_start,4]), slog1[i,1])|
        !grepl(as.character(reduced[search_start,5]), slog1[i,1])){
    search_start <- search_start - 1
    if(search_start==starter - 20 | search_start==0){
      break
    }
  }
  print(c(i, as.integer(reduced[search_start, 18])))
  if(search_start!= starter - 20 & search_start!=0){
    x <- c(x, as.integer(reduced[search_start,18]))
    y <- c(y, sub("].*", "", sub(".*Classification: ",
                                 "", slog1[i,1])))
  }
}
rm(reduced, search_start, starter, i)

out1 <- cbind(x, y)
rm(x,y)
out1

# ==============================================================
# And the second:

x <- c()
y <- c()

for(i in 1:nrow(slog2)){
  
  reduced <- df2[which(between(df2$ts, slog2[i,2]-0.1, slog2[i,2])),]
  starter <- nrow(reduced)
  search_start <- nrow(reduced)
  
  while(!grepl(as.character(reduced[search_start,2]), slog2[i,1])|
        !grepl(as.character(reduced[search_start,3]), slog2[i,1])|
        !grepl(as.character(reduced[search_start,4]), slog2[i,1])|
        !grepl(as.character(reduced[search_start,5]), slog2[i,1])){
    search_start <- search_start - 1
    if(search_start==starter - 20 | search_start==0){
      break
    }
  }
  print(c(i, as.integer(reduced[search_start, 18])))
  if(search_start!= starter - 20 & search_start!=0){
    x <- c(x, as.integer(reduced[search_start,18]))
    y <- c(y, sub("].*", "", sub(".*Classification: ",
                                 "", slog2[i,1])))
  }
}
rm(reduced, search_start, starter, i)

out2 <- cbind(x, y)
rm(x,y)
length(unique(out2[,1]))

# ==============================================================
# And the third:

x <- c()
y <- c()

for(i in 1:nrow(slog3)){
  
  reduced <- df3[which(between(df3$ts, slog3[i,2]-0.1, slog3[i,2])),]
  starter <- nrow(reduced)
  search_start <- nrow(reduced)
  
  while(!grepl(as.character(reduced[search_start,2]), slog3[i,1])|
        !grepl(as.character(reduced[search_start,3]), slog3[i,1])|
        !grepl(as.character(reduced[search_start,4]), slog3[i,1])|
        !grepl(as.character(reduced[search_start,5]), slog3[i,1])){
    search_start <- search_start - 1
    if(search_start==starter - 20 | search_start==0){
      break
    }
  }
  print(c(i, as.integer(reduced[search_start, 18])))
  if(search_start!= starter - 20 & search_start!=0){
    x <- c(x, as.integer(reduced[search_start,18]))
    y <- c(y, sub("].*", "", sub(".*Classification: ",
                                 "", slog3[i,1])))
  }
}
rm(reduced, search_start, starter, i)

out3 <- cbind(x, y)
rm(x,y)
length(unique(out3[,1]))

# Now I share these with Chentian
write.csv(out1, file="out1.csv")
write.csv(out2, file="out2.csv")
write.csv(out3, file="out3.csv")

# Want to attach these to the data frames now!

