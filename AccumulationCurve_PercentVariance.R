library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
###
### Everything with two #s is where you must edit it personally (only two lines, 10 and 46.)
### Plot on line 52 if you so wish to edit it.
###

##Replace your file's location below. Make sure it is set-up like the example.
url <- "~/Desktop/SquareCounting.csv"

#Reading in and formatting data.
dat <- read.csv(url, header = TRUE)
dat2 <- melt(dat)

#Formula
avg <- mean(dat2$value)
se <- function(x) 100-abs((x-avg)/avg*100)
n <- nrow(dat)

#Where the magic happens, takes in the data and our number of repetitions and simulates n number of times with replacement.
acc_curve <- function(x, reps) {
  
  variance_dat <- matrix(, nrow = reps, ncol=n)
  print(variance_dat)
  sd <- data.frame(NA_col = rep(NA, n))
  print(sd)
  
  for(i in 1:reps) {
    for(i in 1:nrow(x)) {
      temp_dat <- sample(dat2[,2], n*(i+1), replace = TRUE)
      temp_matrix <- matrix(temp_dat, nrow = i+1, ncol = n, byrow = TRUE)
      temp2 <- colMeans(temp_matrix)
      borp <- se(temp2)
      sd[,i] <- borp
      colnames(sd)[i] <- paste0("trials_to_", i+1)
    }

    variance_dat <- rbind(variance_dat, t(as.data.frame(colMeans(sd))))
  }
  
  print(variance_dat)
}

##Edit this here to how many repetitions you want
res <-acc_curve(dat, 10000)
res <- na.omit(res)
res <- melt(res)

#Plot -- can change the y-intercept to a higher or lower value as needed.
my_plot <- ggplot(data = res, aes(x=Var2, y=value)) + geom_boxplot(aes(fill=Var2)) + geom_hline(yintercept = 95, linetype="dashed")
my_plot
