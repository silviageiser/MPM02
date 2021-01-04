#ABC Code von mir eingef?gt. Falls Fehlermeldung:D
rm(list = ls(all = TRUE))
#install.packages("abc")
library(abc)

sum_stat <- read.csv(file='summary_stat.csv', header=FALSE)
head(sum_stat)

# import simulation and observed data
obs_data <- c(3,2,1143,1655)
sim_param <- sum_stat[,2:3]
sim_data <- sum_stat[,4:7]

# Run ABC
res <- abc(target=obs_data,
           param=sim_param,
           sumstat=sim_data,
           tol=0.005,
           transf=c("log"),
           method="neuralnet")

plot(res, param=sim_param)
write.table(res$adj.values,"out_abc.csv", sep=",", row.name=FALSE)