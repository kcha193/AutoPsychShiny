#### Simulating data for app performance testing ####

#### Clear workinng directory
rm(list=ls())  
#### Set working directory
setwd("/Users/user/Desktop/Shiny")      

# 20 conditions: 
# I = 20, 40, 60 80
# N = 50, 500, 10000, 100000, 500000

# Condition 1: I=20, N=50
set.seed(0123)                                                                  # The number you choose here will define your final graph; try various numbers; share numbers
N <- 50                                                                         # N = number of students sitting the test
I <- 20                                                                         # I = Number of question items in the test
ability <- seq(-2, 2,length=N)                                                  # Code generates sequence of student ability from min to max
difficulty <- seq(-2, 2, length=I)                                              # Code generates sequence of item difficulty from min to max* 
expected.perf <- plogis( outer( ability , difficulty , "-" ) )                  # Run mathematical code to generate expected outcomes for each student on each item
resp1 <- 1 * ( expected.perf > matrix( runif( N*I ) , nrow=N , ncol=I ) )       # Generate simulated responses/performance from expected performance and random numbers
colnames(resp1) <- paste("I" , sprintf("%02d", 1:I), sep="")                    # Generate column names from I01 to final item

write.csv(resp1, "Sim.I20.N50.csv", row.names = F)


