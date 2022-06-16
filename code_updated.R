data <- read.csv("groupdata.csv")

exp_pts <- ((data$Omega/data$R)^2)*data$P*(data$R/data$R_0)*(data$Omega/data$Omega_0)*(data$D-data$D_0)

data$exp_pts <- exp_pts

library(tidyverse)

normA <- data %>% 
  filter(Groups == "Group A") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normB <- data %>% 
  filter(Groups == "Group B") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normC <- data %>% 
  filter(Groups == "Group C") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normD <- data %>% 
  filter(Groups == "Group D") %>% 
  summarise(minval = min(exp_pts),
    normA <- 3*(exp_pts - minval)/(max(exp_pts)-minval))

normE <- data %>% 
  filter(Groups == "Group E") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normF <- data %>% 
  filter(Groups == "Group F") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normG <- data %>% 
  filter(Groups == "Group G") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))

normH <- data %>% 
  filter(Groups == "Group H") %>% 
  summarise(normA <- 3*(exp_pts - min(exp_pts))/(max(exp_pts)-min(exp_pts)))


#since we do not get the expected values for groups D, E, and H from the code above, we weangle the data in a different manner
a <- data[19:24, 11]
normD2 <- 3*(a - min(a))/(max(a)-min(a))
b <- data[25:30, 11]
normE2 <- 3*(b - min(b))/(max(b)-min(b))
c <- data[25:30, 11]
normH2 <- 3*(c - min(c))/(max(c)-min(c))
norms <- c(normA$`normA <- ...`, normB$`normA <- ...`, normC$`normA <- ...`, 
           normD2, normE2, normF$`normA <- ...`, 
           normG$`normA <- ...`, normH2)

data$norms <- norms

#function to run the simulations for producing different random values at every iteration 
simulation_func <- function(norm_val){
  rand_vals <- c(0)
  for (j in 1:10000){
    if (norm_val == 3){
      rand_vals[j] <- runif(1,-1,0)
    }
    else if (norm_val == 0){
      rand_vals[j] <- runif(1,0,1)
    }
    else if ((norm_val > 1.5) && (norm_val < 3)){
      rand_vals[j] <- runif(1, norm_val - 3, 3 - norm_val)
    }
    else if (norm_val < 1.5){
      rand_vals[j] <- runif(1, -norm_val, norm_val)
    }
  }
  mean_val <- mean(rand_vals)
  mean_val
}

data$rands <- c(0)
#loop to calculate the mean random values after running 10,000 simulations
for (i in 1:48){
  data$rands[i] <- simulation_func(norm_val = data$norms[i])
}

data$final1 <- data$norms + data$rands
data$final2 <- 3 - data$final1


# Visualizations

graph <- read.csv("graph_data.csv")

#coding in the updated data points for expected points since the previous dataset was mnaually filled in 
vals <- c()
for (i in 1:48){
  x <- c(data$final1[i], data$final2[i])
  vals <- append(vals, x)
}
graph$Expected.Points <- vals
graph$Expected.Points[40] <- 0
graph$Expected.Points[62] <- 0

library(ggplot2)

ggplot(graph[1:12,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group A predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25)) +
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[13:24,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group B predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[25:36,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group C predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[37:48,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group D predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[49:60,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group E predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[61:72,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group F predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[73:84,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group G predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))

ggplot(graph[85:96,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group H predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))+
  scale_fill_manual('Team', values = c('pink', 'steelblue','coral2','orange'))




# data2 <- read.csv("groupdata2.csv")
# ggplot(graph[1:12,], aes(fill=Team, y=Expected.Points, x=Match)) +
#   geom_bar(Team = 'stack', stat='identity') +
#   theme_minimal() +
#   labs(x = "Match number", y = 'Expected Points', title= "Group A predictions") +
#   theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25)) +
#   scale_fill_manual('Team', values = c('coral2', 'steelblue','pink','orange'))

# 
# for (i in 1:48){
#   if (data$norms[i] == 3){
#     data$rands[i] <- runif(1,-1, 0)
#   }
#   else if (data$norms[i] == 0){
#     data$rands[i] <- runif(1,0, 1)
#   }
#   else if((data$norms[i] > 1.5) && (data$norms[i] < 3)){
#     data$rands[i] <- runif(1, data$norms[i]-3, 3-data$norms[i])
#   }
#   else if (data$norms[i] < 1.5) {
#     data$rands[i] <- runif(1, -data$norms[i], data$norms[i])
#   }
# }
