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

norms <- c(normA$`normA <- ...`, normB$`normA <- ...`, normC$`normA <- ...`, 
           normD$`normA <- ...`, normE$`normA <- ...`, normF$`normA <- ...`, 
           normG$`normA <- ...`, normH$`normA <- ...`)
data$norms <- norms


a <- data[19:24, 11]
normD2 <- 3*(a - min(a))/(max(a)-min(a))
b <- data[25:30, 11]
normE2 <- 3*(b - min(b))/(max(b)-min(b))
c <- data[25:30, 11]
normH2 <- 3*(c - min(c))/(max(c)-min(c))
norms <- c(normA$`normA <- ...`, normB$`normA <- ...`, normC$`normA <- ...`, 
           normD2, normE2, normF$`normA <- ...`, 
           normG$`normA <- ...`, normH2)
data$rands <- c(0)

for (i in 1:48){
  if (data$norms[i] == 3){
    data$rands[i] <- runif(1,-1, 0)
  }
  else if (data$norms[i] == 0){
    data$rands[i] <- runif(1,0, 1)
  }
  else if((data$norms[i] > 1.5) && (data$norms[i] < 3)){
    data$rands[i] <- runif(1, data$norms[i]-3, 3-data$norms[i])
  }
  else if (data$norms[i] < 1.5) {
    data$rands[i] <- runif(1, -data$norms[i], data$norms[i])
  }
}

data$final1 <- data$norms + data$rands
data$final2 <- 3 - data$final1

graph <- read.csv("graph_data.csv")
graph$Expected.Points[40] <- 0
graph$Expected.Points[62] <- 0

library(ggplot2)

ggplot(graph[1:12,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group A predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25)) +
  scale_fill_manual('Team', values = c('coral2', 'steelblue','pink','orange'))


ggplot(graph[1:12,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group A predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25)) +
  scale_fill_manual('Team', values = c('coral2', 'steelblue','pink','orange'))

ggplot(graph[13:24,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group B predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[25:36,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group C predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[37:48,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group D predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[49:60,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group E predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[61:72,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group F predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[73:84,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group G predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))

ggplot(graph[85:96,], aes(fill=Team, y=Expected.Points, x=Match)) +
  geom_bar(Team = 'stack', stat='identity') +
  theme_minimal() +
  labs(x = "Match number", y = 'Expected Points', title= "Group H predictions") +
  theme(text = element_text(size=18, face = 'bold'), plot.title = element_text(hjust=0.5, size=25))







