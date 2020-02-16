library(tidyverse)
library(assertthat)
 

b_data <- read_csv("bohnanza_cards.csv")
total_cards <- sum(b_data$Frequency)
b_data_tidy <- b_data %>%
  pivot_longer(cols=P1:P4, values_to = "num_cards") %>% 
  mutate(points = gsub(name, pattern="P", replacement=""),
         points = as.numeric(points)) %>% 
  mutate(value_per_card = points/num_cards) %>% 
  filter(is.infinite(value_per_card)==F) #Might need to change this later based on analysis

b_data_tidy %>% 
  ggplot(aes(x=value_per_card, fill=Type)) + 
  #geom_histogram() + 
  geom_density(alpha=0.3) + 
  NULL

b_data_tidy %>% 
  ggplot(aes(x= Frequency, y=value_per_card)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") + 
  NULL

b_data_tidy %>% 
  ggplot(aes(x= points, y=value_per_card)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") + 
  NULL

linmod_frequency <- lm(value_per_card~Frequency, data=b_data_tidy)
summary(linmod_frequency)

linmod_frequency_points <- lm(value_per_card~Frequency + points, data=b_data_tidy)
summary(linmod_frequency_points)



#Chance of achieving level:
cond_prob <- function(frequency=NA, num_cards=NA, total_cards=NA){
  current_prob <- 1
  (frequency-num_cards+1)
  for(i in frequency:(frequency-num_cards+1)){
    new_prob = i / total_cards
    total_cards = total_cards - 1 #Update number of cards in deck. 
    current_prob = current_prob * new_prob
  }
  return(current_prob)
}
assertthat::are_equal(cond_prob(24, 4, total_cards), (24/154 * 23/153 * 22/152 * 21/151))

b_data_tidy$prob_achieving_level <- mapply(cond_prob, b_data_tidy$Frequency, b_data_tidy$num_cards, total_cards)
b_data_tidy <- b_data_tidy %>% 
  mutate(new_term = value_per_card / (1/prob_achieving_level))

b_data_tidy %>% 
  ggplot(aes(x=log(prob_achieving_level), fill=as.factor(points))) + 
  #geom_histogram() + 
  geom_density(alpha=0.3) + 
  NULL


#Main graph to illustrate finding.
b_data_tidy %>% 
  ggplot(aes(x=prob_achieving_level, y=as.factor(points), color=Type)) + 
  geom_point(alpha=0.5) + 
  scale_color_brewer(type="qual", palette = "Paired") + 
  scale_x_log10() + 
  labs(x="Log Probability", y="Points", color="Bean Type") + 
  NULL

#Highlight 'outliers' 
beans_of_interest <- c("Coffee", "Red", "Garden", "Soy")
outlier_labels <- b_data_tidy %>% 
  filter(Type %in% beans_of_interest)
b_data_tidy %>% 
  ggplot(aes(x=prob_achieving_level, y=as.factor(points), color=Type)) + 
  geom_point(alpha=0.5) + 
  scale_color_brewer(type="qual", palette = "Paired") + 
  scale_x_log10() + 
  labs(x="Log Probability", y="Points", color="Bean Type") + 
  geom_label(data=outlier_labels, 
             aes(x=prob_achieving_level, y=as.factor(points), color=Type, label=Type)) + 
  NULL




b_data_tidy %>% 
  ggplot(aes(x=log(prob_achieving_level), y = value_per_card, color=Type)) + 
  geom_point(alpha=0.3) + 
  NULL

