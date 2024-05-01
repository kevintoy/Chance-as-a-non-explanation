#now load "relevant_data"
relevant_data<-read.csv("~/lack of chance explanation/relevant_data.csv",fileEncoding="latin1")

relevant_data$topic <- factor(relevant_data$topic, levels = names(sort(table(relevant_data$topic), decreasing = TRUE)))
library(ggplot2)
ggplot(relevant_data, aes(x = topic)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "", x = "Topic", y = "Count") +
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr) 
filtered_data <- relevant_data %>% 
  filter(affected.by != "")
filtered_data$affected.by <- factor(filtered_data$affected.by, levels = names(sort(table(filtered_data$affected.by), decreasing = TRUE)))
library(ggplot2)
ggplot(filtered_data, aes(x = affected.by)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "", x = "entities influencing the outcome of uncertain events", y = "Count") +
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(psych)
relevant_data$predictability <- as.factor(relevant_data$predictability)
relevant_data$predictability_original <- as.factor(relevant_data$predictability_original)
relevant_data$controllable <- as.factor(relevant_data$controllable)
relevant_data$controllable_original <- as.factor(relevant_data$controllable_original)
relevant_data$intrinsic.randomness <- as.factor(relevant_data$intrinsic.randomness)
relevant_data$intrinsic.randomness_original <- as.factor(relevant_data$intrinsic.randomness_original)

# Compute Cohen's Kappa
kappa_result_pred <- cohen.kappa(matrix(c(relevant_data$predictability, relevant_data$predictability_original), ncol = 2))
kappa_result_control <- cohen.kappa(matrix(c(relevant_data$controllable, relevant_data$controllable_original), ncol = 2))
kappa_result_intrinsic <- cohen.kappa(matrix(c(relevant_data$intrinsic.randomness, relevant_data$intrinsic.randomness_original), ncol = 2))
# Print the result
print(kappa_result_pred)
print(kappa_result_control)
print(kappa_result_intrinsic)


library(tidyr)
library(forcats)
long_data <- relevant_data %>%
  pivot_longer(cols = c("predictability_final", "controllable_final", "intrinsic.randomness_final"),
               names_to = "variable", values_to = "presence") %>%
  mutate(variable = factor(variable,
                           levels = c("predictability_final", "controllable_final", "intrinsic.randomness_final"),
                           labels = c("Predictability", "Controllability", "Intrinsic randomness")))

# Plot
ggplot(long_data, aes(x = variable, y = ..prop.., group = presence, fill = presence)) +
  geom_bar(stat = "count", position = "fill") +  # Using 'fill' to ensure the bar stacks to 100%
  scale_y_continuous("Percentage", labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "", y = "Percentage", 
       subtitle = "") +
  theme_minimal() +
  theme(axis.text.x = element_text()) +
  scale_fill_brewer(palette = "Set1")  # Use a nice color palette
