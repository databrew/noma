library(dplyr)
library(readr)
library(ggplot2)

# My ask is to create a timeline of symptoms. You will see that for every symptom (Pain, Fever, Anorexia, Apathy) there is an "SYMPTOM_since_ref", this is the number of days before/after the reference diagnostic which is day 0, sometimes its a negative value e.g. -40, some times its positive e.g. 40. I envision the timeline as the example attached, mean and range for each symptom and the size of the dot to represent the number of datapoints used.


# read in the data
noma <- read_csv('data/Noma database Paola.csv')

# lower case all column names
colnames(noma) <- tolower(colnames(noma))

# get ref columns
noma <- noma[,grepl('_since_ref', names(noma))]

# clean
for(j in 1:ncol(noma)){
  input <- as.character(unlist(noma[,j]))
  out <- trimws(input, which = 'both')
  out <- gsub("[^0-9.-]", "", out)
  out <- gsub("\\(|\\)", "", out)
  out <- as.numeric(out)
  out <- ifelse(out < -1000, NA, 
                ifelse(out > 1000, NA, out))
  noma[,j] <- out
}

# outputs
if(!dir.exists('outputs')){
  dir.create('outputs')
}

# Get average for each column
average <- function(x){mean(x, na.rm = TRUE)}
output_averages <- noma %>%
  summarise_all(average)
output_averages
write_csv(output_averages, 'outputs/averages.csv')

# Get median for each column
get_median <- function(x){median(x, na.rm = TRUE)}
output_medians <- noma %>%
  summarise_all(get_median)
output_medians
write_csv(output_medians, 'outputs/medians.csv')

# Visualization with variance
long_data <- 
  tidyr::gather(noma, key, value, pain_since_ref:death_since_ref) %>%
  filter(!is.na(value)) %>%
  mutate(key = gsub('_since_ref', '', key)) %>%
  mutate(key = gsub('_', ' ', key))
write_csv(long_data, 'outputs/long_data.csv')

ggplot(long_data,
       aes(x = value,
           y = 1)) +
  geom_jitter(aes(color = key))

# Visualization with only means
long_data_averages <- output_averages %>%
  tidyr::gather(key, value, pain_since_ref:death_since_ref) %>%
  mutate(key = gsub('_since_ref', '', key)) %>%
  mutate(key = gsub('_', ' ', key))
  
ggplot(long_data,
       aes(x = value,
           y = 1)) +
  geom_point(aes(color = key)) +
  theme_bw() +
  scale_y_continuous(breaks = NULL) +
  labs(x = 'Day', y = '') +
  theme(legend.position = 'bottom') +
  scale_colour_discrete(name = '')

# Range plot
pd <- long_data %>%
  group_by(key) %>%
  summarise(avg = mean(value),
            minimum = min(value),
            maximum = max(value))

ggplot(data = pd,
       aes(x = avg,
           y = key)) +
  geom_point() +
  geom_errorbar(aes(
    xmin = minimum,
    xmax = maximum)) +
  theme_bw() +
  labs(x = 'Days',
       y = 'Symptom')
