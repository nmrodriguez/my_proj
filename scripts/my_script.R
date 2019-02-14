
# change working directory
setwd("/Users/casillas/Desktop/my_proj")

# Combine files vertically into large data frame
temp <- list.files(path = "./data", full.names = TRUE, pattern = ".csv")
myfiles = lapply(temp, read.csv, sep = ",")
df <- do.call("rbind", myfiles)

library(tidyverse)

glimpse(df)

# remove practice trials
df <- slice(df, -c(1:4))

df <- select(df, participant, stimuli, key_resp_trial.keys, key_resp_trial.rt)

df_clean <- separate(df, col = stimuli, into = c("junk", "stim"), sep = 4) %>% 
  separate(., col = stim, into = c("stim_num", "junk2"), sep = ".wav") %>% 
  select(., -junk, -junk2) %>% 
  rename(., response = key_resp_trial.keys, rt = key_resp_trial.rt) %>% 
  na.omit(.) %>% 
  mutate(., resp = as.numeric(response) - 2, 
            stim_num = as.numeric(stim_num))

df_clean %>% 
  filter(., participant != "jvc" & participant != "jjgp") %>% 
  ggplot(., aes(x = stim_num, y = resp)) + 
    geom_jitter(width = 0.2, height = 0, alpha = 0.1) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T)

str(df_clean)
