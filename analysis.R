library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)

# colorPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  # "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## load data ----
load("data/nigmatullina.Rda")

NigmatullinaData <-  NigmatullinaData %>%
    select(participant,imagery, direction, cue, direction_num, cue_num,
           response, correct, rt, nystagmus, imagery_score) %>%
    # drop_na() %>%
    mutate(direction = as.factor(direction),
           cue = as.factor(cue),
           imagery = as.factor(imagery)) %>%
    filter(rt < 40)

levels(NigmatullinaData$imagery) <- c("congruent", "neutral", "incongruent")




## -----

NigmatullinaData %>% ggplot(aes(x = rt, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~participant)

NigmatullinaData %>% ggplot(aes(x = nystagmus, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~participant)


RTSummary <- NigmatullinaData %>%
    select(participant, imagery, rt) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarize(mean = mean(rt),
              sd = sd(rt))

VORSummary <- NigmatullinaData %>%
    select(participant, imagery, nystagmus) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarise(mean = mean(nystagmus),
              sd = sd(nystagmus))


ImageryScoreSummary <- NigmatullinaData %>%
    select(participant, imagery, imagery_score) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    # summarise_all(funs(mean,sd, plotrix::std.error))

    summarise(mean = round(mean(imagery_score), 2))

ProportionCorrect <- NigmatullinaData %>%
    select(participant, imagery, correct) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarise(mean = mean(correct),
              sd = sd(correct))



RTSummary %>% ggplot(aes(x = imagery, y = mean, color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))


VORSummary %>% ggplot(aes(x = imagery,
                          y = mean,
                          color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))


ImageryScoreSummary %>% ggplot(aes(x = imagery, y = mean,
                                   color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))






