library(dplyr)
library(readr)
library(readxl)
library(stringr)


import_files <- function() {
    files <- list.files(path = str_c("data/"),
                        # str_c() is equivalent to paste0(),
                        pattern = "^G.*.xlsx",
                        full.names = TRUE)

    read_xlsx_filename <- function(filename) {
        out <- read_excel(filename, sheet = 1)
        # FIXME: add subject variable
        out$source <- str_split(basename(filename), "\\.")[[1]][1]
        out$id <- str_sub(basename(filename), start = 3, end = 4)
        # out$condition <- str_sub(basename(filename), start = 6, end = 8)
        out
    }

    df <- files %>%
        lapply(., read_xlsx_filename) %>%
        bind_rows() %>% # equivalent to: do.call(rbind, .)
        tbl_df()

    df <- df %>% rename(trial_num = `Trial No`, direction = Direction,
                   cue = `Cue Direction`, response = `Button Pressed`,
                   rt = `Button time (ms)`, threshold = Threshold,
                   imagery_score = Imagery, nystagmus = `Nystagmus onset`,
                   vel_post = `8-10s post rotation nystagums velocity`,
                   vel_pre = `2s pre-button press nystagmus velocity`)

    df$imagery_score[df$imagery_score == "N"] <- NA
    df$imagery_score <- as.numeric(df$imagery_score)

    df <- df %>% mutate(
        rt = rt/1000, # convert to seconds
        nystagmus = nystagmus/1000,
        # response coded as 1 (left) and 0 (right)
        response = ifelse(response == "L", 1, 0),
        direction_num = ifelse(direction == "L", 1, -1), # convert to 1 (right) or -1 (left),
        cue_num = ifelse(cue == "L", 1, ifelse(cue == "R", -1, 0)), # convert to 1 (right), -1 (left) or 0 (none)
        imagery = case_when(cue_num == 0 ~ "neutral",
                               direction_num == cue_num ~ "congruent",
                               direction_num != cue_num ~ "incongruent"),
        participant = factor(id),
        correct = case_when(response == 1 & direction == "L" ~ 1,
                            response == 0 & direction == "R" ~ 1,
                            response == 0 & direction == "L" ~ 0,
                            response == 1 & direction == "R" ~ 0),
        id = match(participant, unique(participant)),
        logrt = log(rt),
        source = factor(source))
    df
}


NigmatullinaData <- import_files()
write_csv(x = NigmatullinaData, path = "data/nigmatullina.csv", append = FALSE)
save(NigmatullinaData, file = "data/nigmatullina.Rda")
