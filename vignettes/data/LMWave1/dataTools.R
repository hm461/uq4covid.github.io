## load libraries
library(tidyverse)

## function to convert samples to correct format for MetaWards
convertToMetaWards <- function(pars, repeats) {
    disease <- tibble(`beta[2]` = pars$r_zero / pars$infectious_time)
    disease$`beta[3]` <- disease$`beta[2]`
    disease$`progress[1]` <- 1 / pars$incubation_time
    disease$`progress[2]` <- 1
    disease$`progress[3]` <- 1 / (pars$infectious_time - 1)
    disease$`.scale_rate[1]` <- pars$lock_1_restrict
    disease$`.scale_rate[2]` <- pars$lock_2_release
    disease$repeats <- repeats
    disease$par <- pars$par
    disease
}

## function to create fingerprint
createFingerprint <- function(disease) {
    
    ## check input
    stopifnot(is.data.frame(disease))
    
    ## extract repeats
    disease$fingerprint <- select(disease, -repeats, -par) %>%
        # mutate_all(round, digits = 6) %>%
        mutate_all(as.character) %>%
        mutate_all(function(x) {
            if(all(nchar(x) == 1)) {
                x <- paste0(x, ".0")
            }
            gsub("\\.", "i", x)
        }) %>%
        apply(1, paste0, collapse = "v")
    
    ## add replicates
    disease <- mutate(disease, repeats = map(repeats, ~1:.)) %>%
        unnest(cols = repeats) %>%
        mutate(fingerprintRep = paste0(fingerprint, "x", repeats)) %>%
        rename(`repeat` = repeats)
    
    ## return amended data set
    disease
}
