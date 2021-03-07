library(babynames)
library(dplyr)
library(magrittr)
library(tm)

buildNames <- function(lower, upper) {
    # Creates a lexicon (data frame) with two variables: name and total number of times 
    # baby name used in the period between lower year and upper year
    
    babynames %>% filter(year>(lower-1) & year<(upper+1)) %>% 
       select(name,n) %>% group_by(name) %>% mutate(total=sum(n)) %>% 
       arrange(desc(n)) %>% distinct(name, total) -> df
    df$lcname <- tolower(df$name)
    return(df)
}


detectNames <- function(name) {

}

removeNames <- function(text, namelist) {
    # Given a vector, this function scrubs all elements to remove proper names
    # NOT TESTED - MAY HAVE TERRIBLE ISSUES - TRANSFERRING FROM MOLESKIN (yeah, I know)
    
    # You can test against 4000 names at a time due to regex limitation so have
    # to split into serial or parallel tasks. Serial:
    gnum  <- floor(nrow(namelist)/4000)
    start <- seq(1, nrow(namelist), 4000)
    end   <- seq(1, nrow(namelist), 4000) + 4000
    end[gnum+1] <- nrow(namelist)
    
    for (i in 1:(gnum+1)) {
       start_at <- start[i]; end_at <- end[i]
       # go through each block of 4000 names & scrub over and over
       x[i] <- tm::removeWords(x[i-1], namelist$lcname[start[i]:end[i]])
    }
    
    # Parallel is potential enhancement: see
    # https://towardsdatascience.com/getting-started-with-parallel-programming-in-r-d5f801d43745 or 
    # https://tmieno2.github.io/R-as-GIS-for-Economists/parcomp.html 
    
    results <- df %>% mutate(noname=tm::removeWords(var, namelist$name))
    return(results)
}
