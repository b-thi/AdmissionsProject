#############################################
#############################################
##### Barinder Thind/Richard Groenewald #####
#####         Admissions Project        #####
#############################################
#####           Scraping Code           #####
#############################################
#############################################

### Loading libraries
library(rvest)
library(tidyverse)
library(stringi)

### Function for Schools
# scrapes and returns data frame for a given school input
admissions_scrape <- function(school){
  
  # List of schools
  schools <- c("stanford", "harvard", "duke", "chicago", "ucla")
  
  # Checking if school is in the system
  if (school %in% schools) {
    
    # Initializing df
    school_df_test <- data.frame()
    
    # Getting number of pages
    url <- url(paste0("https://www.thegradcafe.com/survey/index.php?q=", 
                      school, "+statistics&t=a&o=&pp=250&o=&p=", "1"))
    value <- url %>% 
      read_html() %>% 
      html_nodes(xpath = "/html/body/main/form/div[2]/div/text()[2]") %>% 
      html_text()
    
    pageNum <- seq(1, as.numeric(stri_extract_first_regex(value, "[0-9]+")), 1)

    # Looping through pages
    for (i in 1:length(pageNum)) {
      # URL for school
      url <- url(paste0("https://www.thegradcafe.com/survey/index.php?q=", 
                        school, "+statistics&t=a&o=&pp=250&o=&p=", pageNum[i]))
      
      # Reading HTML
      table = url %>% read_html() %>% html_nodes(xpath = "/html/body/main/section[2]/div/table")
      
      # Turning into data frame
      school_df = data.frame(html_table(table))
      
      # Changing column names
      colnames(school_df) <- c("School", "Program", "Decision_Raw", "StudentState",
                               "Date", "Notes")
      
      # Figuring out admissions results
      school_df$Decision[which(stri_detect_fixed(school_df$Decision_Raw, "Accepted") == TRUE)] <- "Accepted" 
      school_df$Decision[which(stri_detect_fixed(school_df$Decision_Raw, "Rejected") == TRUE)] <- "Rejected" 
      school_df$Decision[which(stri_detect_fixed(school_df$Decision_Raw, c("wait listed")) == TRUE)] <- "Waitlist"
      school_df$Decision[which(stri_detect_fixed(school_df$Decision_Raw, c("Wait listed")) == TRUE)] <- "Waitlist"
      school_df$Decision[which(stri_detect_fixed(school_df$Decision_Raw, c("Other")) == TRUE)] <- "Other"
      school_df$Decision <- ifelse(school_df$Decision %in% c("Accepted", "Rejected",
                                                             "Waitlist", "Other"), school_df$Decision, NA)
      
      # Figuring out student state
      school_df$StudentState <- school_df %>% 
        with(replace(StudentState, StudentState == "", NA))
      
      # Figuring out program
      school_df$Level <- NA
      school_df$Level[which(stri_detect_fixed(school_df$Program, "PhD") == TRUE)] <- "PhD"
      school_df$Level[which(stri_detect_fixed(school_df$Program, "Masters") == TRUE)] <- "Masters" 
      
      # Dates
      school_df$Date <- as.Date(school_df$Date, format = "%d %b %Y")
      
      # Binding
      school_df_test <- rbind(school_df_test, school_df)
      
    }
    
    # Dropping variables
    school_df_final <- school_df_test %>% 
      select(School, Program, Decision, StudentState, Date, Level)
    
    # Returning df
      return(school_df_final)
  } else {
    return("Pick a better school")
  }
}

test <- admissions_scrape("duke")
admissions_scrape("sfu")
str(test)
