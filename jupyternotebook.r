library(tidyverse)
library(httr)
library(rvest)
#Task1
get_wiki_covid19_page <- function() {
    
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
    # 1) base URL `https://en.wikipedia.org/w/index.php  
    # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
    
  # Wiki page base
  wiki_base_url <-  "https://en.wikipedia.org/w/index.php"
  
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
   # in our case, it will be `Template:COVID-19_testing_by_country`
  wiki_params <- list(title = "Template:COVID-19_testing_by_country")
  
  
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
   wiki_response <- GET(wiki_base_url, query = wiki_params) 
   
  # Use the `return` function to return the response
return(wiki_response)
}
covid19_response <- get_wiki_covid19_page()
print(covid19_response)

#Task2
#read_html
covid19_root_node <- read_html(covid19_response)
#html_node
covid19_table_node <- html_node(covid19_root_node, "table")
#data_frame
covid19_data_frame <- html_table(covid19_table_node )
covid19_data_frame

#Task3
#summary
summary(covid19_data_frame)
preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units[b]"] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}
new_covid_data_frame <- preprocess_covid_data_frame(covid19_data_frame)

head(new_covid_data_frame)
#get summary again
summary(new_covid_data_frame)
#write the covid frame
write.csv(new_covid_data_frame, "covid.csv")
# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
# File path
print(file_path)
file.exists(file_path)

#Task4
# Read covid_data_frame_csv from the csv file
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Get the 5th to 10th rows, with two "country" "confirmed" columns
covid_data_frame_row <- covid_data_frame_csv %>% select(country, confirmed)
covid_data_frame_row[c(5:10), ]

#Task5
# Get the total confirmed cases worldwide
total_confirmed_cases <- sum(covid_data_frame_csv$confirmed)
total_confirmed_cases
# Get the total tested cases worldwide
total_tested_cases <- sum(covid_data_frame_csv$tested)
total_tested_cases
# Get the positive ratio (confirmed / tested)
positive_ratio <- total_confirmed_cases / total_tested_cases
positive_ratio

#Task6
# Get the `country` column
head(covid_data_frame_csv$country)
# Check its class (it is already character)
class(covid_data_frame_csv$country)
# Convert the country column into character so that you can easily sort them
covid_data_frame_csv$country <- as.character(covid_data_frame_csv$country)
covid_data_frame_csv$country <- covid_data_frame_csv$country %>% modify_if(is.factor, as.character)

# Sort the countries A to Z
covid_data_frame_csv %>% arrange(country)
# Sort the countries Z to A
sort(covid_data_frame_csv$country, decreasing = TRUE)

#Task7
# Use a regular expression `United.+` to find matches
matched_country <- grep("United.+", covid_data_frame_csv$country)

# Print the matched country names
covid_data_frame_csv$country[matched_country]

#Task8
# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_India <- covid_data_frame_csv %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "India")

# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_Argentina <- covid_data_frame_csv %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "Argentina")

covid_data_frame_India
covid_data_frame_Argentina

#Task9
# Use if-else statement
if (covid_data_frame_Argentina$confirmed.population > covid_data_frame_India$confirmed.population) {
    print("Argentina has higher COVID-19 infection risk")
  } else {
    print("India has higher COVID-19 infection risk")
 }

#Task10
subset(covid_data_frame_csv, subset = confirmed.population.ratio < 1)
