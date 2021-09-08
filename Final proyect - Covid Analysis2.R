#instalar y cargar librerias para hacer webscrapping 
#install.packages("httr")
#install.packages("rvest")
require("httr")
require("rvest")

library(httr)
library(rvest)

#task 1, obtener pagina http con un HTTP request
get_wiki_covid19_page <- function() {
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  wiki_covid_page <- list(title="Template:COVID-19_testing_by_country")
  
  url_page <- GET(wiki_base_url, query=wiki_covid_page)  
  
  return(url_page)
}


get_wiki_covid19_page()


# TASK 2: Extraer la informacion de la pagina (tabla) con formato <table>
# Get the root html node from the http response in task 1 
library(rvest)
root_node_wiki <- read_html(get_wiki_covid19_page())
print(root_node_wiki)

# Get the table node from the root html node
body_node_wiki <- html_node(root_node_wiki, "table")
print(body_node_wiki)
#Read the table node as a data frame using html_table function
table_wiki_covid <- html_table(body_node_wiki)
print(table_wiki_covid)

#TASK 3: Pre-process and export the extracted data frame

# Print the summary of the data frame
summary(table_wiki_covid)

#formateo de la tabla
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
# call `preprocess_covid_data_frame` function and assign it to a new data frame
table_wiki_covid_final <- preprocess_covid_data_frame(table_wiki_covid)

# Print the summary of the processed data frame again
summary(table_wiki_covid_final)

# Export the data frame to a csv file
write.csv(table_wiki_covid_final,file = "C:/Users/Julian/Documents/covid.csv", row.names = FALSE)
#poner la direccion de ibm watson studio

## Get working directory
wd <- getwd()
## Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
## File path
#print(file_path)
#file.exists(file_path)

#TASK 4: Get a subset of the extracted data frame

covid_data_frame <- read.csv("C:/Users/Julian/Documents/covid.csv")

covid_data_frame[5:10,c("country","confirmed")]

#TASK 5: Calculate worldwide COVID testing positive ratio

# Get the total confirmed cases worldwide
total_confirmed_cases <- sum(covid_data_frame$confirmed)
print(total_confirmed_cases)
# Get the total tested cases worldwide
total_tested_cases <- sum(covid_data_frame$tested)
print(total_tested_cases)
# Get the positive ratio (confirmed / tested)
positive_confirmed_ratio <- total_confirmed_cases/total_tested_cases*100
print(positive_confirmed_ratio)
#round(positive_confirmed_ratio,2)

#TASK 6: Get a country list which reported their testing data

# Get the `country` column
covid_data_frame[,"country"]
# Check its class (should be Factor)
class(covid_data_frame[,"country"])
# Conver the country column into character so that you can easily sort them
covid_data_frame[,"country"] <- as.character(covid_data_frame[,"country"])
# Sort the countries AtoZ
sort(covid_data_frame[,"country"])
# Sort the countries ZtoA
sort(covid_data_frame[,"country"], decreasing = TRUE )
# Print the sorted ZtoA list
### CHEQUEARRRR
countries.Z_A <- sort(covid_data_frame[,"country"], decreasing = TRUE )
print(countries.Z_A)

#TASK 7: Identify countries names with a specific pattern

# Use a regular expression `United.+` to find matches

reg_pattern <- "United+."

# Print the matched country names
grep(reg_pattern,covid_data_frame[,"country"], value = TRUE)

#TASK 8: Pick two countries you are interested, and then review their testing data

# Select a subset (should be only one row) of data frame based on a selected country name and columns
seleccionArgentina <- table_wiki_covid_final["country"] == "Argentina"
table_wiki_covid_final[seleccionArgentina, c("country","confirmed","confirmed.population.ratio")]

# Select a subset (should be only one row) of data frame based on a selected country name and columns
seleccionChile <- table_wiki_covid_final["country"] == "Chile"
table_wiki_covid_final[seleccionChile, c("country","confirmed","confirmed.population.ratio")]

#TASK 9: Compare which one of the selected countries has a larger ratio of confirmed cases to population
### CHEQUEAR EL DOBLE IF
if(covidChile[,"confirmed.population.ratio"] > covidArgentina[,"confirmed.population.ratio"]) {
  print("Chile has a higher COVID infection risk that Argentina")
 }else{
    print("Argentina has a higher COVID infection risk that Chile")
}

#TASK 10: Find countries with confirmed to population ratio rate less than a threshold
# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
threshold <- mean(table_wiki_covid_final$confirmed.population.ratio)
print(threshold)

table_covid2 <- subset(table_wiki_covid_final, table_wiki_covid_final$confirmed.population.ratio > threshold)
#de las dos formas funciona
#table_wiki_covid_final[(table_wiki_covid_final["confirmed.population.ratio"]<threshold),]
print(threshold)
print(table_covid2)
