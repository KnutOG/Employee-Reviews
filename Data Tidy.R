library(readr)
employee_reviews <- read_csv("employee_reviews.csv")
View(employee_reviews)
summary(employee_reviews)

#Changing the job title columns into 2 different columns Employee Status and Position
library(tidyr)
library(dplyr)
employee_reviews<- employee_reviews %>% separate (`job-title`, c("Employee Status", "Position"), sep = "-" )
employee_reviews$`Employee Status` <- as.factor(employee_reviews$`Employee Status`)

##Changing the none value with an NA value to be able to run the mice function
none <- c("none")

employee_reviews<- employee_reviews%>% 
  mutate (`work-balance-stars` = ifelse(`work-balance-stars` %in% none, NA, `work-balance-stars`))
employee_reviews<- employee_reviews%>%
  mutate (`culture-values-stars` = ifelse(`culture-values-stars`%in% none, NA, `culture-values-stars`))
employee_reviews<- employee_reviews%>%
  mutate (`carrer-opportunities-stars` = ifelse(`carrer-opportunities-stars` %in% none, NA, `carrer-opportunities-stars`))
employee_reviews<- employee_reviews%>%
  mutate (`comp-benefit-stars`= ifelse(`comp-benefit-stars` %in% none, NA, `comp-benefit-stars`))
employee_reviews<- employee_reviews%>%
  mutate (`senior-mangemnet-stars` = ifelse(`senior-mangemnet-stars` %in% none, NA, `senior-mangemnet-stars`))
employee_reviews<- employee_reviews%>%
  mutate (`advice-to-mgmt` = ifelse(`advice-to-mgmt` %in% none, NA, `advice-to-mgmt`))

#Changing all the rating varaibles from chr to Numeric
employee_reviews$`work-balance-stars` <- as.numeric(employee_reviews$`work-balance-stars`)
employee_reviews$`culture-values-stars` <- as.numeric(employee_reviews$`culture-values-stars`)
employee_reviews$`carrer-opportunities-stars` <- as.numeric(employee_reviews$`carrer-opportunities-stars`)
employee_reviews$`comp-benefit-stars` <- as.numeric(employee_reviews$`comp-benefit-stars`)
employee_reviews$`senior-mangemnet-stars` <- as.numeric(employee_reviews$`senior-mangemnet-stars`)
summary(employee_reviews)
library(mice)
md.pattern(employee_reviews)
#THe matrix gives us a good understanding on how the NA values are distrbuted in the data. 
# For example we know that 6693 rows are missing the star rating. The overall rating are complete. 

## Now to replace the NA values of the star there is two way a) we use the mice package (ppm, midastouch...) b) we simply replace it with the value of the overall ratings (make sense to me..)
## Lets do a)
#m= 10 it represents approx the % of the missing data

DataTest <- employee_reviews%>% 
      select(X1,company,`work-balance-stars`,`culture-values-stars`, `carrer-opportunities-stars`, `comp-benefit-stars`, `senior-mangemnet-stars`)
DataTest2 <- complete(mice(DataTest,m=10,maxit=50,meth='norm',seed=500))

##Im having problem with the mice function! Intially I left the variables as character and the NA were still there. Now I changed to numeric and now I get an error. Its driving me crazy

