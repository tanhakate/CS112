
### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.

  {
  
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))

}

# Now R knows that these columns are comprised of dates
# After replication, I obtain the same results as the github post. 

foo[3,12]

foo[4,12]

foo[3,12] - foo[4,12]

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
new_foo <- foo[-which.have.NAs, ]
# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.


head(new_foo)

names(new_foo)

#I will initialize a vector test_foo for the dataset with non-missing "Circulation.Date" >= 2008-01-01.

test_foo <- new_foo[which(new_foo$CirculationDate >= "2008-01-01"),]

dim(test_foo)

head(test_foo)

# I am adding a new column to the dataset which lists the project duration in units of days 
test_foo$ProjectDuration <- test_foo$OriginalCompletionDate-test_foo$ApprovalDate

head(test_foo)

#Upon inspecting the table, I realize that some projects don't have an original completion date. 
sum(is.na(test_foo$OriginalCompletionDate))
sum(is.na(test_foo$ApprovalDate))

#Since there are 'NA' values in the ProjectDuration column, I will set the na.rm logical to TRUE so that R can remove the missing values. 
#I will also divide the result by 365 to evaluate the average project duration in years.
mean(as.numeric(test_foo$ProjectDuration), na.rm = TRUE)/365
quantile(as.numeric(test_foo$ProjectDuration), na.rm = TRUE)/365

#Now, we will check the distribution of project duration in years to see how long different projects took to complete.
hist(as.numeric(test_foo$ProjectDuration)/365, main="Project Duration Distribution for Projects Circulated After 2008-01-01",xlab="Project Durations (months)",col="gray",label=TRUE,plot = TRUE)
#Note that the hist function automatically removes NA values while plotting the histogram. 

range(test_foo$CirculationDate) #First, we will find the range of circulation dates. 

#Now, let's divide the range in half and separate the dataset accordingly.

diff <- as.Date(as.character("2018-06-29"), format="%Y-%m-%d")-
              as.Date(as.character("2008-01-07"), format="%Y-%m-%d")
print(diff/2)

cut_off <- as.Date(as.character("2008-01-07"), format="%Y-%m-%d") + diff/2
print(cut_off)

circulated_earlier <- test_foo[which(new_foo$CirculationDate < "2013-04-03"),]
circulated_later <- test_foo[which(new_foo$CirculationDate >= "2013-04-03"),]

# I am adding a new column to each dataset which lists the project duration in units of days 
circulated_earlier$ProjectDuration <- circulated_earlier$OriginalCompletionDate-circulated_earlier$ApprovalDate

circulated_later$ProjectDuration <- circulated_later$OriginalCompletionDate-circulated_later$ApprovalDate

#Average project duration and quantiles for projects circulated before 2013-04-03

mean(as.numeric(circulated_earlier$ProjectDuration), na.rm = TRUE)/365
quantile(as.numeric(circulated_earlier$ProjectDuration), na.rm = TRUE)/365

#Average project duration and quantiles for projects circulated after 2013-04-03

mean(as.numeric(circulated_later$ProjectDuration), na.rm = TRUE)/365
quantile(as.numeric(circulated_later$ProjectDuration), na.rm = TRUE)/365

p1 <- hist(as.numeric(circulated_earlier$ProjectDuration)/365, main="Project Duration Distribution for Projects Circulated Before 2013-04-03",xlab="Project Durations (months)",col="gray",label=TRUE,plot = TRUE)
p2 <- hist(as.numeric(circulated_later$ProjectDuration)/365, main="Project Duration Distribution for Projects Circulated After 2013-04-03",xlab="Project Durations (months)",col="gray",label=TRUE,plot = TRUE)


# I am adding a new column to the dataset which lists the actual project duration in units of days 
test_foo$ActualProjectDuration <- test_foo$RevisedCompletionDate-test_foo$ApprovalDate

head(test_foo)

#Upon inspecting the table, I realize that all projects have a revised completion date. 
sum(is.na(test_foo$RevisedCompletionDate))
sum(is.na(test_foo$ApprovalDate))

#Since there are no 'NA' values in the ActualProjectDuration column, I will set the na.rm logical to FALSE since R doesn't have to remove the missing values. 
#I will also divide the result by 365 to evaluate the average actual project duration in years.
mean(as.numeric(test_foo$ActualProjectDuration), na.rm = TRUE)/365
quantile(as.numeric(test_foo$ActualProjectDuration), na.rm = TRUE)/365

hist(as.numeric(test_foo$ActualProjectDuration)/365, main="Actual Project Duration Distribution",xlab="Actual Project Durations (months)",col="gray",label=TRUE,plot = TRUE)


#Now, we will calculate the average difference in means between actual project duration and original planned project duration. 
#We will also evaluate difference in quantiles. 
mean(as.numeric(test_foo$ActualProjectDuration - test_foo$ProjectDuration), na.rm = TRUE)/365
quantile(as.numeric(test_foo$ActualProjectDuration), na.rm = TRUE)/365 - quantile(as.numeric(test_foo$ProjectDuration), na.rm = TRUE)/365

#First, we will check if any of the projects are missing a rating. 
sum(is.na(test_foo$Rating))
range(test_foo$Rating) #Ratings range from 0 to 3.

zero_rating_index <- which(test_foo$Rating == "0")
zero_rating <- length(zero_rating_index) * 100/ dim(test_foo)[1]

one_rating_index <- which(test_foo$Rating == "1")
one_rating <- length(one_rating_index)* 100 / dim(test_foo)[1]

two_rating_index <- which(test_foo$Rating == "2")
two_rating <- length(two_rating_index)* 100 / dim(test_foo)[1]

three_rating_index <- which(test_foo$Rating == "3")
three_rating <- length(three_rating_index)* 100 / dim(test_foo)[1]

percentageA <- round(c(zero_rating, one_rating, two_rating, three_rating), 1)
Rate <- c("0","1","2","3")

df <- data.frame(Rate, Percentage = percentageA)
names(df) <- c('Ratings', "% of Projects")
df

#First, we will use the which function to find the indices of the PPTA projects. 
which.are.PPTAs <- which(test_foo$Type == "PPTA")

# Then,  we  will retain only the rows which exclude PPTA projects. 
new_test_foo <- test_foo[-which.are.PPTAs, ]

zero_rating_index <- which(new_test_foo$Rating == "0")
zero_rating <- length(zero_rating_index) * 100/ dim(new_test_foo)[1]

one_rating_index <- which(new_test_foo$Rating == "1")
one_rating <- length(one_rating_index)* 100 / dim(test_foo)[1]

two_rating_index <- which(new_test_foo$Rating == "2")
two_rating <- length(two_rating_index)* 100 / dim(test_foo)[1]

three_rating_index <- which(new_test_foo$Rating == "3")
three_rating <- length(three_rating_index)* 100 / dim(test_foo)[1]

percentageB <- round(c(zero_rating, one_rating, two_rating, three_rating), 1)
Rate <- c("0","1","2","3")

df1 <- data.frame(Rate, Percentage = percentageB, Difference = percentageA-percentageB)
names(df1) <- c('Ratings', '% of Projects excluding PPTAs',"Difference (Including PPTAs - Excluding PPTAs)")
df1

top_revised_foo <- test_foo[test_foo$RevisedAmount > quantile(test_foo$RevisedAmount,prob=1-25/100),]
bottom_revised_foo <- test_foo[test_foo$RevisedAmount < quantile(test_foo$RevisedAmount,prob=25/100),]

dim(top_revised_foo)
dim(bottom_revised_foo)

top_zero_index <- which(top_revised_foo$Rating == "0")
zero_rating_top <- length(top_zero_index) * 100/ dim(top_revised_foo)[1]

bottom_zero_index <- which(bottom_revised_foo$Rating == "0")
zero_rating_bottom <- length(zero_rating_index) * 100/ dim(bottom_revised_foo)[1]

top_one_index <- which(top_revised_foo$Rating == "1")
one_rating_top <- length(top_one_index)* 100 / dim(top_revised_foo)[1]

bottom_one_index <- which(bottom_revised_foo$Rating == "1")
one_rating_bottom <- length(bottom_one_index)* 100 / dim(bottom_revised_foo)[1]

top_two_index <- which(top_revised_foo$Rating == "2")
two_rating_top <- length(top_two_index)* 100 / dim(top_revised_foo)[1]

bottom_two_index <- which(bottom_revised_foo$Rating == "2")
two_rating_bottom <- length(bottom_two_index)* 100 / dim(bottom_revised_foo)[1]

top_three_index <- which(top_revised_foo$Rating == "3")
three_rating_top <- length(top_three_index)* 100 / dim(top_revised_foo)[1]

bottom_three_index <- which(bottom_revised_foo$Rating == "3")
three_rating_bottom <- length(bottom_three_index)* 100 / dim(bottom_revised_foo)[1]

top_percentage <- round(c(zero_rating_top, one_rating_top, two_rating_top, three_rating_top), 1)
bottom_percentage <- round(c(zero_rating_bottom, one_rating_bottom, two_rating_bottom, three_rating_bottom), 1)

Rate <- c("0","1","2","3")

df2 <- data.frame(Rate, top_percentage, bottom_percentage, top_percentage - bottom_percentage )
names(df2) <- c('Ratings', 'Top 25%','Bottom 25%', "Difference (Top 25% - Bottom 25%)")
df2

#Counts unique values of county

county_A <- aggregate(data.frame(count = top_revised_foo$Dept), list(value = top_revised_foo$Dept), length)
county_A[2] <- round(county_A[2] * 100/ dim(top_revised_foo)[1],2)
county_A <- county_A[order(-county_A$count),]
names(county_A) <- c("County (top 25%)", "Percentage Count")


county_B <- aggregate(data.frame(count = bottom_revised_foo$Dept), list(value = bottom_revised_foo$Dept), length)
county_B[2] <- round(county_B[2] * 100/ dim(bottom_revised_foo)[1],2)
county_B <- county_B[order(-county_B$count),]
names(county_B) <- c("County (bottom 25%)", "Percentage Count")



county_A
county_B


#Count unique values of division

division_A <- aggregate(data.frame(count = top_revised_foo$Division), list(value = top_revised_foo$Division), length)
division_A[2] <- round(division_A[2] * 100/ dim(top_revised_foo)[1],2)
names(division_A) <- c("Division (top 25%)", "Percentage Count")
division_B <- aggregate(data.frame(count = bottom_revised_foo$Division), list(value = bottom_revised_foo$Division), length)
division_B[2] <- round(division_B[2] * 100/ dim(bottom_revised_foo)[1],2)
names(division_B) <- c("Division (bottom 25%)", "Percentage Count")

dim(division_A)
dim(division_B)

#Since the dataframes are 83 and 118 rows long in total, I will only examine the top 10 divisions with the highest count.
top_division_A <- head(division_A[order(-division_A[2]),], 10)
names(top_division_A) <- c("10 Divisions with highest projects (top 25%)", "Percentage Count")
top_division_A

top_division_B <- head(division_B[order(-division_B[2]),], 10)
names(top_division_B) <- c("10 Divisions with highest projects (bottom 25%)", "Percentage Count")
top_division_B

#Count unique values of Cluster

cluster_A <- aggregate(data.frame(count = top_revised_foo$Cluster), list(value = top_revised_foo$Cluster), length)
cluster_A[2] <- round(cluster_A[2] * 100/ dim(top_revised_foo)[1],2)
names(cluster_A) <- c("Cluster (top 25%)", "Count")
cluster_A <- cluster_A[order(-cluster_A[2]),]

cluster_B <- aggregate(data.frame(count = bottom_revised_foo$Cluster), list(value = bottom_revised_foo$Cluster), length)
cluster_B[2] <- round(cluster_B[2] * 100/ dim(bottom_revised_foo)[1],2)
names(cluster_B) <- c("Cluster (bottom 25%)", "Percentage Count")
cluster_B <- cluster_B[order(-cluster_B[2]),]

cluster_A
cluster_B

#Count unique values of country

country_A <- aggregate(data.frame(count = top_revised_foo$Country), list(value = top_revised_foo$Country), length)
country_A[2] <- round(country_A[2] * 100/ dim(top_revised_foo)[1],2)
names(country_A) <- c("Country (top 25%)", " Percentage Count")
country_A <- country_A[order(-country_A[2]),]

country_B <- aggregate(data.frame(count = bottom_revised_foo$Country), list(value = bottom_revised_foo$Country), length)
country_B[2] <- round(country_B[2] * 100/ dim(bottom_revised_foo)[1],2)
names(country_B) <- c("Country (bottom 25%)", "Percentage Count")
country_B <- country_B[order(-country_B[2]),]

country_A
country_B
dim(country_A)
dim(country_B)



#Count unique values of type

type_A <- aggregate(data.frame(count = top_revised_foo$Type), list(value = top_revised_foo$Type), length)
type_A[2] <- round(type_A[2] * 100/ dim(top_revised_foo)[1],2)
type_A <- type_A[order(-type_A[2]),]
names(type_A) <- c("Type (top 25%)", "Percentage Count")
type_B <- aggregate(data.frame(count = bottom_revised_foo$Type), list(value = bottom_revised_foo$Type), length)
type_B[2] <- round(type_B[2] * 100/ dim(bottom_revised_foo)[1],2)
type_B <- type_B[order(-type_B[2]),]
names(type_B) <- c("Type (bottom 25%)", "Percentage Count")

type_A
type_B
