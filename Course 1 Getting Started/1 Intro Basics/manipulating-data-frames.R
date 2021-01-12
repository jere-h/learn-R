#####creating dataframes

variable1 <- c(1,2,3,4,5)
variable2 <- c(6,7,8,9,10)

data.frame(variable1,variable2)

my_dat <- data.frame("height"=variable1,"weight"=variable2)

my_dat

#####get a single column of a dataframe
my_dat$weight

####use a column in a function
mean(my_dat$weight)

####save a column to another object
my_weights <- my_dat$weight
my_weights


####selecting data from dataframes

#####remember you specify row, then column in the brackets

###first row, all columns
my_dat[1,]

###first column, all rows
my_dat[,1]

###first column, first row,
my_dat[1,1]

###first column, first three rows
my_dat[1:3,1]





#####creating new columns
####single value repeated
my_dat$variable3 <- 100
my_dat

####add vector with same length
my_dat$variable4 <- c("apple","orange","grape","cherry","melon")
my_dat

####this won't work because the vector is too short - needs to be same length as other vectors/columns in the dataframe
my_dat$variable5 <- c("banana","mango")







####get the dimensions of a dataframe (rows and columns)
dim(my_dat)

####get more information about the structure of a data frame
str(my_dat)
nrow(my_dat)
ncol(my_dat)

###get the column headers of the data frame
names(my_dat)

###if you have a big dataframe, use head() to see the first few rows or tail to see the last few
head(my_dat)
tail(my_dat)

####Spreadsheet View
View(my_dat)




#####Missing data causes lots of problems. Some functions "break" and throw errors if you include missing data
with_missing <- c(1,2,3,NA)
sum(with_missing)

#### You should look at the documentation for a function to understand how it handles missing data. Sometimes you can use an argument with a function to tell it how to deal with the missing data, often telling the function to ignore the missing cells.

?sum

sum(with_missing,na.rm=TRUE)


#####combining two dataframes with cbind

my_dat2 <- data.frame("variable4"=400:499,"variable5"=500:599)

all_dat<- cbind(my_dat,my_dat2)

head(all_dat)

#### adding a row

new_row <- c(1000,2000,3000,4000)

all_dat_plus_new_row <- rbind(all_dat,new_row)

tail(all_dat_plus_new_row)

#### combining two dataframes with rbind

#####you have to make sure the two dataframes have the same column names and order

#######this won't work
my_dat <- data.frame("variable1"=1:100,"variable2"=200:299)

my_dat2 <- data.frame("variable4"=400:499,"variable5"=500:599)

rbind(my_dat,my_dat2)

######this will work
my_dat <- data.frame("variable1"=1:100,"variable2"=200:299)

my_dat2 <- data.frame("variable1"=400:499,"variable2"=500:599)

rbind(my_dat,my_dat2)
