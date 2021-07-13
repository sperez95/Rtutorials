###Youtube tutorial on for loops
# https://www.youtube.com/watch?v=QdyGZ4O6vVs&t=45s

#basic structure of a for loop

#for (item in list_of_items) {
 # dosomething(item)
 # return()
#}
# need return() or print() to display items from the loop

#example
#volume can be any name you make for the loop, volumes is the object that was created before the for function

volumes <- c(1.6, 3, 8)

for (volume in volumes) {
  print(2.65 * volume^0.9)
}
#this loop is assigning the first number in volumes to the calculation, then doing it for the other two values

#multiple rows in a loop body

for (volume in volumes) {
  mass <- 2.65*volume^0.9
  mass_lb <- mass*2.2
  print(mass_lb)
}

##looping with an index and store results
#length(volumes) goes to the number that is equivalent to the length of volumes (3)
#instead of assigning volume, call for volumes directly using i in mass function

for (i in 1:length(volumes)) {
  mass <- 2.65 * volumes[i]^0.9
  print(mass)
}
#how to store results from loop
#create an empty object

masses <- vector(mode="numeric", length = length(volumes))

#add ith values to new object

for(i in 1:length(volumes)) {
  mass <- 2.65 * volumes[i]^0.9
  masses[i] <- mass
}
masses

##looping using values from multiple vectors
#seq_along creates a sequence from 1 to however long your object is

b0 <- c(2.65, 1.28, 3.29)
b1 <- c(0.9, 1.1, 1.2)
masses <- vector(mode="numeric", length= length(volumes))
for (i in seq_along(volumes)) {
  mass <- b0[i] * volumes[i] ^ b1[i]
  masses[i] <- mass
}

#looping over files - repeat the same action over various files
#satellite collar locations downloaded from this website

download.file("http://www.datacarpentry.org/semester-biology/data/locations.zip",
              "locations.zip")
unzip("locations.zip")

#create an object that lists different text files with the name starting with locations-
data_files <- list.files(pattern = "locations-.*.txt", full.names = TRUE)
data_files

#call number of observations in each file
#create empty object

results <- vector(mode="integer", length=length(data_files))

#the loop converts the list.files to a csv then counts the number of rows within each text file

for (i in 1:length(data_files)) {
  data <- read.csv(data_files[i])
  count <-nrow(data)
  results[i] <- count
}
results

#instead of creating an empty vector, create a an empty dataframe to add the loop data to
#in data.frame, new column called file_name, count= a new column that counts how long the files are

results <- data.frame(file_name = character(length(data_files)), count= integer(length(data_files)),
                                                                            stringsAsFactors = FALSE)
results

for (i in 1:length(data_files)) {
  data <- read.csv(data_files[i])
  count <- nrow(data)
  results$file_name[i] <- data_files[i]
  results$count[i] <- count
}
results

##using apply function instead of large loops
#another option for what we did above
#sometimes apply function works faster than for loops

get_counts <- function(data_file_name) {
  file <- read.csv(data_file_name)
  count <- nrow(file)
  return(count)
}

#want a dataframe, not a list, so use unlist
#returns a vector because the function calls for a vector in return
results <- unlist(lapply(data_files, FUN = get_counts))
results
