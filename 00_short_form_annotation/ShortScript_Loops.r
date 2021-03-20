#============================================================================#

# Loops #
# 
# written by Henrik von Wehrden
# edited by Chan Le

# In this script:

### FOR Loops
### WHILE Loops
### REPEAT Loops
### Examples

#============================================================================#
#### 1. FOR Loops ####
#============================================================================#

# Loop through forward range
for (i in 1:10){print(i)}

# Loop through backward range
for (i in 5:1){print(i)}

# Loop through certain vector
for (i in c(1,3,4,10)){print(i)}


#============================================================================#
#### 2. WHILE Loops ####
#============================================================================#

# Set initial state
n = 1

while(n < 11){
    print(n)
    n <- n + 1
}

#============================================================================#
#### 3. REPEAT Loops ####
#============================================================================#

# Set initial state
n = 1

repeat{
    print(n)
    n <- n + 1
    
    # Break condition
    if(n > 10) break
}

#============================================================================#
#### 4. Examples ####
#============================================================================#

### Example: Add 1 to a whole vector
z <- 1:10
for (i in 1:10){
    z[i] <- z[i] + 1
}

# Vectorization instead of loop
z <- 1:10
z <- z+1


### Example: Extract string from a phrase
phrase<-"the quick brown fox jumps over the lazy dog"

# Placeholder
q<-character(20)

# Write extracted string to vector q
for (i in 1:20) q[i]<- substr(phrase,1,i)

# Look at vector q
q


### Example: Plot multiple samples
par(mfrow=c(2,2))
for (i in 1:4){
    
    # Sample 50 data points from the normal distribution (mean 0 and sd 1)
    daten <- rnorm(50)
    
    # Scatter plot
    plot(daten, main = paste("Stichprobe",i))
}

# Reset plotting canvas
par(mfrow = c(1,1))

### Example: Loop with data

# Load data (required package MASS)
utils::data(npk, package="MASS")
attach(npk)
head(npk, 10)

# Create placeholder matrix
result<-matrix(ncol=2,nrow=3)
colnames(result) <- c("nutrients","p.value")
result

# Boxplots
boxplot(yield ~ N)
boxplot(yield ~ P)
boxplot(yield ~ K)

for (i in 2:4){
    print(i)
    
    # ANOVA model
    model <- aov(yield ~ npk[,i])
    
    # Write name of nutrients in the result table (first column)
    result[i-1,1] <- colnames(npk)[i]
    
    # Write p-value of model in the result table (second column)
    result[i-1,2] <- summary(model)[[1]]$P[1]
}

# Check result table
result

### Example: Climate loop

# Import and check data
hvw1<-read.table("climate.txt",header=T)
attach(hvw1)

# Check column names
colnames(hvw1)

# Loop through each row in data
for (i in 1:5){
    
    # Extract min temperature
    Min.t. <- as.numeric(hvw1[i,6:17])
    
    # Extract max temperature
    Max.t. <- as.numeric(hvw1[i,18:29])
    
    # Extract prec
    Prec<-as.numeric(hvw1[i,30:41])
    
    # Create data frame station
    station<-data.frame(Prec, Min.t., Max.t., Ab.m.t. = Min.t.)
    
    # Transpose data frame
    stationT <- as.data.frame(t(station))
    
    # Label the columns
    names(stationT) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                       "Jul", "Aug", "Sep","Oct", "Nov", "Dec")

    # Begin saving plot
    jpeg(filename = paste0(hvw1$POINTNO[i], ".jpg"))
    
    # Plot Walter & Lieth climatic diagram of the station 
    # (requires package climatol)
    climatol::diagwl(stationT, 
           est = paste(hvw1$POINTNO[i],
                              "; LON:",hvw1$LON_EX[i],
                              "; LAT:",hvw1$LAT_EX[i]),
           alt=hvw1$ALT_EX[i])
    
    # End saving plot
    dev.off()
}




