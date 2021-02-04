

area <- rnorm(200, 40, 4)
fromCenter <- rnorm(200, 5, 2)
onFloor <- runif(200, 0, 4)

petsAllowed <- sample(c("No","Yes"), 200, T) %>% factor(levels = c("No","Yes"))

flooringType <- sample(c("Vinyl","Carpet","Tiles","Hardwood"), 200, T) %>% factor(levels = c("Vinyl","Carpet","Tiles","Hardwood"))

rent <- 100 + 8*area - 12*fromCenter + 4*onFloor + 7*as.integer(flooringType) + 50*as.integer(petsAllowed) + rnorm(200, 0, 30) %>% round(0)

boxplot(rent)

data <- data.frame(area = area,
                   fromCenter = fromCenter,
                   onFloor = onFloor,
                   petsAllowed = petsAllowed,
                   flooringType = flooringType,
                   rent = rent)

lm(rent ~., data = data) %>% summary()

write.csv2(rent, "data/rent.csv")
