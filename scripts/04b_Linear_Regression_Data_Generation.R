

area <- rnorm(200, 40, 4) %>% round(1)
fromCenter <- pmax(0.5, rnorm(200, 5, 2)) %>% round(1)
onFloor <- runif(200, 0, 4) %>% round(0)

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

write.csv2(data, "data/rent.csv", row.names = F)

data %>% select(area, fromCenter, onFloor, rent) %>% write.csv2("data/rentSimple.csv", row.names = F)

data  %>% write.csv2("data/rentFull.csv", row.names = F)

shapiro.test(data$rent)