library(foreign)

dataset <- read.spss("/Users/sheliaxin/Desktop/customer segmentation/Latent Class Modeling/LG5.0/LG50demo/DemoData/gss82white.sav")
df <- as.data.frame(dataset)
df
write.csv(df, "gss83white.csv", row.names = FALSE)
