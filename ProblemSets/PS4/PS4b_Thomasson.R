#!/usr/bin/Rbatch

library(sparklyr)
library(tidyverse)

sc <- spark_connect(master = "local")

df1 <- as_tibble(iris,overwrite = TRUE)

df <- copy_to(sc, df1,overwrite = TRUE)

class(df1)
class(df)

print(names(df1))
print(names(df))

df %>% select(Sepal_Length, Species) %>% head %>% print

df %>% filter(Sepal_Length > 5.5) %>% head %>% print

df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head %>% print

df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length, na.rm = TRUE), count = n()) %>% collect()  # Convert back to regular R dataframe

print(df2)

df2 <- df2 %>% arrange(Species)

print(df2)
