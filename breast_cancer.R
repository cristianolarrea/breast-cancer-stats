library(tidyverse)
df <- read.csv('data.csv')
df

#We're going to use just the mean columns
df <- df %>% select("id",
              "diagnosis",
              "radius_mean",
              "texture_mean",
              "perimeter_mean",
              "area_mean",
              "smoothness_mean",
              "compactness_mean",
              "concavity_mean",
              "concave_points_mean",
              "symmetry_mean",
              "fractal_dimension_mean")

#Tirando area, e perimetro, já que temos raio
df <- df %>% select("diagnosis",
                    "radius_mean",
                    "texture_mean",
                    "smoothness_mean",
                    "compactness_mean",
                    "concavity_mean",
                    "concave_points_mean",
                    "symmetry_mean",
                    "fractal_dimension_mean")

plot(df)

