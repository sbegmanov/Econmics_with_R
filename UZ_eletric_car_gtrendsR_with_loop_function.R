library(readr)
library(gtrendsR)
library(purrr)
#  Sys.setlocale(category = "LC_COLLATE", locale = "Russian")

kwlist <- c("electric vehicles", "electric cars", "hybrid electric vehicles", "electric two-wheelers", "electric scooters",
            "электромобили", "электромобили», «гибридные электромобили", "электрические двухколесные транспортные средства", 
            "электрические скутеры","elektr transport vositalari", "elektr avtomobillar", "gibrid elektr transport vositalari", 
            "elektrik ikki g'ildirakli velosipedlar", "elektr skuterlar", "електр транспорт воситалари",  "електр автомобиллар",
            "гибрид электр транспорт воситалари",  "електрик икки ғилдиракли велосипедлар",  "електр скутерлар")

# interest_over_time ### grop - web
googleTrendsData <- function (keywords) {
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'web'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time
}
output_interest_over_time <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_over_time, "output_interest_over_time_web.csv")

# interest_over_time ### grop - news
googleTrendsData <- function (keywords) {
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'news'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time
}
output_interest_over_time <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_over_time, "output_interest_over_time_news.csv")

# interest_over_time ### grop - images
googleTrendsData <- function (keywords) {
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'images'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time
}
output_interest_over_time <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_over_time, "output_interest_over_time_images.csv")

# interest_over_time ### grop - froogle
googleTrendsData <- function (keywords) {
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'froogle'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time
}
output_interest_over_time <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_over_time, "output_interest_over_time_froogle.csv")

# interest_over_time ### grop - youtube
googleTrendsData <- function (keywords) {
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'youtube'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time
}
output_interest_over_time <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_over_time, "output_interest_over_time_youtube.csv")



# interest_by_city ###
googleTrendsData <- function (keywords) { 
  
  country <- c('UZ') 
  time <- ("today+5-y") 
  channel <- 'web'
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_by_region
}


output_interest_by_city <- map_dfr(.x = kwlist, .f = googleTrendsData)
write.csv(output_interest_by_city, "output_interest_by_city_web.csv")

