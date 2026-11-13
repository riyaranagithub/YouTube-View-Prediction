install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")


# ==========================================
# YOUTUBE VIEW PREDICTION
# Collect YouTube Data and Create CSV
# ==========================================

library(httr)
library(jsonlite)
library(dplyr)

# 1. Enter your API key
api_key <- Sys.getenv("YOUTUBE_API_KEY")

# 2. Categories to search
categories <- c(
  "Education",
  "Gaming",
  "Music",
  "Technology",
  "Cooking"
)

# Number of pages per category
# Each page can return up to 50 videos
max_pages <- 2

# Function to make API requests
get_api_data <- function(url, params) {
  
  response <- GET(url, query = params)
  
  if (status_code(response) != 200) {
    stop(content(response, "text", encoding = "UTF-8"))
  }
  
  fromJSON(
    content(response, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

# ==========================================
# 3. SEARCH FOR VIDEOS
# ==========================================

video_list <- list()

search_url <- "https://www.googleapis.com/youtube/v3/search"

for (category in categories) {
  
  cat("Searching:", category, "\n")
  
  page_token <- NULL
  
  for (page in seq_len(max_pages)) {
    
    params <- list(
      part = "snippet",
      type = "video",
      q = category,
      regionCode = "IN",
      maxResults = 50,
      key = api_key
    )
    
    if (!is.null(page_token)) {
      params$pageToken <- page_token
    }
    
    result <- get_api_data(search_url, params)
    
    if (length(result$items) == 0) break
    
    for (item in result$items) {
      
      video_list[[length(video_list) + 1]] <- data.frame(
        video_id = item$id$videoId,
        title = item$snippet$title,
        channel_id = item$snippet$channelId,
        published_at = item$snippet$publishedAt,
        search_category = category
      )
    }
    
    page_token <- result$nextPageToken
    
    if (is.null(page_token)) break
  }
}

# Combine search results
search_data <- bind_rows(video_list)

# Remove duplicate videos
search_data <- distinct(search_data, video_id, .keep_all = TRUE)

cat("Videos found:", nrow(search_data), "\n")

# ==========================================
# 4. FETCH VIDEO STATISTICS
# ==========================================

video_ids <- unique(search_data$video_id)

video_url <- "https://www.googleapis.com/youtube/v3/videos"

# Split IDs into batches of 50
video_batches <- split(
  video_ids,
  ceiling(seq_along(video_ids) / 50)
)

video_details <- list()

for (ids in video_batches) {
  
  cat("Fetching video statistics...\n")
  
  result <- get_api_data(
    video_url,
    list(
      part = "snippet,statistics,contentDetails",
      id = paste(ids, collapse = ","),
      key = api_key
    )
  )
  
  for (item in result$items) {
    
    stats <- item$statistics
    snippet <- item$snippet
    details <- item$contentDetails
    
    get_number <- function(x) {
      if (is.null(x)) NA_real_ else as.numeric(x)
    }
    
    video_details[[length(video_details) + 1]] <- data.frame(
      video_id = item$id,
      title = snippet$title,
      channel_id = snippet$channelId,
      category_id = snippet$categoryId,
      duration = details$duration,
      views = get_number(stats$viewCount),
      likes = get_number(stats$likeCount),
      comments = get_number(stats$commentCount)
    )
  }
}

video_data <- bind_rows(video_details)

# ==========================================
# 5. FETCH CHANNEL SUBSCRIBERS
# ==========================================

channel_ids <- unique(video_data$channel_id)

channel_batches <- split(
  channel_ids,
  ceiling(seq_along(channel_ids) / 50)
)

channel_details <- list()

channel_url <- "https://www.googleapis.com/youtube/v3/channels"

for (ids in channel_batches) {
  
  cat("Fetching channel information...\n")
  
  result <- get_api_data(
    channel_url,
    list(
      part = "statistics",
      id = paste(ids, collapse = ","),
      key = api_key
    )
  )
  
  for (item in result$items) {
    
    subs <- item$statistics$subscriberCount
    
    channel_details[[length(channel_details) + 1]] <- data.frame(
      channel_id = item$id,
      subscribers = if (is.null(subs)) NA_real_
      else as.numeric(subs)
    )
  }
}

channel_data <- bind_rows(channel_details)

# ==========================================
# 6. COMBINE ALL DATA
# ==========================================

final_data <- video_data %>%
  left_join(channel_data, by = "channel_id") %>%
  left_join(
    search_data %>% select(video_id, search_category),
    by = "video_id"
  )

# ==========================================
# 7. EXPORT CSV FILE
# ==========================================

write.csv(
  final_data,
  "youtube_dataset.csv",
  row.names = FALSE,
  na = ""
)

cat("\nDataset created successfully!\n")
cat("Total videos:", nrow(final_data), "\n")
cat("CSV file: youtube_dataset.csv\n")

# Display dataset
head(final_data)