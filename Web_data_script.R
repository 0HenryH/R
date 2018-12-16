#delay requests
# Construct a vector of 2 URLs
urls <- c("http://fakeurl.com/api/1.0/","http://fakeurl.com/api/2.0/")

for(url in urls){
  # Send a GET request to url
  result <- GET(url)
  # Delay for 5 seconds between requests
  Sys.sleep(5)
}

# Construct a directory-based API URL to `http://swapi.co/api`,
# looking for person `1` in `people`
directory_url <- paste("http://swapi.co/api", "people", "1", sep = "/")
directory_url <- paste('http://swapi.co/api', 'people', "1", sep = "/")
# Make a GET call with it
result <- GET(directory_url)

# Create list with nationality and country elements
query_params <- list(nationality = "americans", 
                     country = "antigua")

# Make parameter-based call to httpbin, with query_params
parameter_response <- GET(url = "https://httpbin.org/get", query = query_params)

# Print parameter_response
parameter_response

# Full Edition
get_pageviews <- function(article_title){
  url <- paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents", 
    article_title, 
    "daily/2015100100/2015103100", 
    sep = "/"
  )   
  response <- GET(url, user_agent("my@email.com this is a test")) 
  # Is there an HTTP error?
  if(http_error(response)){ 
    # Throw an R error
    stop("the request failed") 
  }
  # Return the response's content
  content(response)
  
  # Get revision history for "Hadley Wickham"
  resp_json <- rev_history("Hadley Wickham")
  
  # Check http_type() of resp_json
  http_type(resp_json)
  
  # Examine returned text with content()
  content(resp_json, as = 'text')
  
  # Parse response with content()
  content(resp_json, as = 'parsed')
  
  # Parse returned text with fromJSON()
  library(jsonlite)
  fromJSON(content(resp_json, as = 'text'))
}



get_revision_history <- function(article_title){
    # Get raw revision response
    rev_resp <- rev_history(article_title, format = "xml")
    
    # Turn the content() of rev_resp into XML
    rev_xml <- read_xml(content(rev_resp, "text"))
    
    # Find revision nodes
    rev_nodes <- xml_find_all(rev_xml, "//rev")
    
    # Parse out usernames
    user <- xml_attr(rev_nodes, "user")
    
    # Parse out timestamps
    timestamp <- readr::parse_datetime(xml_attr(rev_nodes, "timestamp"))
    
    # Parse out content
    content <- xml_text(rev_nodes)
    
    # Return data frame
    data.frame(user = user,
               timestamp = timestamp,
               content = substr(content, 1, 40))
}

# Call function for "Hadley Wickham"
get_revision_history("Hadley Wickham")
