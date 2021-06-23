## Set STAGING to TRUE in .Renviron to run tests against staging server.
assign("onStaging", identical(tolower(Sys.getenv("STAGING")), "true"))
assign("baseUrl", ifelse(onStaging, "https://dataspace-staging.cavd.org", "https://dataspace.cavd.org"))
