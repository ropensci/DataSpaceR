assign("onStaging", identical(tolower(Sys.getenv("STAGING")), "true"), .GlobalEnv)
assign("baseUrl", ifelse(onStaging, "https://dataspace-staging.cavd.org", "https://dataspace.cavd.org"))
