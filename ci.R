netrc <- ifelse(.Platform$OS.type == "windows", "~/_netrc", "~/.netrc")
write(
  x = paste(
    "machine dataspace.cavd.org",
    "login", Sys.getenv("DSR_login"),
    "password", Sys.getenv("DSR_pwd")
  ),
  file = netrc
)
