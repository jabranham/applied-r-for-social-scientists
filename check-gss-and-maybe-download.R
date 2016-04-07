## If you source() this file, it checks whether the gss file is in a
## subdirectory "data". If so, nothing happens. If not, it creates the
## directory, downloads the cumulative gss file from NORC's website,
## and unzips the downloaded file

if(!file.exists("./data/GSS7214_R5.DTA")){
  if(!dir.exists("./data/")){
    dir.create("./data/")
  }
  download.file(url="http://gss.norc.org/documents/stata/GSS_stata.zip",
                destfile="./data/2014_stata.zip")
  unzip("./data/2014_stata.zip", exdir = "./data")
}

if(file.exists("./data/GSS7214_R5.DTA")){
  print("GSS file exists!")
} else {
  print("Error - GSS couldn't be found!")
}
