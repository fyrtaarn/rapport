# Extract data from annual report 2021
# ------------------------------------

source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_load(pdftools, data.table, stringi, stringr)

rootPath <- "c:/Users/ybka/Git-fhi/rapport/npr"

txtpdf <- pdftools::pdf_text(file.path(rootPath, "Aarsrapport_personskadedata_2021.pdf" ))

## Tabell 1
## ----------------------------
tbl01 <- txtpdf[grep("Tabell 1. Kontakt\U00E5rsak fordelt", txtpdf)]
tbl01 <- stringi::stri_split_fixed(tbl01, "\n")

# Total
tbl01tot <- tbl01[[1]][24]
tbl01tot <- stringi::stri_replace_all_regex(tbl01tot, "\\s+", " ") #delete multiple whitespace
tbl01tot <- stringi::stri_replace_all_regex(tbl01tot, "[^\\d+]", " ") #delete all other than digits
tbl01tot

tbl01tot <- trimws(tbl01tot)
## vec01 <- regmatches(tbl01tot, gregexpr("[[:digit:]]+", tbl01tot))
vec01 <- stringi::stri_split_fixed(tbl01tot, " ")
vec01 <- unlist(vec01)
vec01 <- vec01[vec01!="100"]
vec01 <- matrix(vec01, ncol = 2, byrow = T)
vec01 <- as.data.table(vec01)
vec01[, case := paste0(V1, V2)]
vec01
tabTot <- as.numeric(vec01$case)
tabTot



tbl01x <- tbl01[[1]][16:23]
## tbl01x <- gsub("(?<=[\s])\s*|^\s+|\s+$", "", tbl01x, perl = TRUE)
## tbl01x <- stringr::str_squish(tbl01x)
tbl01x <- stringi::stri_replace_all_regex(tbl01x, "\\s+", " ")
tbl01x <- trimws(tbl01x)
tbl01x



table_pro <- function(x, row, char){
  dt <- x[row]

  if (missing(char)){
    dt <- stringi::stri_replace_all_regex(dt, "[^,|\\d+]", " ") #delete all other than , or digits
  } else {
    dt <- stringi::stri_replace_all_fixed(dt, char, "")
  }

  dt <- stringi::stri_replace_all_fixed(dt, ",", ".")
  dt <- stringi::stri_split_fixed( trimws(dt), " " )
  dt <- unlist(dt)
  ulno <- length(dt)
  ulTot <- paste0(dt[ulno -1], dt[ulno])
  dtPro <- dt[1:(ulno - 2)]
  as.numeric(dtPro)

}

ulykke <- table_pro(tbl01x, row = 3)
ulykke

vold <- table_pro(tbl01x, 4, char = "Vold, overfall")
vold

villet <- table_pro(tbl01x, 5)
villet

skade <- table_pro(tbl01x, 6)
skade

ukjent <- table_pro(tbl01x, 8)
ukjent


## Function testing
ulykke <- tbl01x[3]
ulykke <- stringi::stri_replace_all_regex(ulykke, "[^,|\\d+]", " ") #delete all other than , or digits
ulykke <- stringi::stri_replace_all_fixed(ulykke, ",", ".")
ulykke <- stringi::stri_split_fixed( trimws(ulykke), " " )
ulykke <- unlist(ulykke)
ulno <- length(ulykke)
ulno
ulTot <- paste0(ulykke[ulno -1], ulykke[ulno])
ulTot
ulykkePro <- ulykke[1:(ulno - 2)]
ulykkePro


## Tabell 15
## -------------------------------------------------------------
tbl15 <- txtpdf[grep("Tabell 15: Veitrafikkulykke", txtpdf)]
tblVei <- stringi::stri_split(tbl15, fixed = "\n")

get_tbl_value <- function(tbl, row){
  tblx <- tbl[[1]][row]
  tblx <- trimws(tblx)
  tblx <- unlist(stri_split(tblx, fixed = " "))
  tblx[tblx!=""]
}

agp <- get_tbl_value(tblVei, 35)
agp

# \s* - zero or more whitespaces
# (\([^()]*(?:(?1)[^()]*)*\)) - Capture group 1 means:
# \( - a "(" char
# [^()]* - zero or more chars other than "(" and ")"
# (?:(?1)[^()]*)* - zero or more occurences of the whole Group 1 pattern ie.
#    (?1) is a regex subroutine recursing Group 1 pattern) and then zero or more chars
#    other than "(" and ")"
# "\)" - a ")" char

agpVal <- tblVei[[1]][36]
agpVal
agpVal <- stringi::stri_replace_all(agpVal, "", fixed = " ")
agpVal <- stringi::stri_replace_all_regex(agpVal, pattern = "\\s*\\([^\\)]+\\)", replacement = ";")
agpVal <- stringi::stri_split_fixed(agpVal, pattern = ";")
agpVal <- as.numeric( unlist(agpVal) )
agpVal <- setNames(agpVal, agp)
agpVal

agpDT <- data.table(age = agp[-6], val = agpVal[-6])
agpDT[, id := 1:.N]
agpDT


### Population data
kh_load(PxWebApiData)

meta <- ApiData("http://data.ssb.no/api/v0/en/table/07459", returnMetaData = TRUE)
str(meta)
names(meta)

# Region = TRUE for alll regions else should be char
befolk <- ApiData("http://data.ssb.no/api/v0/en/table/07459",
                  Region = "0",
                  Alder = TRUE,
                  Kjonn = TRUE,
                  Tid = 3i)
str(befolk)
dt <- befolk[[1]]
setDT(dt)
dt <- dt[year %chin% "2021"]
dt[, .N, by=region]
dt[, .N, by=age]
dt[, alder := as.numeric( gsub("\\D+", "\\1", age) )]
dt[, id := fcase(alder <20, 1,
                 alder %in% 20:39, 2,
                 alder %in% 40:59, 3,
                 alder %in% 60:79, 4,
                 alder >79, 5)]

     dt
     dt[alder == 80]
     setkey(dt, id)
     dt[, ageVal := sum(value, na.rm = T), by = id]
     dd <- dt[!duplicated(id), .(id, ageVal)]


     DT <- agpDT[dd, on = "id"]
     DT[, pros := (val / ageVal)*10000]
     DT
