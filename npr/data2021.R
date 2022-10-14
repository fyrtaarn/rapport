# Extract data from annual report 2021
# ------------------------------------

source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_load(pdftools, data.table, stringi)

txtpdf <- pdftools::pdf_text("Aarsrapport_personskadedata_2021.pdf")

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
dt
dt[, .N, by=region]
dt[, .N, by=age]
dt[, alder := as.numeric( gsub("\\D+", "\\1", age) )]
dt[, agp := fcase(alder <20, 1,
                  alder %in% 20:39, 2,
                  alder %in% 40:59, 3,
                  alder %in% 60:79, 4,
                  alder >79, 5)]

dt[, .N, keyby=agp]
dt[agp == 1]
