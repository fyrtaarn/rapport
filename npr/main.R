## Function

age_group <- function(per = 100){

  source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
  kh_load(pdftools, data.table, stringi)

  rootPath <- "c:/Users/ybka/Git-fhi/rapport/npr"

  txtpdf <- pdftools::pdf_text(file.path(rootPath, "Aarsrapport_personskadedata_2021.pdf" ))

  tbl15 <- txtpdf[grep("Tabell 15: Veitrafikkulykke", txtpdf)]
  tblVei <- stringi::stri_split(tbl15, fixed = "\n")

  agp <- get_tbl_value(tblVei, 35)

  agpVal <- tblVei[[1]][36]
  agpVal <- stringi::stri_replace_all(agpVal, "", fixed = " ")
  agpVal <- stringi::stri_replace_all_regex(agpVal, pattern = "\\s*\\([^\\)]+\\)", replacement = ";")
  agpVal <- stringi::stri_split_fixed(agpVal, pattern = ";")
  agpVal <- as.numeric( unlist(agpVal) )
  agpVal <- setNames(agpVal, agp)

  agpDT <- data.table(age = agp[-6], case = agpVal[-6])
  agpDT[, id := 1:.N]


  ### Population data
  kh_load(PxWebApiData)
  meta <- ApiData("http://data.ssb.no/api/v0/en/table/07459", returnMetaData = TRUE)

  # Region = TRUE for alll regions else should be char
  befolk <- ApiData("http://data.ssb.no/api/v0/en/table/07459",
                    Region = "0",
                    Alder = TRUE,
                    Kjonn = TRUE,
                    Tid = 3i)
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

  setkey(dt, id)
  dt[, pop := sum(value, na.rm = T), by = id]
  dd <- dt[!duplicated(id), .(id, pop)]


  DT <- agpDT[dd, on = "id"]
  DT[, pros := (case / pop)*per]
  DT[, id := NULL]

  message("Insidens per ", per)
  DT[]
}

get_tbl_value <- function(tbl, row){
  tblx <- tbl[[1]][row]
  tblx <- trimws(tblx)
  tblx <- unlist(stri_split(tblx, fixed = " "))
  tblx[tblx!=""]
  }
