require(data.table)
require(readxl)
require(xml2)

# Directory creation
d <- "quiz1"
if (!dir.exists(d)) {
    dir.create(d)
}

#Q1

q1csvurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
q1pdfurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
q1csv <- file.path(d, "q1.csv")
q1pdf <- file.path(d, "q1.pdf")
if (!file.exists(q1csv)) {
    download.file(q1csvurl, q1csv, method = "curl")
}
if (!file.exists(q1pdf)) {
    download.file(q1pdfurl, q1pdf, method = "curl")
}
t <- fread(q1csv)
s <- sum(t$VAL[!is.na(t$VAL)] == 24)
print(sprintf("q1. properties over 1mil : %d", s))

#Q3
q3xlsurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
q3xls <- file.path(d, "q3.xlsx")
if (!file.exists(q3xls)) {
    download.file(q3xlsurl, q3xls, method = "curl")
}
t <- read_excel(q3xls)
dat <- t[18:23, 7:15]
print(sprintf("q2. val = %d", sum(dat$Zip*dat$Ext,na.rm=T)))

#Q4
q4xmlurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
q4xml <- file.path(d, "q4.xml")
if (!file.exists(q4xml)) {
    download.file(q4xmlurl, q4xml, method = "curl")
}
t <- read_xml(q4xml)
t <- xml_find_all(t, ".//zipcode")
t <- as.numeric(xml_text(t))
print(sprintf("q3. num = %d", sum(t == 21231)))