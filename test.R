library(googlesheets)
suppressMessages(library(dplyr))

my_sheets <- gs_ls()
# ti <- gs_title("Test ... Euro2016")
gs_gap() %>% gs_copy(to = "Gapminder")
gap <- gs_title("Gapminder")

# Need to access a sheet you do not own but you have a sharing link?
# Access it by URL!
(GAP_URL <- gs_gap_url())
third_party_gap <- GAP_URL %>%
  gs_url()

?gs_read()
##- url : https://docs.google.com/spreadsheets/d/1VvRgsESFmGAdaQ4Il0H4de1A2KjwwMUcipUoeHAF-uU/edit#gid=0

gs <- gs_title("Copy of Euro 2016 DIDE")
# gs_browse(gs)
tab <- gs_read(gs, range = cell_rows(1:37))
?gs_read
str(tab)
head(tab)
names(tab)
tab[2,5]
scores <- as.vector(tab[,4])
Joe <- as.vector(tab[,5])
##- compare scores vs Joe
?split
class(a)
class(scores)
a <- unlist(apply(scores,2, function(x) strsplit(x, "-")))
a <- apply(a[[1]],1, as.numeric )
matrix(as.numeric(a), ncol = 2)
class(sapply(tab[,4], function(x) strsplit(x, "-")) )

##- extract vector
tab %>% select(Stephane) %>% collect %>% .[["Stephane"]]
##- or
collect(select(tab, Stephane))[[1]]
