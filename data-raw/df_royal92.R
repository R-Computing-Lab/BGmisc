# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)

date_qualifier_regex <- "\\b(?:A[BF]T|BE[TF])\\b\\s*"
text_cleanup_regex <- c(
  "/|\\(twin\\)" = "",
  "_" = " ",
  "\\s*-\\s*" = "-"
)

is_approximated_date <- function(x) {
  str_detect(x, date_qualifier_regex) | str_length(x) == 4
}

strip_date_qualifier <- function(x) {
  x %>%
    str_replace_all(date_qualifier_regex, "") %>%
    str_squish()
}

standardize_partial_date <- function(x) {
  case_when(
    str_length(x) == 0 ~ NA_character_,
    str_length(x) == 4 ~ paste0("15 JUN ", x),
    TRUE ~ x
  )
}

parse_gedcom_date <- function(x) {
  x %>%
    str_trim() %>%
    as.Date(format = "%d %b %Y")
}


## Create dataframe
royal92 <- df_raw <- readGedcom("data-raw/royal92.ged") %>%
  addPersonToPed(
    personID = 1147,
    name = "Henry de Montfort",
    sex = "M",
    momID = 1370,
    dadID = 873,
    overwrite = TRUE
  ) %>%
  addPersonToPed(
    personID = 3011,
    name = "Simon de Montfort the Younger",
    sex = "M",
    momID = 1370,
    dadID = 873
  ) %>%
  addPersonToPed(
    personID = 2848,
    name = "Michael of Greece and Denmark",
    sex = "M",
    momID = 2846,
    dadID = 465,
    overwrite = TRUE
  )

#----
# Create overrides for dates and names based on historical records and data cleaning needs
#----

date_overrides <- tribble(
  ~personID, ~birth_date_override, ~death_date_override,
  24, "25 FEB 1883", "3 JAN 1981", # Alice of Athlone
  41, "26 NOV 1847", "13 OCT 1928", # Dagmar (Marie) of Denmark
  52, "21 APR 1926", "8 SEP 2022", # Elizabeth II Alexandra Mary Windsor
  151, "11 MAY 1857", "17 FEB 1905", # Serge Alexandrovich Romanov
  152, "3 OCT 1860", "28 JAN 1919", # Paul Alexandrovich Romanov
  153, "9 MAY 1871", "10 JUL 1899", # George Alexandrovich Romanov
  166, "15 JUL 1895", "26 FEB 1970", # Irina
  167, "23 MAR 1887", "27 SEP 1967", # Felix Yussoupov
  182, "21 JUL 1824", "16 FEB 1904", # Claude Bowes-Lyon
  220, "27 MAR 1819", "27 MAR 1819", # Charlotte Augusta Louisa Hanover
  282, "11 OCT 1895", "16 APR 1981", # George of Cambridge
  289, "23 JAN 1906", "29 MAY 1994", # May Cambridge
  369, "2 OCT 1908", "6 JUL 1993", # Ruth Sylvia Gill
  393, "15 JUN 1843", "30 JUN 1898", # Barbara Smith Marr
  420, "6 MAY 1882", "20 JUL 1951", # William
  421, "7 JUL 1883", "8 DEC 1942", # Eitel Frederick
  447, "7 JUN 1907", "4 FEB 2002", # Sigvard Oscar Fredrik
  520, "9 NOV 1907", "26 SEP 1994", # Louis Ferdinand of Prussia
  659, "30 APR 1909", "20 MAR 2004", # Juliana of Netherlands
  691, "3 MAY 1905", "8 JUL 1996", # Albrecht (Albert)
  812, "16 JUN 1908", "11 DEC 1996", # Marian Louisa Montagu-Douglas-Scott
  897, "9 OCT 1757", "6 NOV 1836", # Charles X
  953, "23 SEP 1893", "16 FEB 1992", # Charles of Southesk
  1097, "5 JAN 1909", "21 JAN 1991", # Ileana Hohenzollern
  1121, "4 AUG 1906", "27 JAN 2001", # Marie Jose
  1147, "15 NOV 1238", "4 AUG 1265", # Henry de Montfort
  1149, "7 OCT 1816", "12 APR 1817", # Theodolinde
  1298, "27 APR 1779", "15 JUN 1831", # Konstantin Romanov
  1304, "29 DEC 1709", "5 JAN 1762", # Elizabeth Petrovna Romanov
  1358, "5 DEC 1905", "27 DEC 1981", # Natalie Romanov
  1409, "8 MAY 1909", "21 DEC 2004", # Lennart Gustaf Nicholas
  1562, "24 JAN 1897", "8 MAY 1981", # Andrew
  1563, "23 DEC 1898", "30 NOV 1968", # Theodore
  1564, "17 JAN 1900", "12 SEP 1974", # Nikita
  1565, "15 AUG 1901", "7 JUL 1980", # Dimitri
  1566, "24 NOV 1902", "31 JUL 1978", # Rostislav
  1567, "7 JUL 1907", "24 JUN 1989", # Vassily
  1573, "10 MAY 1883", "28 MAY 1957", # Alexandra Zarnekau
  1581, "3 NOV 1890", "29 SEP 1978", # Serge Obelensky
  1668, "15 JUN 1893", "15 JUN 1978", # Jorgen Castenskiold
  2456, "15 JUN 1070", "14 FEB 1117", # Bertrada de Montfort
  2509, "6 AUG 1775", "3 JUN 1844", # of Angouleme
  2510, "24 JAN 1778", "14 FEB 1820", # of Berry
  2846, "25 DEC 1902", "25 FEB 1953", # Françoise of Orléans
  2848, "7 JAN 1939", "28 JUL 2024", # Michael of Greece and Denmark
  2939, "17 JUL 1838", "14 NOV 1886", # Harriet Marsham
  2941, "12 JUN 1811", "5 JUN 1846", # Margaret-Scott Montagu-Douglas-
  2942, "24 MAY 1772", "20 APR 1819", # Charles of Buccleuch Montagu-Douglas
  2948, "2 MAY 1841", "22 NOV 1906", # Henry Robert Brand
  2950, "2 SEP 1746", "11 JAN 1812", # Henry of Buccleuch Scott
  2955, "16 JUN 1803", "22 JUL 1844", # Anne Amelia Keppel
  2985, "26 APR 1924", "14 DEC 1997", # Gerald Legge
  3011, "15 APR 1240", "15 JUN 1271" # Simon de Montfort the Younger
)


name_overrides <- tribble(
  ~personID, ~name_override,
  12, "Alexandra of Denmark (Alix)",
  27, "Victoria Eugenie (Ena)",
  39, "Alexandra Fedorovna (Alix)",
  41, "Dagmar (Marie) of Denmark",
  84, "Elizabeth (Ella)",
  85, "Mary (May)",
  136, "Mary Adelaide (Fat Mary)",
  155, "Michael (Mischa) Alexandrovich Romanov",
  220, "Charlotte Augusta Louisa Hanover",
  345, "Frederick William of Schleswig-Holstein-Sonderburg-Glücksburg",
  354, "Elizabeth Alexandra of Saxe-Altenburg",
  402, "Augusta Victoria of Schleswig-Holstein-Sonderburg-Augustenburg",
  549, "Alexandra Victoria of Schleswig-Holstein-Sonderburg-Glücksburg",
  760, "Louise Eleonore of Hohenlohe-Langenburg",
  785, "Richard Curzon-Howe",
  788, "James Hamilton",
  812, "Marian Louisa Montagu-Douglas-Scott",
  1137, "Augusta Wilhelmine of Hesse-Darmstadt",
  1176, "Sophia Louise of Mecklenburg-Schwerin",
  1212, "Gösta von dem Bussche-Haddenhausen",
  1213, "Frederick Francis II of Mecklenburg-Schwerin",
  1419, "Charles Frederick of Schleswig-Holstein-Gottorp",
  1197, "Karl Theodor (Gackl)",
  1200, "Sophie Charlotte Auguste",
  1442, "Ferdinand Philippe Marie d'Orléans",
  1594, "John IV (the Conqueror) of Montfort",
  1644, "Sophia Frederica of Mecklenburg-Schwerin",
  1654, "Frederick Christian of Schleswig-Holstein-Sonderburg-Augustenburg",
  1673, "Richard of Sayn-Wittgenstein-Berleburg",
  1694, "John Frederick of Brandenburg-Ansbach",
  1709, "Henry Somerset",
  2509, "Louis Antoine of Angouleme",
  2510, "Charles Ferdinand of Berry",
  2846, "Françoise of Orléans",
  2851, "Ernest Frederick III of Saxe-Hildburghausen",
  2941, "Margaret Scott-Montagu-Douglas",
  2943, "Walter Scott-Montagu-Douglas, Duke of Buccleuch",
  2944, "William Scott of Buccleuch Montagu-Douglas",
  2946, "Herbert Montagu Douglas Scott",
  2948, "Henry Robert Brand",
  2955, "Anne Amelia Keppel",
  2990, "William Legge",
  2991, "Rupert Legge",
  2992, "Charlotte Legge",
  2993, "Henry Legge"
)


royal92 <- ped2fam(royal92, personID = "personID") %>%
  select(
    -death_place, -birth_place,
    -name_given,
    -name_surn,
    -FAMC,
    -FAMS
  ) %>%
  left_join(date_overrides, by = "personID") %>%
  mutate(
    birth_date = coalesce(birth_date_override, birth_date),
    death_date = coalesce(death_date_override, death_date)
  ) %>%
  select(-birth_date_override, -death_date_override) %>%
  left_join(name_overrides, by = "personID") %>%
  mutate(name = coalesce(name_override, name)) %>%
  select(-name_override)

royal92_cleaned <- royal92 %>%
  mutate(
    momID = as.numeric(momID),
    dadID = as.numeric(dadID),
    approximated_dob = is_approximated_date(birth_date),
    approximated_dod = is_approximated_date(death_date),
    birth_date = strip_date_qualifier(birth_date),
    death_date = strip_date_qualifier(death_date),
    # if only year is given, assign 15th June as the date
    birth_date = parse_gedcom_date(standardize_partial_date(birth_date)),
    death_date = parse_gedcom_date(standardize_partial_date(death_date)),
    attribute_title = case_when(
      personID == 2943 ~ "Duke of Buccleuch",
      personID == 146 ~ "Countess of Strathmore and Kinghorne",
      TRUE ~ attribute_title
    ),
    twinID = case_when(
      personID == 223 ~ 222,
      personID == 222 ~ 223,
      personID == 1116 ~ 1117,
      personID == 1117 ~ 1116,
      personID == 1155 ~ 1156,
      personID == 1156 ~ 1155,
      TRUE ~ NA_real_
    ),
    attribute_title = str_replace_all(attribute_title, text_cleanup_regex) %>%
      str_squish(),
    sex = case_when(
      personID %in% c(
        1098,
        1753,
        1755,
        1756,
        1803,
        2033,
        2509,
        2990,
        2991,
        2993
      ) ~ "M",
      personID %in% c(
        1149,
        2992
      ) ~ "F",
      TRUE ~ sex
    ),
    name = str_replace_all(name, text_cleanup_regex) %>%
      str_squish()
  )


royal92 <- royal92_cleaned %>%
  select(-approximated_dob, -approximated_dod)

checkis_acyclic <- checkPedigreeNetwork(royal92,
  personID = "personID",
  momID = "momID",
  dadID = "dadID",
  verbose = TRUE
)
checkis_acyclic
if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(royal92, here("data-raw", "royal92.csv"))
  usethis::use_data(royal92, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
