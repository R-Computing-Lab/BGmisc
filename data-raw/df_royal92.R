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
    personID = 914, # overwriting duplicate
    name = "Elizabeth Alexandrovna of Russia",
    sex = "F",
    momID = 1297,
    dadID = 1296,
    overwrite = TRUE
  ) %>%
  addPersonToPed(
    personID = 915,
    name = "Ferdinand I of Bulgaria",
    sex = "M",
    momID = 1043,
    dadID = 911,
    overwrite = TRUE
  ) %>%
  addPersonToPed(
    personID = 1147,
    name = "Henry de Montfort",
    sex = "M",
    momID = 1370,
    dadID = 873,
    overwrite = TRUE
  ) %>%
  addPersonToPed(
    personID = 2848,
    name = "Michael of Greece and Denmark",
    sex = "M",
    momID = 2846,
    dadID = 465,
    overwrite = TRUE
  ) %>%
  addPersonToPed(
    personID = 1298,
    name = "Konstantin Pavlovich Romanov",
    sex = "M",
    momID = 1295,
    dadID = 1294,
    overwrite = TRUE
  )  %>%
  addPersonToPed(
    personID = 3011,
    name = "Simon de Montfort the Younger",
    sex = "M",
    momID = 1370,
    dadID = 873
  )


#----
# Create overrides for dates and names based on historical records and data cleaning needs
#----

date_overrides <- tribble(
  ~personID, ~birth_date_override, ~death_date_override,
  19, "14 JAN 1831", "12 MAY 1893", # George Victor of Waldeck
  22, "12 SEP 1837", "13 MAR 1892", # Louis IV of Hesse
  24, "25 FEB 1883", "3 JAN 1981", # Alice of Athlone
  25, "5 OCT 1858", "20 JAN 1896", # Henry Maurice of Battenberg
  26, "23 NOV 1886", "23 FEB 1960", # Alexander of Carisbrooke
  27, "24 OCT 1887", "15 APR 1969", # Victoria Eugenie (Ena)
  28, "21 MAY 1889", "23 APR 1922", # Leopold Mountbatten
  29, "3 OCT 1891", "27 OCT 1914", # Maurice of Battenberg
  34, "31 MAR 1900", "10 JUN 1974", # Henry William Frederick Windsor
  38, "5 APR 1863", "24 SEP 1950", # Victoria Alberta of Hesse
  40, "10 MAR 1845", "1 NOV 1894", # Alexander III Alexandrovich Romanov, Gregorian/New Style; Old Style = 26 FEB 1845, 20 OCT 1894
  41, "26 NOV 1847", "13 OCT 1928", # Dagmar (Marie) of Denmark
  42, "6 JUL 1796", "2 MAR 1855", # Nicholas I Romanov, Gregorian/New Style; Old Style = 25 JUN 1796, 18 FEB 1855
  43, "13 JUL 1798", "1 NOV 1860", # Charlotte of Prussia
  44, "29 APR 1818", "13 MAR 1881", # Alexander II Nicholoevich Romanov, Gregorian/New Style; Old Style = 17 APR 1818, 1 MAR 1881
  45, "8 AUG 1824", "3 JUN 1880", # Marie of Hesse-Darmstadt

  51, "4 AUG 1900", "30 MAR 2002", #Elizabeth Angela Marguerite Bowes-Lyon
  52, "21 APR 1926", "8 SEP 2022", # Elizabeth II Alexandra Mary Windsor
  53, "21 AUG 1930", "9 FEB 2002", # Margaret Rose Windsor
  54, "7 APR 1930", "13 JAN 2017", # Antony Armstrong-Jones
  57, "10 JUN 1921", "9 APR 2021", #Philip Mountbatten
  65, "1 JUL 1961", "31 AUG 1997", # Diana Frances Spencer
  66, "13 DEC 1906", "27 AUG 1968", # Marina of Greece, Gregorian/New Style; Old Style birth = 30 NOV 1906
  68, "9 SEP 1882", "24 MAY 1947", # Henry George Charles Lascelles
  69, "25 DEC 1901", "29 OCT 2004", # Alice Christabel Montagu-Douglas-Scott, Duchess of Gloucester
  70, "19 JUN 1896", "24 APR 1986", # Bessie Wallis Warfield
  71, "3 AUG 1872", "21 SEP 1957", # Charles Haakon VII
  72, "14 AUG 1862", "20 APR 1929", # Henry of Prussia
  73, "15 SEP 1864", "18 JUN 1866", # Sigismund of Prussia
  74, "12 APR 1866", "13 NOV 1929", # Victoria of Prussia
  75, "10 FEB 1868", "27 MAR 1879", # Waldemar of Prussia
  77, "24 JUL 1860", "1 OCT 1919", # Charlotte of Prussia, Duchess of Saxe-Meiningen
  78, "22 APR 1872", "22 JAN 1954", # Margarete of Hesse
  79, "11 JUL 1866", "11 NOV 1953", # Irene of Hesse
  80, "20 MAR 1889", "2 MAY 1945", # Waldemar
  81, "9 JAN 1900", "26 FEB 1904", # Prince Henry
  82, "27 NOV 1896", "14 NOV 1978", # Sigismund of Prussia
  83, "25 NOV 1868", "9 OCT 1937", # Ernest Louis of Hesse
  84, "1 NOV 1864", "18 JUL 1918", # Elizabeth (Ella)
  85, "24 MAY 1874", "16 NOV 1878", # Mary (May) of Hesse
  86, "7 OCT 1870", "29 MAY 1873", # Frederick of Hesse and by Rhine
  89, "20 JUN 1946", NA_character_, # Birgitte of Denmark von Deurs; exact birth date, living
  91, "20 SEP 1888", "29 MAY 1950", # Earl Winfield Spencer Jr.
  92, "6 MAY 1897", "30 NOV 1958", # Ernest Aldrich Simpson
  93, "20 JAN 1936", "3 JUN 2004", # Frances Burke Roche / Frances Shand Kydd
  95, "15 OCT 1874", "6 FEB 1899", # Alfred, Hereditary Prince of Saxe-Coburg and Gotha
  97, "25 NOV 1876", "2 MAR 1936", # Victoria Melita of Edinburgh
  98, "1 SEP 1878", "16 APR 1942", # Alexandra of Saxe-Coburg and Gotha
  99, "20 APR 1884", "13 JUL 1966", # Beatrice of Saxe-Coburg and Gotha
  100, "24 MAY 1854", "11 SEP 1921", # Louis of Battenberg
  101, "25 FEB 1885", "5 DEC 1969", # Alice of Battenberg
  102, "6 NOV 1892", "8 APR 1938", # George Mountbatten
  103, "25 JUN 1900", "27 AUG 1979", # Louis Mountbatten of Burma
  104, "2 FEB 1882", "3 DEC 1944", # Andrew of Greece
  105, "10 NOV 1849", "29 JAN 1912", # Alexander Duff
  108, "14 SEP 1928", "26 DEC 2004", # Angus Ogilvy
  111, "22 FEB 1933", "4 SEP 2025", # Katharine Worsley, Duchess of Kent
  117, "22 JAN 1831", "28 OCT 1917", # Frederick Christian Charles of Schleswig-Holstein
  118, "12 AUG 1872", "8 DEC 1956", # Marie Louise of Schleswig-Holstein
  119, "18 JUN 1866", "24 DEC 1933", # Aribert of Anhalt
  122, "13 JAN 1883", "12 SEP 1938", # Arthur of Connaught
  123, "17 MAR 1886", "12 JAN 1974", # Patricia of Connaught
  125, "17 MAY 1891", "26 FEB 1959", # Alexandra, 2nd Duchess of Fife
  126, "29 MAY 1881", "8 OCT 1972", # Alexander Ramsay
  127, "1295", "22 AUG 1358", # Isabella of France; birth year uncertain, sources vary ca. 1292/1295/1296; death date varies by one day, 22 AUG vs 23 AUG 1358
  129, "19 JUL 1884", "6 MAR 1954", # Charles Edward, Duke of Saxe-Coburg and Gotha
  132, "24 FEB 1774", "8 JUL 1850", # Adolphus, Duke of Cambridge
  134, "25 JUL 1797", "6 APR 1889", # Augusta of Hesse-Kassel
  135, "19 JUL 1822", "5 DEC 1916", # Augusta Caroline of Cambridge
  136, "27 NOV 1833", "27 OCT 1897", # Mary Adelaide of Cambridge
  137, "28 AUG 1837", "21 JAN 1900", # Francis, Duke of Teck
  139, "2 JAN 1784", "29 JAN 1844", # Ernest I of Saxe-Coburg and Gotha
  140, "21 DEC 1800", "30 AUG 1831", # Louise of Saxe-Gotha-Altenburg
  142, "26 JUL 1756", "27 MAR 1837", # Maria Anne Fitzherbert
  143, "17 MAY 1768", "7 AUG 1821", # Caroline Amelia of Brunswick
  145, "14 MAR 1855", "7 NOV 1944", # Claude George Bowes-Lyon
  146, "11 SEP 1862", "23 JUN 1938", # Cecilia Nina Cavendish-Bentinck
  147, "18 APR 1905", "24 APR 1981", # Margarita of Greece and Denmark
  148, "30 MAY 1906", "16 OCT 1969", # Theodora of Greece and Denmark
  149, "22 APR 1847", "17 FEB 1909", # Vladimir Alexandrovich Romanov, Gregorian/New Style; Old Style birth = 10 APR 1847
  150, "14 JAN 1850", "27 NOV 1908", # Alexei Alexandrovich Romanov, Gregorian/New Style; Old Style = 2 JAN 1850, 14 NOV 1908
  151, "11 MAY 1857", "17 FEB 1905", # Serge Alexandrovich Romanov
  152, "3 OCT 1860", "28 JAN 1919", # Paul Alexandrovich Romanov
  153, "9 MAY 1871", "10 JUL 1899", # George Alexandrovich Romanov
  154, "6 APR 1875", "20 APR 1960", # Xenia Alexandrovna Romanov, Gregorian/New Style; Old Style birth = 25 MAR 1875
  155, "4 DEC 1878", "13 JUN 1918", # Michael (Mischa) Alexandrovich Romanov, New Style
  156, "13 JUN 1882", "24 NOV 1960", # Olga Alexandrovna Romanov, Gregorian/New Style; Old Style birth = 1 JUN 1882
  157, "14 MAY 1854", "6 SEP 1920", # Maria Pavlovna the Elder, Gregorian/New Style; Old Style birth = 2 MAY 1854
  158, "12 OCT 1876", "12 OCT 1938", # Kirill Vladimirovich Romanov, Gregorian/New Style; Old Style birth = 30 SEP 1876
  159, "24 NOV 1877", "9 NOV 1943", # Boris Vladimirovich Romanov, Gregorian/New Style
  160, "14 MAY 1879", "30 OCT 1956", # Andrei Vladimirovich Romanov, Gregorian/New Style; Old Style birth = 2 MAY 1879
  161, "31 AUG 1872", "6 DEC 1971", # Mathilde Kschessinska, Gregorian/New Style; Old Style birth = 19 AUG 1872
  162, "3 AUG 1770", "7 JUN 1840", # Frederick William III of Prussia
  163, "30 AUG 1870", "24 SEP 1891", # Alexandra of Greece and Denmark
  164, "18 SEP 1891", "5 MAR 1942", # Dmitri Pavlovich Romanov, Gregorian/New Style
  165, "14 FEB 1850", "26 JAN 1918", # Nicholas Konstantinovich Romanov, Gregorian/New Style
  166, "15 JUL 1895", "26 FEB 1970", # Irina
  167, "23 MAR 1887", "27 SEP 1967", # Felix Yussoupov
  169, "10 OCT 1931", "16 MAR 2003", # Ronald Ivor Ferguson
  170, "9 JUN 1937", "19 SEP 1998", # Susan Mary Wright / Susan Barrantes
  173, "17 APR 1882", "17 OCT 1893", # Violet Hyacinth Bowes-Lyon
  174, "30 AUG 1883", "8 FEB 1961", # Mary Frances Bowes-Lyon / Lady Elphinstone
  175, "22 SEP 1884", "25 MAY 1949", # Patrick Bowes-Lyon, 15th Earl of Strathmore and Kinghorne
  176, "1 APR 1886", "7 FEB 1930", # John Herbert Bowes-Lyon
  177, "14 APR 1887", "19 OCT 1911", # Alexander Francis Bowes-Lyon
  178, "18 APR 1889", "27 SEP 1915", # Fergus Bowes-Lyon; some sources give 26 SEP 1915
  179, "6 MAY 1890", "17 NOV 1967", # Rose Constance Bowes-Lyon / Countess Granville
  180, "1 OCT 1893", "1 MAY 1953", # Michael Claude Hamilton Bowes-Lyon
  181, "2 MAY 1902", "13 SEP 1961", # David Bowes-Lyon
  182, "21 JUL 1824", "16 FEB 1904", # Claude Bowes-Lyon
  183, "29 JUL 1832", "5 FEB 1922", # Frances Dora Smith / Countess of Strathmore and Kinghorne
  187, "17 JUL 1737", "7 MAR 1776", # John Lyon, 9th Earl of Strathmore and Kinghorne
  188, "24 FEB 1749", "28 APR 1800", # Mary Eleanor Bowes
  189, "14 APR 1769", "3 JUL 1820", # John Bowes, 10th Earl of Strathmore and Kinghorne
  191, "27 JUL 1869", "28 NOV 1955", # Sidney Elphinstone, 16th Lord Elphinstone
  192, "3 DEC 1888", "18 JUN 1946", # Dorothy Beatrix Godolphin Osborne
  195, "11 JUL 1880", "25 JUN 1953", # William Spencer Leveson-Gower, 4th Earl Granville
  198, "16 NOV 1528", "9 JUN 1572", # Jeanne d'Albret of France
  201, "6 AUG 1845", "2 MAY 1914", # John Campbell, 9th Duke of Argyll
  211, "30 JUL 1769", "2 APR 1829", # Frederick VI of Hesse-Homburg
  220, "27 MAR 1819", "27 MAR 1819", # Charlotte Augusta Louisa Hanover
  224, "15 FEB 1852", "20 JUN 1923", # Princess Marie of Battenberg / Princess of Erbach-Schönberg
  228, "3 SEP 1851", "18 JUN 1926", # Olga Constantinovna of Russia, Gregorian/New Style
  229, "22 JAN 1872", "8 FEB 1938", # Nicholas of Greece and Denmark, Gregorian/New Style
  230, "4 MAY 1913", "2 OCT 2007", # Princess Katherine of Greece and Denmark; row is currently labeled Child 6
  240, "19 MAR 1955", NA_character_, # Sarah Spencer / Lady Sarah McCorquodale; living
  241, "11 FEB 1957", NA_character_, # Jane Spencer / Lady Jane Fellowes; living
  242, "20 MAY 1964", NA_character_, # Charles Spencer, 9th Earl Spencer; living
  243, "9 SEP 1929", "21 OCT 2016", # Raine McCorquodale / Countess Spencer
  244, "7 FEB 1923", "11 JUL 2011", # George Lascelles, 7th Earl of Harewood
  246, "15 DEC 1719", "6 APR 1790", # Louis IX, Landgrave of Hesse-Darmstadt
  247, "6 NOV 1754", "30 OCT 1816", # Frederick I of Württemberg
  251, "21 SEP 1845", "14 NOV 1923", # Ernest Augustus, Crown Prince of Hanover
  252, "9 JAN 1848", "16 OCT 1926", # Frederica of Hanover
  253, "3 DEC 1849", "4 JUN 1904", # Mary of Hanover
  254, "29 SEP 1853", "26 FEB 1933", # Thyra of Denmark
  255, "11 OCT 1879", "31 JAN 1948", # Marie Louise of Hanover
  256, "28 OCT 1880", "20 MAY 1912", # George William of Hanover
  257, "29 SEP 1882", "30 AUG 1963", # Alexandra of Hanover
  258, "11 JUL 1884", "21 SEP 1958", # Olga of Hanover
  259, "4 JUL 1885", "3 SEP 1901", # Christian of Hanover
  260, "17 OCT 1894", "30 JUL 1962", # René of Bourbon-Parma
  261, "27 JUL 1843", "20 NOV 1932", # Alfons Pawel-Rammingen
  262, "26 MAR 1819", "17 MAR 1904", # George, Duke of Cambridge
  263, "31 OCT 1816", "12 JAN 1890", # Sarah Louisa Fairbrother
  264, "24 AUG 1843", "2 SEP 1907", # George FitzGeorge
  265, "30 JAN 1846", "17 DEC 1922", # Adolphus FitzGeorge
  266, "12 JUN 1847", "30 OCT 1933", # Augustus FitzGeorge
  271, "17 OCT 1819", "30 MAY 1904", # Frederick William, Grand Duke of Mecklenburg-Strelitz
  272, "22 JUL 1848", "11 JUN 1914", # Adolphus Frederick V, Grand Duke of Mecklenburg-Strelitz
  273, "7 SEP 1857", "20 JUL 1933", # Elisabeth of Anhalt
  278, "13 AUG 1868", "24 OCT 1927", # Adolphus Cambridge, 1st Marquess of Cambridge
  279, "9 JAN 1870", "22 OCT 1910", # Francis of Teck
  280, "14 APR 1874", "16 JAN 1957", # Alexander Cambridge, 1st Earl of Athlone
  281, "8 APR 1873", "27 MAR 1929", # Margaret Grosvenor / Marchioness of Cambridge
  282, "11 OCT 1895", "16 APR 1981", # George of Cambridge
  284, "12 JUN 1897", "23 JUN 1987", # Mary Cambridge / Duchess of Beaufort
  285, "23 OCT 1899", "22 DEC 1969", # Helena Cambridge
  287, "24 APR 1907", "15 APR 1928", # Rupert Cambridge, Viscount Trematon; note conflict: RoyalFamilyTree gives 24 AUG 1907
  289, "23 JAN 1906", "29 MAY 1994", # May Cambridge
  291, "21 AUG 1924", "27 FEB 1998", # Gerald David Lascelles
  292, "18 OCT 1926", "6 MAR 2014", # Marion Stein / Countess of Harewood
  293, "21 OCT 1950", NA_character_, # David Lascelles, 8th Earl of Harewood; living
  294, "5 OCT 1953", NA_character_, # James Edward Lascelles; living
  295, "14 FEB 1955", NA_character_, # Robert Jeremy Hugh Lascelles; living
  296, "20 APR 1919", "28 FEB 2007", # Angela Dowding / Angela Lascelles
  297, "19 MAY 1953", NA_character_, # Henry Ulick Lascelles; living
  298, "24 DEC 1787", "5 SEP 1867", # William of Hesse-Kassel
  299, "30 OCT 1789", "28 MAR 1864", # Louise Charlotte of Denmark
  301, "11 SEP 1747", "20 MAY 1837", # Frederick of Hesse-Cassel
  302, "4 APR 1762", "17 AUG 1823", # Caroline of Nassau-Usingen
  303, "9 SEP 1804", "4 JUL 1885", # Alexander of Wurttemberg
  304, "21 SEP 1812", "1 OCT 1841", # Claudine Rhédey
  305, "11 FEB 1836", "18 NOV 1894", # Claudine of Teck
  306, "12 NOV 1838", "20 JUL 1893", # Amelie of Teck
  307, "13 APR 1835", "13 APR 1897", # Paul von Hugel
  310, "14 APR 1867", "29 OCT 1900", # Christian Victor
  311, "26 FEB 1869", "27 APR 1931", # Albert of Schleswig-Holstein
  312, "3 MAY 1870", "13 MAR 1948", # Helena Victoria
  314, "24 NOV 1926", "4 MAY 2018", # Patricia Tuckwell
  315, "5 JUL 1964", NA_character_, # Mark Lascelles; living
  316, "23 APR 1924", "14 JAN 2006", # Elizabeth Collingwood Colvin
  322, "1 MAR 1683", "20 NOV 1737", # Caroline of Ansbach
  345, "4 JAN 1785", "17 FEB 1831", # Frederick William of Schleswig-Holstein-Sonderburg-Glücksburg
  346, "28 SEP 1789", "13 MAR 1867", # Louise Caroline of Hesse-Cassel
  347, "15 JUL 1823", "15 DEC 1888", # Alexander of Hesse and the Rhine
  348, "24 NOV 1825", "19 SEP 1895", # Julia of Battenberg von Hauke
  349, "26 DEC 1777", "16 JUN 1848", # Louis II of Hesse and the Rhine
  350, "21 SEP 1788", "27 JAN 1836", # Wilhelmina of Baden
  351, "26 OCT 1775", "29 NOV 1830", # John Maurice von Hauke
  352, "1790", "27 AUG 1831", # Sophie la Fontaine; approximate birth year
  353, "21 SEP 1827", "25 JAN 1892", # Constantine Nikolaievitch of Russia, Gregorian/New Style; Old Style = 9 SEP 1827, 13 JAN 1892
  354, "8 JUL 1830", "6 JUL 1911", # Elizabeth Alexandra of Saxe-Altenburg / Alexandra Iosifovna
  355, "27 AUG 1789", "25 NOV 1868", # Joseph of Saxe-Altenburg
  356, "28 JUN 1799", "28 NOV 1848", # Amalie of Wurttemberg
  357, "23 APR 1809", "20 MAR 1877", # Charles of Hesse
  358, "18 JUN 1815", "21 MAR 1885", # Elizabeth of Prussia
  359, "8 NOV 1817", "17 AUG 1865", # Charles William Frederick Cavendish-Bentinck
  360, "3 OCT 1780", "28 APR 1826", # William Charles Augustus Cavendish-Bentinck
  361, "1788", "19 MAR 1875", # Anne Wellesley; approximate birth year
  366, "23 MAY 1892", "9 JUN 1975", # Albert Edward John Spencer
  367, "16 AUG 1897", "4 DEC 1972", # Cynthia Elinor Beatrix Hamilton
  368, "15 MAY 1885", "8 JUL 1955", # Edmund Maurice Burke Roche
  369, "2 OCT 1908", "6 JUL 1993", # Ruth Sylvia Gill
  370, "28 JUL 1851", "30 OCT 1920", # James Boothby Burke Roche
  371, "27 OCT 1857", "26 JAN 1947", # Frances Ellen Work
  372, "10 FEB 1819", "16 MAR 1911", # Frank Work
  378, "9 AUG 1815", "17 SEP 1874", # Edmund Burke Roche
  379, "1821", "26 APR 1897", # Elizabeth Caroline Boothby; birth year only
  384, "30 NOV 1869", "12 SEP 1953", # James Hamilton, 3rd Duke of Abercorn
  385, "26 FEB 1869", "18 JAN 1958", # Rosalind Cecilia Caroline Bingham
  386, "24 AUG 1838", "3 JAN 1913", # James Hamilton, 2nd Duke of Abercorn
  387, "23 JUL 1848", "10 MAY 1929", # Mary Anna Curzon-Howe
  388, "8 MAY 1830", "5 JUN 1914", # George Bingham, 4th Earl of Lucan
  389, "13 APR 1838", "5 OCT 1910", # Cecilia Catherine Gordon-Lennox
  393, "15 JUN 1843", "30 JUN 1898", # Barbara Smith Marr
  396, "30 OCT 1857", "26 SEP 1922", # Charles Robert Spencer
  397, "14 DEC 1868", "4 JUL 1906", # Margaret Baring
  398, "14 APR 1798", "27 DEC 1857", # Frederick Spencer
  399, "27 JAN 1825", "29 OCT 1877", # Adelaide Horatia Elizabeth Seymour
  400, "13 APR 1828", "17 JUL 1897", # Edward Charles Baring
  401, "18 JUN 1839", "16 OCT 1892", # Louisa Emily Charlotte Bulteel
  403, "1 APR 1851", "16 JAN 1928", # Bernard of Saxe-Meiningen
  404, "20 JUL 1859", "9 JUL 1916", # Adolphus of Schaumburg-Lippe
  406, "1 MAY 1868", "28 MAY 1940", # Frederick Charles of Hesse
  408, "4 JUL 1890", "16 JUL 1956", # Irene Denison
  409, "17 MAY 1886", "28 FEB 1941", # Alfonso XIII
  410, "10 MAR 1776", "19 JUL 1810", # Louise of Mecklenburg-Strelitz
  413, "13 OCT 1799", "30 MAR 1800", # Frederica of Prussia
  414, "29 JUN 1801", "21 JAN 1883", # Charles of Prussia
  416, "13 DEC 1804", "1 APR 1806", # Ferdinand of Prussia
  417, "2 APR 748", "28 JAN 814", # Charlemagne
  418, "4 OCT 1809", "14 OCT 1872", # Albert of Prussia
  420, "6 MAY 1882", "20 JUL 1951", # William
  421, "7 JUL 1883", "8 DEC 1942", # Eitel Frederick
  422, "14 JUL 1884", "22 SEP 1948", # Adalbert of Prussia
  423, "29 JAN 1887", "25 MAR 1949", # Augustus William of Prussia
  424, "27 JUL 1888", "27 JAN 1958", # Oscar of Prussia
  425, "17 DEC 1890", "18 JUL 1920", # Joachim of Prussia
  426, "13 SEP 1892", "11 DEC 1980", # Victoria Louise of Prussia
  427, "30 SEP 1811", "7 JAN 1890", # Augusta of Saxe-Weimar
  428, "3 DEC 1838", "23 APR 1923", # Louise of Prussia / Grand Duchess of Baden
  429, "9 SEP 1826", "28 SEP 1907", # Frederick of Baden
  430, "10 MAY 1907", "6 SEP 1938", # Alphonso of Cavadonga
  431, "23 JUN 1908", "20 MAR 1975", # Don Jaime / Infante Jaime, Duke of Segovia
  432, "20 JUN 1913", "1 APR 1993", # Don Juan of Spain / Count of Barcelona
  433, "22 JUN 1909", "22 NOV 2002", # Beatrice of Spain
  434, "23 DEC 1910", "2 JAN 2000", # Maria de las Mercedes of Bourbon
  435, "5 JAN 1938", NA_character_, # Juan Carlos I; living
  436, "6 JAN 1900", "22 JUN 1961", # Marie (Mignon) Hohenzollern / Maria of Romania
  440, "16 DEC 1888", "9 OCT 1934", # Alexander I of Yugoslavia
  441, "2 NOV 1938", NA_character_, # Sophia of Greece / Queen Sofia of Spain; living
  442, "20 DEC 1963", NA_character_, # Helen / Infanta Elena; living
  443, "13 JUN 1965", NA_character_, # Christine / Infanta Cristina; living
  444, "30 JAN 1968", NA_character_, # Philip of Asturias / Felipe VI; living
  445, "22 APR 1906", "26 JAN 1947", # Gustav Adolf
  446, "20 APR 1889", "20 SEP 1918", # Erik of Vastmanland
  447, "7 JUN 1907", "4 FEB 2002", # Sigvard Oscar Fredrik
  448, "28 FEB 1912", "5 JAN 1997", # Bertil Gustaf Oscar
  449, "31 OCT 1916", "5 MAY 2012", # Carl Johan Arthur
  451, "28 MAR 1901", "5 APR 1954", # Martha of Sweden / Crown Princess Märtha of Norway
  453, "4 JUL 1937", NA_character_, # Sonja Haraldsen / Queen Sonja of Norway; living
  454, "22 SEP 1971", NA_character_, # Märtha Louise of Norway; living
  455, "20 JUL 1973", NA_character_, # Haakon Magnus of Norway; living
  460, "17 SEP 1871", "16 NOV 1937", # Eleonore of Solms-Hohensolms-Lich
  461, "8 NOV 1906", "16 NOV 1937", # Georg Donatus of Hesse
  462, "20 NOV 1908", "30 MAY 1968", # Louis of Hesse and by Rhine
  463, "22 JUN 1911", "16 NOV 1937", # Cecilie of Greece and Denmark
  464, "18 MAR 1913", "26 JAN 1997", # Margaret Campbell-Geddes
  465, "10 AUG 1888", "21 JAN 1940", # Christopher of Greece and Denmark, Gregorian/New Style; Old Style birth = 29 JUL 1888
  466, "24 JUN 1869", "25 NOV 1957", # George of Greece and Denmark
  467, "7 APR 1880", "2 NOV 1880", # Olga of Greece and Denmark
  474, "25 MAR 1921", "30 JAN 1993", # Alexandra of Greece / Queen of Yugoslavia
  475, "6 SEP 1923", "3 NOV 1970", # Peter II of Yugoslavia
  476, "26 JUN 1914", "24 NOV 2001", # Sophie of Greece and Denmark
  477, "24 MAR 1897", "11 MAY 1960", # Gottfried of Hohenlohe-Langenburg
  479, "24 FEB 1906", "27 OCT 1963", # Berthold, Margrave of Baden
  481, "14 MAY 1901", "7 OCT 1943", # Christoph of Hesse
  482, "25 MAR 1915", "8 JAN 2006", # George William of Hanover
  484, "30 AUG 1756", "20 SEP 1817", # Duke Ludwig of Württemberg
  485, "22 APR 1780", "2 JAN 1857", # Henriette of Nassau-Weilburg
  486, "5 APR 1857", "17 NOV 1893", # Alexander of Battenberg / Alexander I of Bulgaria
  487, "24 SEP 1861", "31 JUL 1924", # Francis Joseph of Battenberg
  488, "17 AUG 1840", "29 JAN 1908", # Gustav Ernst of Erbach-Schönberg
  489, "18 APR 1865", "20 JUL 1951", # Johanna Loisinger
  490, "18 AUG 1874", "22 APR 1971", # Anna of Montenegro
  491, "30 AUG 1842", "10 JUL 1849", # Alexandra Alexandrovna Romanov, Gregorian/New Style; Old Style = 18 AUG 1842, 28 JUN 1849
  492, "20 SEP 1843", "24 APR 1865", # Nicholas Alexandrovich Romanov, Gregorian/New Style; Old Style = 8 SEP 1843, 12 APR 1865
  493, "9 JUN 1806", "13 JUN 1877", # Louis III of Hesse
  494, "28 NOV 1901", "21 FEB 1960", # Edwina Ashley / Countess Mountbatten of Burma
  495, "30 AUG 1813", "25 MAY 1862", # Mathilde of Bavaria
  496, "20 AUG 1752", "22 MAY 1782", # Frederica of Hesse-Darmstadt
  497, "28 NOV 1838", "16 SEP 1900", # Henry of Hesse and by Rhine
  498, "16 NOV 1845", "24 MAY 1900", # William of Hesse and by Rhine
  499, "25 MAY 1843", "16 APR 1865", # Anna of Hesse and by Rhine
  500, "14 FEB 1924", "13 JUN 2017", # Patricia Mountbatten / Countess Mountbatten of Burma
  501, "19 APR 1929", NA_character_, # Pamela Mountbatten / Lady Pamela Hicks; living
  502, "28 MAR 1896", "22 JAN 1963", # Nadejda de Torby / Marchioness of Milford Haven
  503, "16 DEC 1917", "15 MAY 1988", # Tatiana Elizabeth Mountbatten
  504, "12 MAY 1919", "14 APR 1970", # David Mountbatten, 3rd Marquess of Milford Haven
  505, "9 NOV 1924", "23 SEP 2005", # John Ulick Knatchbull, 7th Baron Brabourne
  507, "25 MAR 1929", "29 MAR 1998", # David Nightingale Hicks
  509, "13 JAN 1920", "1 SEP 1982", # Iris Mountbatten
  510, "30 AUG 1800", "5 JUN 1873", # Auguste von Harrach
  513, "29 AUG 1820", "5 MAR 1879", # Rosalie von Rauch / Countess of Hohenau
  515, "15 SEP 1800", "7 MAR 1842", # Paul Frederick, Grand Duke of Mecklenburg-Schwerin
  516, "3 FEB 1808", "18 JAN 1877", # Marie of Saxe-Weimar-Eisenach
  517, "9 MAR 1721", "30 MAR 1774", # Caroline of Zweibrücken
  518, "20 SEP 1886", "6 MAY 1954", # Cecilie of Mecklenburg-Schwerin
  519, "4 JUL 1906", "26 MAY 1940", # Wilhelm of Prussia
  520, "9 NOV 1907", "26 SEP 1994", # Louis Ferdinand of Prussia
  521, "30 SEP 1909", "8 APR 1950", # Hubertus of Prussia
  522, "19 DEC 1911", "20 APR 1966", # Frederick of Prussia
  523, "7 APR 1915", "2 OCT 1980", # Alexandrine of Prussia
  524, "5 SEP 1917", "21 APR 1975", # Cecilie of Prussia
  525, "10 SEP 1907", "7 MAY 1972", # Dorothea von Salviati
  528, "9 MAY 1909", "8 SEP 1967", # Kira Kirillovna of Russia
  530, "24 JAN 1547", "11 APR 1578", # Joanna of Austria
  531, "25 MAR 1541", "19 OCT 1587", # Francesco I de' Medici, Grand Duke of Tuscany
  532, "1160", "12 FEB 1218", # Alice de Courtenay; approximate birth year
  533, "1160", "16 JUN 1202", # Aymer of Angoulême; approximate birth year
  537, "20 AUG 1920", "10 OCT 2009", # Magdalene Reuss
  540, "30 JUL 1920", "8 MAR 1995", # Brigid Guinness
  547, "2 FEB 1879", "29 MAR 1964", # Sophie Charlotte of Oldenburg
  548, "16 AUG 1891", "25 APR 1971", # Adelheid of Saxe-Meiningen
  549, "21 APR 1887", "15 APR 1957", # Alexandra Victoria of Schleswig-Holstein-Sonderburg-Glücksburg
  550, "27 JAN 1888", "17 SEP 1973", # Ina Marie von Bassewitz
  551, "10 JUN 1898", "22 MAY 1983", # Marie Auguste of Anhalt
  552, "17 NOV 1887", "30 JAN 1953", # Ernest Augustus of Brunswick
  557, "1 MAY 1772", "31 DEC 1773", # Christine of Prussia
  558, "5 NOV 1773", "28 DEC 1796", # Louis Charles of Prussia
  561, "1 MAY 1780", "19 FEB 1841", # Augusta of Prussia
  562, "30 DEC 1781", "12 JUL 1846", # Henry Charles of Prussia; row name currently Charles
  563, "3 JUL 1783", "28 SEP 1851", # William of Prussia
  564, "10 OCT 1741", "6 NOV 1816", # Charles II, Grand Duke of Mecklenburg-Strelitz
  566, "28 JUL 1777", "20 NOV 1847", # William II of Hesse
  568, "9 AUG 1722", "12 JUN 1758", # Augustus William of Prussia
  569, "29 JAN 1722", "13 JAN 1780", # Louise of Brunswick-Wolfenbüttel
  570, "30 DEC 1747", "26 MAY 1767", # Frederick Henry Charles of Prussia
  571, "7 AUG 1751", "9 JUN 1820", # Wilhelmina of Prussia
  572, "30 OCT 1758", "15 FEB 1759", # George Charles Emil of Prussia
  573, "8 MAR 1748", "9 APR 1806", # William V of Orange
  574, "16 JAN 1735", "28 NOV 1788", # Charles Christian of Nassau-Weilburg
  575, "28 FEB 1743", "6 MAY 1787", # Caroline of Orange-Nassau
  576, "25 OCT 1768", "9 JAN 1816", # Friedrich Wilhelm of Nassau-Weilburg
  578, "14 JUN 1792", "20 AUG 1839", # Wilhelm, Duke of Nassau
  580, "24 JUL 1817", "17 NOV 1905", # Adolphe of Luxembourg
  582, "22 APR 1852", "25 FEB 1912", # Guillaume IV of Luxembourg
  584, "23 JAN 1896", "9 JUL 1985", # Charlotte of Luxembourg
  586, "5 JAN 1921", "23 APR 2019", # Jean of Luxembourg
  589, "6 OCT 1914", "23 MAY 2010", # Leonide Bagration-Moukhransky, Gregorian/New Style; Old Style birth = 23 SEP 1914
  590, "24 APR 1608", "2 FEB 1660", # Gaston, Duke of Orléans
  591, "23 JUN 1908", "20 MAR 1975", # James / Jaime, Duke of Segovia
  592, "30 JUL 1936", "8 JAN 2020", # Dona Maria of Bourbon / Infanta Pilar of Spain
  593, "6 MAR 1939", NA_character_, # Margarita of Bourbon / Duchess of Soria; living
  594, "3 OCT 1941", "29 MAR 1956", # Alfonso of Bourbon / Infante Alfonso of Spain
  595, "1 NOV 1797", "30 MAR 1855", # Maria Dorothea of Württemberg
  597, "27 FEB 1861", "24 OCT 1951", # Charles of Sweden / Prince Carl, Duke of Västergötland
  598, "2 AUG 1878", "12 MAR 1958", # Ingeborg of Denmark
  601, "7 SEP 1930", "31 JUL 1993", # Baudouin I of the Belgians; death date completed
  602, "18 JAN 1908", "28 NOV 1972", # Sibylla of Saxe-Coburg and Gotha
  609, "10 JUL 1965", NA_character_, # Alexia of Greece and Denmark; living
  611, "17 NOV 1627", "7 AUG 1693", # John George II of Anhalt-Dessau
  613, "3 JUL 1676", "7 APR 1747", # Leopold I of Anhalt-Dessau
  614, "26 AUG 1666", "18 APR 1726", # Henriette Amalie of Anhalt-Dessau
  615, "18 JAN 1657", "25 MAR 1696", # Henry Casimir II of Nassau-Dietz
  616, "14 AUG 1687", "14 JUL 1711", # John William Friso
  617, "7 FEB 1688", "9 APR 1765", # Marie Louise of Hesse-Kassel
  618, "1 SEP 1711", "22 OCT 1751", # William IV of Orange
  619, "23 OCT 1710", "18 SEP 1777", # Anna Charlotte Amalia of Nassau-Dietz / Charlotte Amalia
  620, "7 OCT 1703", "26 MAR 1732", # Frederick, Hereditary Prince of Baden-Durlach
  621, "22 NOV 1728", "10 JUN 1811", # Charles Frederick, Grand Duke of Baden
  623, "14 FEB 1755", "16 DEC 1801", # Charles Louis, Hereditary Prince of Baden
  625, "8 JUL 1786", "8 DEC 1818", # Charles, Grand Duke of Baden
  628, "11 OCT 1817", "17 OCT 1888", # Marie Amelie of Baden
  629, "19 FEB 1811", "15 JUL 1863", # William Hamilton, 11th Duke of Hamilton
  630, "11 DEC 1850", "14 MAY 1922", # Mary Victoria Hamilton
  631, "13 NOV 1848", "26 JUN 1922", # Albert I of Monaco
  632, "12 JUL 1870", "9 MAY 1949", # Louis II of Monaco
  634, "30 SEP 1898", "16 NOV 1977", # Charlotte, Duchess of Valentinois; some sources give 15 NOV 1977
  635, "24 OCT 1895", "10 NOV 1964", # Pierre de Polignac
  636, "31 MAY 1923", "6 APR 2005", # Rainier III of Monaco
  638, "27 JAN 1805", "28 MAY 1872", # Sophie of Bavaria
  639, "17 DEC 1802", "8 MAR 1878", # Franz Karl of Austria
  640, "21 APR 1865", "1 NOV 1906", # Otto Franz of Austria
  641, "7 JUL 1878", "13 MAR 1960", # Elisabeth Amalie of Austria
  643, "17 AUG 1887", "1 APR 1922", # Karl I of Austria
  645, "20 NOV 1912", "4 JUL 2011", # Otto von Habsburg
  646, "30 JUL 1833", "19 MAY 1896", # Karl Ludwig of Austria
  648, "17 JUN 1869", "17 MAR 1955", # Aloys of Liechtenstein
  649, "16 AUG 1906", "13 NOV 1989", # Franz Joseph II of Liechtenstein
  650, "8 MAR 1748", "9 APR 1806", # William V of Orange
  651, "7 AUG 1751", "9 JUN 1820", # Wilhelmina of Prussia
  653, "28 FEB 1797", "8 SEP 1881", # Frederick of the Netherlands
  659, "30 APR 1909", "20 MAR 2004", # Juliana of Netherlands
  660, "29 JUN 1911", "1 DEC 2004", # Bernhard of Lippe-Biesterfeld
  663, "25 DEC 1700", "16 DEC 1751", # Leopold II of Anhalt-Dessau
  673, "15 AUG 1873", "27 MAY 1924", # Gyula Apponyi de Nagy-Appony
  675, "6 AUG 1915", "22 OCT 2002", # Geraldine of Albania
  676, "8 OCT 1895", "9 APR 1961", # Zog I of Albania
  677, "5 APR 1939", "30 NOV 2011", # Leka I of Albania
  678, "5 SEP 1771", "30 APR 1847", # Archduke Charles of Austria, Duke of Teschen
  679, "30 OCT 1797", "29 DEC 1829", # Henrietta of Nassau-Weilburg
  680, "29 JUL 1818", "20 NOV 1874", # Karl Ferdinand of Austria
  682, "21 JUL 1858", "6 FEB 1929", # Maria Cristina of Austria; some sources give 9 FEB 1929
  683, "28 NOV 1857", "25 NOV 1885", # Alfonso XII
  684, "9 MAR 1776", "13 JAN 1847", # Archduke Joseph of Austria, Palatine of Hungary
  685, "17 JAN 1831", "14 FEB 1903", # Elisabeth Franziska of Austria
  686, "20 JUL 1821", "15 DEC 1849", # Ferdinand Karl Viktor of Austria-Este
  689, "18 MAY 1869", "2 AUG 1955", # Rupprecht of Bavaria
  690, "9 OCT 1878", "24 OCT 1912", # Marie Gabrielle in Bavaria
  691, "3 MAY 1905", "8 JUL 1996", # Albrecht (Albert)
  716, "10 JAN 1675", "3 OCT 1675", # Catherine Laura Stuart; birth date added
  729, "24 DEC 1598", "1600", # Margaret Stuart; approximate death year
  735, "26 AUG 1596", "29 NOV 1632", # Frederick V of the Palatinate
  736, "14 OCT 1630", "8 JUN 1714", # Sophia of Hanover; sources vary 13/14 OCT 1630
  741, "26 APR 1575", "3 JUL 1642", # Marie de Medici; birth year varies in sources, selected 1575
  750, "27 MAY 1626", "6 NOV 1650", # William II of Orange
  751, "21 SEP 1640", "9 JUN 1701", # Philippe I, Duke of Orléans
  753, "19 MAR 1604", "6 NOV 1656", # John IV of Portugal
  754, "13 OCT 1613", "27 FEB 1666", # Luisa Maria de Guzmán
  755, "2 FEB 1634", "16 JUL 1662", # Alfonso IV d'Este, Duke of Modena
  756, "27 MAY 1639", "19 JUL 1687", # Laura Martinozzi
  758, "20 NOV 1629", "23 JAN 1698", # Ernest Augustus of Brunswick / Elector of Hanover
  759, "11 OCT 1761", "24 DEC 1803", # George I of Saxe-Meiningen
  760, "11 AUG 1763", "30 APR 1837", # Louise Eleonore of Hohenlohe-Langenburg
  762, "10 JUL 1736", "22 AUG 1807", # Maria Walpole / Countess Waldegrave / Duchess of Gloucester
  763, "24 JAN 1743", "28 DEC 1808", # Anne Horton / Anne Luttrell, Duchess of Cumberland
  765, "22 DEC 1617", "28 AUG 1680", # Charles I Louis, Elector Palatine
  766, "17 DEC 1619", "29 NOV 1682", # Rupert of the Rhine / Duke of Cumberland
  767, "16 JAN 1621", "1652", # Maurice of the Palatinate; death year approximate, lost at sea
  768, "5 OCT 1625", "10 MAR 1663", # Edward, Count Palatine of Simmern
  769, "20 NOV 1627", "16 MAR 1686", # Charlotte of Hesse-Kassel
  770, "7 SEP 1674", "14 AUG 1728", # Ernest Augustus, Duke of York and Albany
  777, "1793", "10 SEP 1843", # John Crocker Bulteel; birth year approximate
  779, "22 NOV 1791", "23 NOV 1851", # Horace Beauchamp Seymour
  781, "1 SEP 1758", "10 NOV 1834", # George John Spencer, 2nd Earl Spencer
  782, "27 JUL 1762", "8 JUN 1831", # Lavinia Bingham / Countess Spencer
  783, "18 JAN 1777", "13 APR 1848", # Henry Baring
  785, "11 DEC 1796", "12 MAY 1870", # Richard Curzon-Howe, 1st Earl Howe
  787, "8 JUL 1812", "31 MAR 1905", # Louisa Jane Russell
  788, "21 JAN 1811", "31 OCT 1885", # James Hamilton, 1st Duke of Abercorn
  789, "16 APR 1800", "10 NOV 1888", # George Charles Bingham, 3rd Earl of Lucan
  790, "29 JUN 1809", "1 APR 1877", # Anne Brudenell / Countess of Lucan
  791, "6 JUN 1796", "12 MAR 1874", # Caroline Paget / Duchess of Richmond
  792, "3 AUG 1791", "21 OCT 1860", # Charles Lennox, 5th Duke of Richmond
  803, "30 MAR 1864", "19 OCT 1935", # John Charles Montagu Douglas Scott, 7th Duke of Buccleuch
  804, "23 APR 1925", "23 MAR 2006", # Peter Shand Kydd
  806, "9 JUL 1901", "21 MAY 2000", # Barbara Cartland
  807, "11 DEC 1941", "29 JUL 2024", # Robert Fellowes, Baron Fellowes
  808, "19 JUL 1980", NA_character_, # Laura Jane Fellowes; living
  809, "4 OCT 1951", NA_character_, # Neil McCorquodale; living
  811, "13 SEP 1899", "7 AUG 1966", # Andrew Henry Ferguson
  812, "16 JUN 1908", "11 DEC 1996", # Marian Louisa Montagu-Douglas-Scott
  823, "1939", "10 AUG 1990", # Hector Barrantes; birth year approximate
  828, "28 JUN 1491", "28 JAN 1547", # Henry VIII Tudor
  832, "2 FEB 1503", "10 FEB 1503", # Katherine Tudor; death date corrected from year placeholder
  833, "16 DEC 1485", "7 JAN 1536", # Catherine of Aragon
  834, "17 MAR 1473", "9 SEP 1513", # James IV of Scotland
  835, "10 APR 1512", "14 DEC 1542", # James V of Scotland
  836, "1489", "22 JAN 1557", # Archibald Douglas, 6th Earl of Angus; birth year approximate
  839, "1484", "22 AUG 1545", # Charles Brandon, 1st Duke of Suffolk; birth year approximate
  840, "10 MAR 1452", "23 JAN 1516", # Ferdinand II of Aragon / Ferdinand V of Castile
  841, "22 APR 1451", "26 NOV 1504", # Isabella I of Castile
  848, "1501", "19 MAY 1536", # Anne Boleyn; birth year uncertain, commonly c.1501 or c.1507
  851, "1508", "24 OCT 1537", # Jane Seymour; birth year uncertain, commonly c.1508/1509
  856, "1524", "13 FEB 1542", # Catherine Howard; birth year uncertain, often c.1521/1524
  857, "1478", "19 MAR 1539", # Edmund Howard; birth year approximate
  863, "17 NOV 1493", "2 MAR 1543", # John Neville, 3rd Baron Latimer
  864, "1508", "20 MAR 1549", # Thomas Seymour, Baron Seymour; birth year approximate
  865, "1483", "12 NOV 1517", # Thomas Parr of Kendal; birth year approximate
  866, "6 APR 1492", "20 AUG 1531", # Maud Green
  868, "1537", "12 FEB 1554", # Jane Grey; birth year approximate
  869, "1535", "12 FEB 1554", # Guildford Dudley; birth year approximate
  871, "24 FEB 1500", "21 SEP 1558", # Charles V, Holy Roman Emperor
  872, "24 OCT 1503", "1 MAY 1539", # Isabella of Portugal
  873, "1208", "4 AUG 1265", # Simon de Montfort, 6th Earl of Leicester; birth year approximate
  874, "19 JAN 1928", "12 JUL 2000", # Tomislav of Yugoslavia
  875, "28 JUN 1929", "7 MAY 1990", # Andrej of Yugoslavia
  876, "1190", "6 APR 1231", # William Marshal, 2nd Earl of Pembroke; birth year approximate
  877, "17 JUL 1945", NA_character_, # Alexander, Crown Prince of Yugoslavia; living
  878, "13 DEC 1946", NA_character_, # Maria da Gloria of Orléans-Braganza; living
  879, "5 FEB 1980", NA_character_, # Peter of Yugoslavia; living
  880, "15 JAN 1982", NA_character_, # Philip of Yugoslavia; living
  881, "15 JAN 1982", NA_character_, # Alexander of Yugoslavia; living
  883, "15 MAR 1958", NA_character_, # Nikolas / Nikola of Yugoslavia; living
  884, "28 NOV 1959", NA_character_, # Katarina of Yugoslavia; living
  887, "10 JAN 1933", "21 NOV 2011", # Christina of Hesse
  889, "4 FEB 1960", NA_character_, # Christopher of Yugoslavia; living
  890, "18 JUL 1930", "24 SEP 2005", # Kira Melita of Leiningen
  895, "2 FEB 1907", "25 OCT 1951", # Maria Kirillovna of Russia; born in Coburg, no Old Style normalization applied
  896, "22 NOV 1602", "6 OCT 1644", # Elizabeth of France / Elisabeth of Bourbon
  897, "9 OCT 1757", "6 NOV 1836", # Charles X
  898, "17 NOV 1755", "16 SEP 1824", # Louis XVIII of France
  899, "23 DEC 1953", NA_character_, # Maria Vladimirovna of Russia; living
  900, "3 SEP 1943", NA_character_, # Franz Wilhelm of Prussia; living
  901, "13 MAR 1981", NA_character_, # George Mikhailovich Romanov; living
  902, "13 FEB 1898", "2 AUG 1946", # Karl, Prince of Leiningen
  903, "18 OCT 1926", "30 OCT 1991", # Emich Kyrill, Prince of Leiningen
  904, "2 JAN 1928", "28 SEP 1990", # Karl Vladimir Ernst Heinrich of Leiningen
  905, "9 MAY 1932", "16 JUN 1996", # Margarita Ileana of Leiningen; death date should be checked against sources if stricter certainty needed
  906, "2 JAN 1936", "12 FEB 2021", # Mechtilde Alexandra of Leiningen
  907, "18 JUN 1938", "29 AUG 1999", # Friedrich Wilhelm of Leiningen; source conflict: some line-of-succession summaries show 1998, genealogical memorial sources show 1999
  908, "2 FEB 1928", "26 JAN 2016", # Eilika of Oldenburg
  909, "19 JUN 1951", NA_character_, # Melita of Leiningen; living
  910, "12 JUN 1952", NA_character_, # Karl Emich of Leiningen; living
  911, "27 NOV 1955", NA_character_, # Andreas, Prince of Leiningen; living
  912, "1 OCT 1958", "23 SEP 2017", # Stephanie of Leiningen
  914, "15 NOV 1806", "12 MAY 1808", # Elizabeth Alexandrovna of Russia
  915, "8 AUG 1982", NA_character_, # Ferdinand, Hereditary Prince of Leiningen; living
  917, "20 FEB 1963", NA_character_, # Ulf-Karl Bauscher; living
  918, "31 OCT 1965", NA_character_, # Berthold Alexander Eric Bauscher; living
  919, "2 FEB 1971", NA_character_, # Johann Karl Joachim Fritz Markwart Bauscher; living
  920, "22 SEP 1601", "20 JAN 1666", # Anne of Austria
  921, "9 FEB 1939", "29 SEP 2015", # Friedrich Wilhelm of Prussia
  922, "22 MAR 1940", "3 APR 2014", # Michael of Prussia
  923, "28 MAY 1942", NA_character_, # Marie-Cécile of Prussia; living
  924, "25 AUG 1944", "11 JUL 1977", # Louis Ferdinand of Prussia, 1944-1977
  930, "30 JAN 1978", NA_character_, # Cornelie-Cécile of Prussia
  925, "14 MAR 1946", NA_character_, # Christian-Sigismund of Prussia; living
  929, "10 JUN 1976", NA_character_, # Georg Friedrich, Prince of Prussia; living
  932, "5 MAR 1967", NA_character_, # Michaela of Prussia; living; CSV sex currently M but name/source indicate female
  933, "13 JAN 1970", NA_character_, # Nataly of Prussia; living
  937, "16 AUG 1979", NA_character_, # Friedrich of Prussia; living
  938, "2 MAY 1982", NA_character_, # Viktoria-Luise of Prussia; living
  939, "26 JUN 1984", NA_character_, # Joachim of Prussia; living
  941, "23 NOV 1975", NA_character_, # Emily Lascelles / Emily Shard; living
  942, "19 SEP 1978", NA_character_, # Benjamin Lascelles; living
  943, "13 MAY 1980", NA_character_, # Alexander Edgar Lascelles; living
  944, "19 NOV 1982", NA_character_, # Edward David Lascelles; living
  946, "1 OCT 1973", NA_character_, # Sophie Amber Lascelles; living
  947, "6 NOV 1977", NA_character_, # Rowan Nash Lascelles; living
  951, "26 MAR 1662", "12 FEB 1689", # Marie Louise d'Orléans
  952, "3 APR 1893", "14 DEC 1945", # Maud Carnegie / Princess Maud, Countess of Southesk
  953, "23 SEP 1893", "16 FEB 1992", # Charles of Southesk
  954, "23 SEP 1929", "22 JUN 2015", # James Carnegie, 3rd Duke of Fife
  958, "9 JUN 1930", "16 SEP 2012", # Ragnhild Alexandra of Norway
  959, "12 FEB 1932", NA_character_, # Astrid Maud Ingeborg of Norway / Princess Astrid, Mrs Ferner; living
  960, "22 JUL 1927", "24 JAN 2015", # Johan Martin Ferner
  961, "22 JUL 1962", NA_character_, # Cathrine Ferner; living
  962, "27 SEP 1963", NA_character_, # Benedikte Ferner; living
  963, "15 MAR 1965", NA_character_, # Alexander Ferner; living
  964, "30 MAR 1969", NA_character_, # Elisabeth Ferner; living
  965, "22 OCT 1972", NA_character_, # Carl-Christian Ferner; living
  966, "28 JAN 1923", "9 MAR 2021", # Erling Lorentzen
  967, "23 AUG 1954", NA_character_, # Haakon Lorentzen; living
  968, "27 FEB 1957", NA_character_, # Ingeborg Lorentzen; living
  969, "8 MAY 1968", NA_character_, # Ragnhild Alexandra Lorentzen; living
  970, "18 SEP 1923", "1 AUG 2016", # Anne of Bourbon-Parma / Queen Anne of Romania
  972, "26 MAR 1949", NA_character_, # Margareta of Romania; living
  973, "15 NOV 1950", NA_character_, # Elena of Romania; living
  974, "28 FEB 1953", NA_character_, # Irina of Romania; living
  975, "29 OCT 1957", NA_character_, # Sophie of Romania; living
  976, "13 JUL 1964", NA_character_, # Maria of Romania; living
  977, "8 DEC 1942", "2 FEB 2002", # Robin Medforth-Mills
  978, "1 APR 1985", NA_character_, # Nicholas Medforth-Mills; living
  979, "3 AUG 1945", NA_character_, # John Kreuger; living or death not found in this pass
  987, "22 NOV 1428", "14 APR 1471", # Richard Neville, 16th Earl of Warwick
  988, "21 SEP 1411", "30 DEC 1460", # Richard of York, 3rd Duke of York
  989, "3 MAY 1415", "31 MAY 1495", # Cecily Neville
  990, "1364", "21 OCT 1425", # Ralph Neville, 1st Earl of Westmorland; approximate birth year
  992, "17 MAY 1443", "30 DEC 1460", # Edmund, Earl of Rutland
  993, "21 OCT 1449", "18 FEB 1478", # George Plantagenet, Duke of Clarence
  994, "10 AUG 1439", "14 JAN 1476", # Anne of York, Duchess of Exeter
  995, "22 APR 1444", "1503", # Elizabeth of York, Duchess of Suffolk; death year only
  996, "3 MAY 1446", "23 NOV 1503", # Margaret of York, Duchess of Burgundy
  998, "1437", "8 JUN 1492", # Elizabeth Woodville; approximate birth year
  999, "11 AUG 1467", "23 MAY 1482", # Mary of York
  1005, "1477", "1479", # George Plantagenet, Duke of Bedford; year-level dates only
  1007, "10 NOV 1480", "1517", # Bridget of York; death year only
  1008, "1450", "9 FEB 1499", # John Welles, 1st Viscount Welles; approximate birth year
  1010, "10 DEC 1472", "19 NOV 1481", # Anne Mowbray
  1011, "1473", "25 AUG 1554", # Thomas Howard, 3rd Duke of Norfolk; birth year approximate
  1012, "1475", "9 JUN 1511", # William Courtenay, 1st Earl of Devon; birth year approximate
  1013, "1432", "17 FEB 1461", # John Grey of Groby; birth year approximate
  1017, "25 DEC 1584", "3 OCT 1611", # Margaret of Austria, Queen of Spain
  1018, "25 FEB 1475", "28 NOV 1499", # Edward Plantagenet, 17th Earl of Warwick
  1019, "14 AUG 1473", "27 MAY 1541", # Margaret Pole, Countess of Salisbury
  1021, "1440", "8 NOV 1483", # Thomas St Leger; birth year approximate
  1022, "10 NOV 1433", "5 JAN 1477", # Charles the Bold
  1023, "20 JUL 1385", "5 AUG 1415", # Richard of Conisburgh, Earl of Cambridge
  1024, "27 DEC 1388", "22 SEP 1411", # Anne Mortimer
  1025, "1409", "2 OCT 1484", # Isabel Plantagenet; birth year only
  1026, "1404", "4 APR 1483", # Henry Bourchier, 1st Earl of Essex; birth year approximate
  1029, "1 FEB 1808", "6 DEC 1870", # Louise of Prussia
  1030, "18 MAR 1914", "9 DEC 1987", # Ernst August, Prince of Hanover
  1031, "25 MAR 1915", "8 JAN 2006", # George William of Hanover
  1033, "1 SEP 1919", "10 DEC 1981", # Christian Oscar of Hanover
  1034, "11 MAR 1923", "12 JUL 1997", # Welf Henry of Hanover
  1035, "8 AUG 1929", "4 JUN 2015", # Monika of Solms-Laubach / Princess of Hanover
  1036, "23 OCT 1937", "1 JUN 2015", # Alexandra of Ysenburg and Büdingen / Princess of Hanover
  1037, "26 JUN 1914", "24 NOV 2001", # Sophie of Greece and Denmark; likely duplicate/identity match to personID 476
  1038, "19 DEC 1925", "6 FEB 1980", # Ortrud of Schleswig-Holstein
  1039, "26 NOV 1952", NA_character_, # Marie of Hanover / Countess Marie of Hochberg; living
  1040, "26 FEB 1954", NA_character_, # Ernst August of Hanover; living
  1042, "17 FEB 1958", NA_character_, # Olga of Hanover; living
  1043, "18 FEB 1959", NA_character_, # Alexandra of Hanover / Princess of Leiningen; living; duplicate of personID 914 in OG likely
  1044, "29 APR 1961", NA_character_, # Heinrich Julius of Hanover; living
  1046, "2 JUN 1955", NA_character_, # Chantal Hochuli; living
  1047, "19 JUL 1983", NA_character_, # Ernst August of Hanover, born 1983; living
  1048, "1 JUN 1985", NA_character_, # Christian Heinrich of Hanover; living
  1049, "12 DEC 1962", "29 NOV 1988", # Isabella von Thurn und Valsassina; source conflict: another source gives 8 SEP 1962, selected 12 DEC 1962
  1050, "13 FEB 1988", NA_character_, # Otto Heinrich of Hanover; living
  1051, "27 NOV 1955", NA_character_, # Andreas of Leiningen; living; likely duplicate/identity match to personID 911
  1052, "3 JUL 1933", "29 DEC 2022", # Maximilian / Max, Margrave of Baden
  1053, "9 APR 1882", "17 NOV 1945", # Frederick Francis IV of Mecklenburg-Schwerin
  1054, "16 APR 1940", NA_character_, # Margrethe II of Denmark; living
  1058, "15 SEP 1895", "28 JUN 1977", # Elena (Magda) Lupescu; dates already exact in CSV, retained for explicit override
  1059, "28 MAR 1910", "7 NOV 2000", # Ingrid Victoria of Sweden / Queen Ingrid of Denmark
  1060, "20 MAY 1967", NA_character_, # Pavlos of Greece / Crown Prince Pavlos; living
  1061, "1 OCT 1969", NA_character_, # Nikolaos of Greece and Denmark; living
  1064, "11 MAY 1942", NA_character_, # Irene of Greece and Denmark; living
  1065, "7 APR 1871", "4 APR 1918", # Petros Manos
  1067, "21 JAN 1732", "23 DEC 1797", # Frederick Eugene of Württemberg
  1068, "18 DEC 1736", "9 MAR 1798", # Friederike Dorothea Sophia of Brandenburg-Schwedt
  1069, "3 DEC 1764", "27 SEP 1788", # Augusta of Brunswick-Wolfenbüttel
  1070, "27 SEP 1781", "25 JUN 1864", # William I of Württemberg
  1071, "21 FEB 1783", "29 NOV 1835", # Catherine of Württemberg
  1072, "24 DEC 1783", "3 OCT 1784", # Sophia Dorothea of Württemberg
  1073, "19 JAN 1785", "16 APR 1852", # Paul of Württemberg
  1074, "8 FEB 1792", "9 FEB 1873", # Charlotte of Bavaria
  1075, "21 MAY 1788", "9 JAN 1819", # Catherine Pavlovna of Russia, Gregorian/New Style; Old Style birth = 10 MAY 1788
  1076, "30 OCT 1816", "4 JAN 1887", # Marie of Württemberg
  1078, "4 SEP 1800", "10 MAR 1873", # Pauline of Württemberg
  1079, "24 AUG 1821", "6 DEC 1898", # Catherine of Württemberg
  1080, "6 MAR 1823", "6 OCT 1891", # Charles I of Württemberg
  1081, "4 OCT 1826", "3 DEC 1898", # Augusta of Württemberg
  1082, "11 SEP 1822", "30 OCT 1892", # Olga Nikolaevna of Russia, Gregorian/New Style; Old Style birth = 30 AUG 1822
  1083, "4 AUG 1825", "31 AUG 1901", # Hermann of Saxe-Weimar-Eisenach
  1084, "17 JUN 1787", "12 DEC 1847", # Charlotte of Saxe-Hildburghausen
  1085, "9 JAN 1807", "2 FEB 1873", # Charlotte of Württemberg
  1086, "21 FEB 1808", "9 MAY 1870", # Frederick of Württemberg
  1087, "7 MAR 1809", "28 MAY 1810", # Paul of Württemberg
  1088, "25 FEB 1810", "7 JUL 1856", # Pauline of Württemberg
  1089, "24 JAN 1813", "12 JAN 1885", # August of Württemberg
  1090, "25 FEB 1848", "2 OCT 1921", # William II of Württemberg
  1091, "23 MAY 1857", "30 APR 1882", # Marie of Waldeck and Pyrmont
  1092, "19 DEC 1877", "7 MAY 1965", # Pauline of Württemberg
  1093, "28 JUL 1880", "28 DEC 1880", # Ulrich of Württemberg
  1094, "27 JUN 1872", "18 JUN 1945", # Frederick, Prince of Wied
  1095, "10 OCT 1864", "16 JUL 1946", # Charlotte of Schaumburg-Lippe
  1096, "18 AUG 1903", "9 JUN 1978", # Nicholas Hohenzollern / Prince Nicholas of Romania
  1097, "5 JAN 1909", "21 JAN 1991", # Ileana Hohenzollern
  1098, "3 JAN 1913", "2 NOV 1916", # Mircea Hohenzollern / Prince Mircea of Romania
  1099, "22 SEP 1835", "8 JUN 1905", # Leopold of Hohenzollern
  1100, "17 FEB 1845", "27 DEC 1913", # Antonia of Portugal
  1101, "7 MAR 1864", "22 OCT 1927", # William of Hohenzollern
  1102, "1 SEP 1868", "21 FEB 1919", # Charles Anthony / Karl Anton of Hohenzollern
  1103, "18 OCT 1872", "6 JAN 1958", # Josephine of Belgium / Joséphine Caroline
  1104, "7 SEP 1811", "2 JUN 1885", # Charles Anthony / Karl Anton of Hohenzollern-Sigmaringen
  1105, "21 OCT 1813", "19 JUN 1900", # Josephine of Baden
  1106, "15 JUL 1837", "17 JUL 1859", # Stephanie of Hohenzollern-Sigmaringen / Queen of Portugal
  1108, "7 OCT 1841", "6 AUG 1866", # Anthony / Anton of Hohenzollern
  1109, "25 JUN 1843", "2 DEC 1904", # Frederick of Hohenzollern
  1110, "17 NOV 1845", "26 NOV 1912", # Marie of Hohenzollern-Sigmaringen / Countess of Flanders
  1111, "16 SEP 1837", "11 NOV 1861", # Pedro V of Portugal
  1114, "24 MAR 1837", "17 NOV 1905", # Philip of Flanders
  1115, "3 JUN 1869", "23 JAN 1891", # Prince Baudouin of Belgium
  1116, "30 NOV 1870", "28 MAR 1948", # Princess Henriette of Belgium; twin of Josephine
  1117, "30 NOV 1870", "18 JAN 1871", # Princess Josephine of Belgium; twin of Henriette
  1120, "10 OCT 1903", "1 JUN 1983", # Charles of Belgium, Regent
  1121, "4 AUG 1906", "27 JAN 2001", # Marie Jose
  1122, "15 SEP 1904", "18 MAR 1983", # Umberto II of Italy
  1123, "11 OCT 1927", "10 JAN 2005", # Josephine-Charlotte of Belgium
  1124, "6 JUN 1934", NA_character_, # Albert II of Belgium; living
  1125, "5 JAN 1921", "23 APR 2019", # Jean of Luxembourg; duplicate/identity match to personID 586 likely
  1126, "11 JUN 1928", "5 DEC 2014", # Fabiola de Mora y Aragón
  1127, "11 SEP 1937", NA_character_, # Paola Ruffo di Calabria; living
  1128, "15 APR 1960", NA_character_, # Philippe of Belgium; living
  1129, "5 JUN 1962", NA_character_, # Astrid of Belgium; living
  1130, "19 OCT 1963", NA_character_, # Laurent of Belgium; living
  1131, "16 DEC 1955", NA_character_, # Lorenz of Austria-Este; living
  1132, "28 NOV 1916", "7 JUN 2002", # Mary Lilian Baels / Princess Lilian of Belgium
  1133, "18 JUL 1942", "29 NOV 2009", # Alexandre of Belgium
  1134, "6 FEB 1951", NA_character_, # Marie-Christine of Belgium; living
  1135, "30 SEP 1956", NA_character_, # Marie-Esméralda of Belgium; living
  1139, "21 JUN 1788", "13 MAY 1851", # Auguste of Bavaria
  1140, "9 OCT 1790", "24 JAN 1794", # Amalie of Bavaria
  1141, "7 JUL 1795", "16 AUG 1875", # Charles / Karl Theodor of Bavaria
  1147, "15 NOV 1238", "4 AUG 1265", # Henry de Montfort
  1149, "7 OCT 1816", "12 APR 1817", # Theodolinde
  1150, "12 MAR 1821", "12 DEC 1912", # Luitpold, Prince Regent of Bavaria
  1151, "19 MAR 1823", "28 OCT 1914", # Adelgunde of Bavaria
  1152, "10 JUN 1825", "2 APR 1864", # Hildegard of Bavaria
  1153, "19 JUL 1828", "21 SEP 1875", # Adalbert of Bavaria
  1154, "28 OCT 1800", "12 FEB 1803", # Maximilian of Bavaria
  1155, "13 NOV 1801", "14 DEC 1873", # Elisabeth Ludovika of Bavaria; twin of Amalie
  1156, "13 NOV 1801", "8 NOV 1877", # Amalie Auguste of Bavaria; twin of Elisabeth
  1157, "27 JAN 1805", "13 SEP 1877", # Maria Anna of Bavaria
  1158, "30 AUG 1808", "25 JAN 1892", # Ludovika / Louise of Bavaria
  1159, "21 JUL 1810", "4 FEB 1821", # Maximiliana of Bavaria
  1160, "23 NOV 1707", "13 MAY 1708", # Frederick Louis of Prussia
  1161, "16 AUG 1710", "21 JUL 1711", # Frederick William of Prussia
  1163, "5 MAY 1713", "10 JUN 1714", # Charlotte Albertine of Prussia
  1164, "28 SEP 1714", "4 FEB 1784", # Frederica Louise of Prussia
  1165, "13 MAR 1716", "17 FEB 1801", # Philippine Charlotte of Prussia
  1166, "2 MAY 1717", "31 AUG 1719", # Louis Charles William of Prussia
  1167, "25 JAN 1719", "13 NOV 1765", # Sophia Dorothea Marie of Prussia
  1168, "24 JUL 1720", "16 JUL 1782", # Louise Ulrika of Prussia / Queen of Sweden
  1169, "9 NOV 1723", "30 MAR 1787", # Anna Amalia of Prussia
  1170, "18 JAN 1726", "3 AUG 1802", # Henry of Prussia
  1171, "23 MAY 1730", "2 MAY 1813", # Ferdinand of Prussia
  1172, "28 MAR 1846", "19 APR 1902", # Heinrich XXII Reuss
  1174, "29 SEP 1680", "23 DEC 1705", # Louise Dorothea Sophie of Prussia
  1175, "28 APR 1676", "5 APR 1751", # Frederick I of Sweden
  1177, "4 OCT 1685", "31 JAN 1686", # Frederick Augustus of Brandenburg
  1179, "1 APR 1825", "26 APR 1864", # Augusta Ferdinande of Austria-Tuscany
  1180, "9 FEB 1846", "28 SEP 1930", # Leopold of Bavaria
  1181, "12 NOV 1850", "19 SEP 1925", # Therese of Bavaria
  1182, "6 JUL 1852", "12 NOV 1907", # Arnulf / Amulf of Bavaria
  1183, "8 MAY 1901", "27 AUG 1914", # Luitpold of Bavaria
  1184, "21 SEP 1902", "21 APR 1903", # Irmingard of Bavaria
  1185, "30 MAY 1909", "26 JUN 1912", # Rudolf of Bavaria
  1186, "7 OCT 1899", "31 JUL 1954", # Antoinette of Luxembourg
  1187, "28 MAR 1922", "14 FEB 1958", # Heinrich / Henry of Bavaria
  1188, "29 MAY 1923", "23 OCT 2010", # Irmingard of Bavaria
  1189, "16 SEP 1924", "4 MAY 2013", # Editha of Bavaria
  1190, "24 MAR 1926", "5 MAY 2002", # Hilda of Bavaria
  1191, "10 MAY 1927", "19 APR 2019", # Gabriele of Bavaria
  1192, "20 JUN 1935", "28 MAY 2021", # Sophie of Bavaria
  1193, "4 DEC 1808", "15 NOV 1888", # Maximilian Joseph, Duke in Bavaria
  1194, "21 JUN 1831", "6 NOV 1920", # Ludwig Wilhelm, Duke in Bavaria
  1195, "4 APR 1834", "16 MAY 1890", # Helene in Bavaria
  1196, "24 DEC 1837", "10 SEP 1898", # Elisabeth in Bavaria / Empress Elisabeth of Austria
  1197, "9 AUG 1839", "30 NOV 1909", # Karl Theodor (Gackl)
  1198, "4 OCT 1841", "19 JAN 1925", # Marie Sophie in Bavaria
  1199, "30 SEP 1843", "18 JUN 1925", # Mathilde Ludovika in Bavaria
  1200, "22 FEB 1847", "4 MAY 1897", # Sophie Charlotte Auguste in Bavaria; source conflict on 22/23 FEB 1847
  1201, "7 DEC 1849", "12 JUN 1893", # Maximilian Emanuel in Bavaria
  1202, "19 MAR 1857", "11 MAR 1943", # Maria Josepha of Portugal
  1203, "25 SEP 1901", "28 JAN 1936", # Alexander Zoubkoff
  1204, "5 AUG 1939", NA_character_, # Irene of the Netherlands; living
  1205, "19 JAN 1943", NA_character_, # Margriet of the Netherlands; living
  1206, "18 FEB 1947", "16 AUG 2019", # Christina of the Netherlands
  1207, "6 SEP 1926", "6 OCT 2002", # Claus van Amsberg / Prince Claus of the Netherlands
  1208, "27 APR 1967", NA_character_, # Willem-Alexander of the Netherlands; living
  1209, "25 SEP 1968", "12 AUG 2013", # Johan Friso of Orange-Nassau
  1210, "11 OCT 1969", NA_character_, # Constantijn of the Netherlands; living
  1211, "1 SEP 1890", "19 DEC 1953", # Claus Felix von Amsberg
  1212, "26 JAN 1902", "13 JUN 1996", # Gösta von dem Bussche-Haddenhausen
  1213, "28 FEB 1823", "15 APR 1883", # Frederick Francis II of Mecklenburg-Schwerin
  1214, "29 JAN 1850", "22 APR 1922", # Marie of Schwarzburg-Rudolstadt
  1215, "22 APR 1518", "17 NOV 1562", # Antoine de Bourbon of France
  1216, "18 AUG 1831", "27 OCT 1888", # Helene of Nassau
  1222, "1369", "4 JUN 1394", # Mary de Bohun; approximate birth year, sources give 1369/70
  1224, "1388", "22 MAR 1421", # Thomas of Lancaster, Duke of Clarence; birth year approximate
  1226, "3 OCT 1390", "23 FEB 1447", # Humphrey of Gloucester
  1227, "25 FEB 1392", "22 MAY 1409", # Blanche of Lancaster
  1228, "4 JUN 1394", "5 JAN 1430", # Philippa of Lancaster
  1232, "16 JUN 1332", "5 OCT 1379", # Isabella of England, daughter of Edward III
  1233, "1335", "2 SEP 1348", # Joan of England; approximate birth year
  1236, "6 MAR 1340", "3 FEB 1399", # John of Gaunt
  1238, "1342", "1342", # Blanche of the Tower; year-level infant dates
  1239, "10 OCT 1344", "1361", # Mary of Waltham; death year approximate
  1241, "24 JUN 1348", "5 SEP 1348", # William of Windsor
  1243, "25 MAR 1342", "12 SEP 1368", # Blanche of Lancaster; death year sometimes given as 1368/1369
  1244, "10 OCT 1830", "9 APR 1904", # Isabella II of Spain
  1245, "13 MAY 1822", "17 APR 1902", # Francisco de Asís de Borbón
  1246, "14 OCT 1784", "29 SEP 1833", # Ferdinand VII of Spain
  1249, "7 DEC 1545", "10 FEB 1567", # Henry Stuart, Lord Darnley
  1250, "1534", "14 APR 1578", # James Hepburn, 4th Earl of Bothwell; approximate birth year
  1251, "22 NOV 1515", "11 JUN 1560", # Mary of Guise
  1252, "10 JUL 1451", "11 JUN 1488", # James III of Scotland; birth year sometimes given as 1451/1452
  1253, "1259", "29 JUL 1326", # Richard de Burgh, 2nd Earl of Ulster; approximate birth year
  1254, "11 JUN 1430", "3 NOV 1456", # Edmund Tudor, Earl of Richmond
  1255, "31 MAY 1443", "29 JUN 1509", # Margaret Beaufort
  1259, "1295", "12 OCT 1343", # Rainald II of Guelders; approximate birth year
  1260, "24 NOV 1394", "5 JAN 1465", # Charles of Orléans
  1262, "1241", "28 NOV 1290", # Eleanor of Castile; approximate birth year
  1268, "1272", "23 APR 1307", # Joan of Acre; approximate birth year, born April 1272
  1270, "15 MAR 1275", "11 MAR 1333", # Margaret of England, Duchess of Brabant; death often given as after 11 MAR 1333
  1271, "1 MAY 1276", "27 JUN 1278", # Berengaria of England
  1274, "7 AUG 1282", "5 MAY 1316", # Elizabeth of Rhuddlan
  1277, "1279", "14 FEB 1318", # Marguerite of France; approximate birth year
  1278, "1 JUN 1300", "4 AUG 1338", # Thomas of Brotherton
  1279, "5 AUG 1301", "19 MAR 1330", # Edmund of Woodstock
  1282, "1223", "24 JUN 1291", # Eleanor of Provence; approximate birth year
  1291, "13 MAY 1734", "31 OCT 1783", # John Spencer
  1292, "12 MAR 1716", "21 AUG 1780", # Georgiana Caroline Carteret
  1294, "1 OCT 1754", "24 MAR 1801", # Paul I Romanov, Gregorian/New Style; Old Style = 20 SEP 1754, 12 MAR 1801
  1295, "25 OCT 1759", "5 NOV 1828", # Maria Feodorovna of Württemberg; Gregorian/New Style death, Old Style death = 24 OCT 1828
  1296, "23 DEC 1777", "1 DEC 1825", # Alexander I Romanov, Gregorian/New Style; Old Style = 12 DEC 1777, 19 NOV 1825
  1297, "24 JAN 1779", "16 MAY 1826", # Elizabeth Alexeievna (Louise of Baden), New Style
  1298, "8 MAY 1779", "27 JUN 1831", # Konstantin Pavlovich Romanov,New Style
  1300, "21 FEB 1728", "17 JUL 1762", # Peter III Romanov, Gregorian/New Style death; Old Style death = 6 JUL 1762
  1301, "9 JUN 1672", "8 FEB 1725", # Peter I the Great Romanov, Gregorian/New Style; Old Style = 30 MAY 1672, 28 JAN 1725
  1302, "15 APR 1684", "17 MAY 1727", # Catherine I, Gregorian/New Style; Old Style = 5 APR 1684, 6 MAY 1727
  1303, "7 FEB 1708", "15 MAR 1728", # Anna Petrovna Romanov, Gregorian/New Style; Old Style = 27 JAN 1708, 4 MAR 1728
  1304, "29 DEC 1709", "5 JAN 1762", # Elizabeth Petrovna Romanov
  1305, "9 AUG 1669", "7 SEP 1731", # Yevdokiya Lopukhina, Gregorian/New Style; Old Style = 30 JUL 1669, 27 AUG 1731
  1306, "28 FEB 1690", "7 JUL 1718", # Alexei Petrovich Romanov, Gregorian/New Style; Old Style = 18 FEB 1690, 26 JUN 1718
  1308, "23 OCT 1715", "30 JAN 1730", # Peter II Romanov, Gregorian/New Style; Old Style = 12 OCT 1715, 19 JAN 1730; some sources give death as 29 JAN 1730
  1309, "19 MAR 1629", "8 FEB 1676", # Alexis I Michaylovich Romanov, Gregorian/New Style; Old Style = 9 MAR 1629, 29 JAN 1676
  1310, "11 APR 1624", "13 MAR 1669", # Maria Miloslavskaya, Gregorian/New Style; Old Style = 1 APR 1624, 3 MAR 1669
  1311, "9 JUN 1661", "7 MAY 1682", # Feodor III (Theodore) Romanov, Gregorian/New Style; Old Style = 30 MAY 1661, 27 APR 1682
  1312, "22 JUL 1596", "23 JUL 1645", # Mikhail III Feodorovich Romanov, Gregorian/New Style; Old Style = 12 JUL 1596, 13 JUL 1645
  1313, "1608", "18 AUG 1645", # Eudoxia Streshneva; birth year only; Gregorian/New Style death
  1314, "1 SEP 1651", "4 FEB 1694", # Natalia Naryshkina, Gregorian/New Style; Old Style = 22 AUG 1651, 25 JAN 1694
  1316, "6 SEP 1666", "8 FEB 1696", # Ivan V Romanov, Gregorian/New Style; Old Style = 27 AUG 1666, 29 JAN 1696
  1318, "7 FEB 1693", "28 OCT 1740", # Anna Ioannovna Romanov, Gregorian/New Style; Old Style = 28 JAN 1693, 17 OCT 1740
  1319, "29 OCT 1691", "14 JUN 1733", # Catherine Ivanovna of Mecklenburg / Yekaterina Ivanovna, Gregorian/New Style
  1321, "18 DEC 1718", "18 MAR 1746", # Anna Leopoldovna of Brunswick, Gregorian/New Style; Old Style = 7 DEC 1718, 7 MAR 1746
  1323, "23 AUG 1740", "16 JUL 1764", # Ivan VI Romanov, Gregorian/New Style; Old Style = 12 AUG 1740, 5 JUL 1764
  1324, "1655", "1669", # Ivan Alexeyevich Romanov; year-level dates only
  1325, "27 SEP 1657", "14 JUL 1704", # Sophia Alekseyevna Romanov, Gregorian/New Style; Old Style = 17 SEP 1657, 3 JUL 1704
  1327, "1415", "30 MAY 1472", # Jacquetta of Luxembourg; birth year approximate, often 1415/1416
  1328, "1350", "10 MAY 1403", # Catherine Swynford Roet; birth year approximate
  1329, "1373", "16 MAR 1410", # John Beaufort, 1st Earl of Somerset; birth year approximate
  1330, "1375", "11 APR 1447", # Henry Beaufort, Cardinal; birth year approximate
  1331, "1377", "13 NOV 1440", # Joan Beaufort; birth year approximate
  1333, "26 NOV 1401", "25 NOV 1418", # Henry Beaufort, 2nd Earl of Somerset
  1334, "25 MAR 1404", "27 MAY 1444", # John Beaufort, Duke of Somerset
  1335, "1406", "22 MAY 1455", # Edmund Beaufort, Duke of Somerset; birth year approximate
  1336, "1410", "8 AUG 1482", # Margaret Beauchamp; birth year approximate
  1337, "1355", "23 DEC 1392", # Isabella of Castile, Duchess of York; approximate birth year; death date varies in secondary sources
  1338, "1373", "25 OCT 1415", # Edward of Norwich, Duke of York; approximate birth year
  1339, "13 JUL 1426", "20 SEP 1492", # Anne Beauchamp, 16th Countess of Warwick
  1340, "5 SEP 1451", "22 DEC 1476", # Isabel Neville
  1342, "1400", "31 DEC 1460", # Richard Neville, Earl of Salisbury; birth year approximate
  1343, "1366", "3 OCT 1399", # Eleanor de Bohun; birth year approximate
  1344, "11 APR 1374", "20 JUL 1398", # Roger Mortimer, 4th Earl of March
  1345, "1386", "18 OCT 1405", # Eleanor Holland; birth year approximate
  1346, "6 NOV 1391", "18 JAN 1425", # Edmund Mortimer, 5th Earl of March
  1347, "1354", "NOV 1386", # Violante Visconti; year-level birth and month-level death
  1348, "16 AUG 1355", "5 JAN 1382", # Philippa of Ulster / Philippa of Clarence
  1349, "1 FEB 1352", "27 DEC 1381", # Edmund Mortimer, 3rd Earl of March
  1351, "1435", "29 JUL 1504", # Thomas Stanley, 1st Earl of Derby; birth year approximate
  1352, "1400", "2 FEB 1461", # Owen Tudor; birth year approximate
  1353, "NOV 1431", "21 DEC 1495", # Jasper Tudor; month-level birth date
  1354, "27 JUN 1880", "26 JAN 1952", # Natalia Sheremetevskaya, Gregorian/New Style; Old Style birth = 15 JUN 1880
  1355, "18 APR 1890", "13 DEC 1958", # Maria Pavlovna Romanov, Gregorian/New Style; Old Style birth = 6 APR 1890
  1357, "9 JAN 1897", "18 JUL 1918", # Vladimir Paley / Vladimir Romanov, Gregorian/New Style; Old Style birth = 28 DEC 1896
  1358, "5 DEC 1905", "27 DEC 1981", # Natalie Romanov
  1359, "21 DEC 1903", "15 NOV 1990", # Irina Paley / Irina Romanov, Gregorian/New Style; Old Style birth = 8 DEC 1903
  1360, "6 AUG 1910", "21 JUL 1931", # George Mikhailovich, Count Brasov, Gregorian/New Style; Old Style birth = 24 JUL 1910
  1361, "21 NOV 1868", "11 MAR 1924", # Peter of Oldenburg, Gregorian/New Style; Old Style birth = 9 NOV 1868
  1362, "29 JAN 1882", "13 MAR 1957", # Helen Vladimirovna of Russia Romanov, Gregorian/New Style; Old Style birth = 17 JAN 1882
  1366, "1188", "31 MAY 1246", # Isabella of Angouleme; birth year approximate
  1367, "5 JAN 1209", "2 APR 1272", # Richard, Earl of Cornwall
  1369, "1214", "1 DEC 1241", # Isabella of England; birth year approximate
  1370, "1215", "13 APR 1275", # Eleanor of England; birth year approximate
  1372, "1122", "1 APR 1204", # Eleanor of Aquitaine; birth year approximate
  1373, "17 AUG 1153", "1156",  #William IX, count of Poitiers
  1375, "1156", "28 JUN 1189", # Matilda (Maud), Duchess of Saxony; birth year approximate
  1376, "8 SEP 1157", "6 APR 1199", # Richard I Coeur de Lion
  1379, "OCT 1165", "4 SEP 1199", # Joan Plantagenet; month-level birth date
  1380, "1028", "9 SEP 1087", # William I the Conqueror; birth year approximate
  1381, "1031", "2 NOV 1083", # Matilda of Flanders; birth year approximate
  1382, "1054", "3 FEB 1134", # Robert Curthose; birth year approximate
  1383, "1055", "1075", # Richard of Normandy; year-level dates approximate
  1384, "1056", "2 AUG 1100", # William II Rufus; birth year approximate
  1385, "1056", "30 JUL 1126", # Cecilia of Normandy / Abbess of Holy Trinity; birth year approximate
  1388, "1067", "8 MAR 1137", # Adela of Normandy; birth year approximate
  1390, "1066", "13 AUG 1090", # Constance of Normandy; birth year approximate
  1391, "1068", "1 DEC 1135", # Henry I Beauclerc; birth year approximate
  1392, "1080", "1 MAY 1118", # Matilda (Edith) of Scotland; birth year approximate
  1393, "1090", "31 OCT 1147", # Robert of Gloucester; birth year approximate
  1394, "5 AUG 1103", "25 NOV 1120", # William Adelin, Duke of Normandy
  1395, "7 FEB 1102", "10 SEP 1167", # Empress Matilda
  1396, "1103", "23 APR 1151", # Adeliza of Louvain; birth year approximate
  1397, "1092", "25 OCT 1154", # Stephen, King of England; birth year approximate
  1398, "1105", "3 MAY 1152", # Matilda of Boulogne; birth year approximate
  1400, "1130", "17 AUG 1153", # Eustace IV of Boulogne; birth year approximate
  1402, "1137", "11 OCT 1159", # William of Boulogne; birth year approximate
  1403, "1136", "1182", # Mary of Boulogne; approximate year-level dates
  1404, "11 AUG 1086", "23 MAY 1125", # Henry V, Holy Roman Emperor
  1405, "24 AUG 1113", "7 SEP 1151", # Geoffrey V Plantagenet
  1406, "4 JAN 1904", "25 NOV 1971", # Audrey Emery / Princess Romanovsky-Ilyinsky
  1407, "27 JAN 1928", "10 FEB 2004", # Paul Romanovsky-Ilyinsky
  1408, "17 JUN 1884", "5 JUN 1965", # Prince Wilhelm of Sweden
  1409, "8 MAY 1909", "21 DEC 2004", # Lennart Gustaf Nicholas
  1412, "6 JAN 1367", "14 FEB 1400", # Richard II of England
  1415, "1340", "18 FEB 1397", # Enguerrand VII de Coucy; birth year approximate
  1416, "1354", "24 MAR 1394", # Constance of Castile; birth year approximate
  1417, "29 AUG 1347", "16 APR 1375", # John Hastings, 2nd Earl of Pembroke
  1418, "6 JUL 1332", "10 DEC 1363", # Elizabeth de Burgh, Duchess of Clarence
  1419, "30 APR 1700", "18 JUN 1739", # Charles Frederick of Schleswig-Holstein-Gottorp
  1423, "23 JUN 1703", "24 JUN 1768", # Maria Leszczynska / Marie Leczinska
  1426, "11 MAR 1516", "8 MAR 1534", # Henry Brandon, Earl of Lincoln
  1427, "16 JUL 1517", "20 NOV 1559", # Frances Brandon
  1428, "1519", "27 SEP 1547", # Eleanor Brandon; birth year approximate
  1429, "10 AUG 1520", "7 JUL 1537", # Madeleine of France
  1430, "17 JAN 1517", "23 FEB 1554", # Henry Grey, Duke of Suffolk
  1431, "8 OCT 1515", "9 MAR 1578", # Margaret Douglas
  1432, "21 SEP 1516", "4 SEP 1571", # Matthew Stuart, 4th Earl of Lennox
  1433, "1555", "APR 1576", # Charles Stuart, Earl of Lennox; approximate birth year and month-level death date
  1434, "1555", "21 JAN 1582", # Elizabeth Cavendish; birth year approximate
  1435, "10 NOV 1575", "25 SEP 1615", # Arabella Stuart
  1436, "1588", "24 OCT 1660", # William Seymour, Duke of Somerset; birth year approximate
  1439, "21 AUG 1858", "30 JAN 1889", # Rudolf, Crown Prince of Austria
  1442, "12 JUL 1844", "29 JUN 1910", # Ferdinand Philippe Marie d'Orléans, Duc d'Alençon
  1447, "11 JUL 1274", "7 JUN 1329", # Robert I Bruce
  1449, "1296", "2 MAR 1316", # Marjorie Bruce; birth year approximate
  1450, "1284", "27 OCT 1327", # Elizabeth de Burgh; birth year approximate
  1451, "5 MAR 1324", "22 FEB 1371", # David II Bruce
  1454, "1296", "9 APR 1327", # Walter Stewart, 6th High Steward; birth year approximate
  1455, "2 MAR 1316", "19 APR 1390", # Robert II of Scotland
  1457, "14 AUG 1337", "4 APR 1406", # Robert III of Scotland
  1459, "1340", "3 SEP 1420", # Robert Stewart, Duke of Albany; birth year approximate
  1461, "1350", "1401", # Annabella Drummond; approximate year-level dates
  1462, "24 OCT 1378", "26 MAR 1402", # David Stewart, Duke of Rothesay
  1463, "1394", "21 FEB 1437", # James I of Scotland; birth date late July 1394
  1465, "1404", "15 JUL 1445", # Joan Beaufort, Queen of Scots; birth year approximate
  1466, "16 OCT 1430", "3 AUG 1460", # James II of Scotland
  1467, "1434", "1 DEC 1463", # Mary of Guelders; birth year approximate
  1468, "2 FEB 1455", "20 FEB 1513", # John, King of Denmark
  1469, "23 JUN 1456", "14 JUL 1486", # Margaret of Denmark
  1470, "1454", "7 AUG 1485", # Alexander Stewart, Duke of Albany; birth year approximate
  1471, "1456", "1479", # John Stewart, Earl of Mar and Garioch; year-level dates approximate
  1472, "1453", "MAY 1488", # Mary Stewart, Countess of Arran; approximate birth year and month-level death date
  1474, "1415", "6 NOV 1479", # James Hamilton, 1st Lord Hamilton; birth year approximate
  1476, "1475", "1529", # James Hamilton, 1st Earl of Arran; year-level dates approximate
  1478, "1490", "4 SEP 1526", # John Stewart, 3rd Earl of Lennox; birth year approximate
  1482, "1542", "26 MAY 1583", # Esme Stuart, 1st Duke of Lennox; birth year approximate
  1484, "1484", "2 JUN 1536", # John Stewart, Duke of Albany; birth year approximate
  1488, "1440", "15 SEP 1512", # John Stewart, 1st Earl of Atholl; birth year approximate; death year/source conflict with 1513
  1492, "1516", "22 JAN 1575", # James Hamilton, Duke of Châtellerault; birth year approximate
  1494, "1537", "1609", # James Hamilton, 3rd Earl of Arran; year-level dates approximate
  1495, "1248", "2 MAY 1302", # Blanche of Artois; birth year approximate
  1496, "1278", "22 MAR 1322", # Thomas of Lancaster; birth year approximate
  1497, "1281", "22 SEP 1345", # Henry of Lancaster; birth year approximate
  1498, "2 FEB 1282", "3 DEC 1322", # Maud Chaworth
  1499, "1310", "23 MAR 1361", # Henry of Grosmont, Duke of Lancaster; birth year approximate
  1501, "31 MAR 1360", "19 JUL 1415", # Philippa of Lancaster
  1502, "21 FEB 1363", "24 NOV 1426", # Elizabeth of Lancaster
  1503, "11 APR 1357", "14 AUG 1433", # John I of Portugal
  1506, "1161", "5 SEP 1201", # Constance of Brittany; birth year approximate
  1507, "29 MAR 1187", "3 APR 1203", # Arthur, Duke of Brittany
  1508, "1165", "23 DEC 1230", # Berengaria of Navarre; birth year approximate
  1509, "1129", "6 AUG 1195", # Henry the Lion; birth year approximate
  1510, "1158", "18 SEP 1197", # Margaret of France; birth year approximate
  1511, "1031", "13 NOV 1093", # Malcolm III Canmore; birth year approximate
  1512, "1045", "16 NOV 1093", # St Margaret of Scotland; birth year approximate
  1513, "1074", "8 JAN 1107", # Edgar of Scotland; birth year approximate
  1514, "1078", "23 APR 1124", # Alexander I the Fierce; birth year approximate
  1515, "1084", "24 MAY 1153", # David I the Saint; birth year approximate
  1517, "1045", "19 MAY 1102", # Stephen Henry, Count of Blois; birth year approximate
  1518, "1090", "8 JAN 1152", # Theobald, Count of Blois; birth year approximate
  1520, "11 NOV 1155", "5 OCT 1214", # Alfonso VIII of Castile
  1525, "1000", "3 JUL 1035", # Robert the Devil / Robert I of Normandy; birth year approximate
  1527, "23 AUG 963", "28 AUG 1026", # Richard II of Normandy
  1529, "997", "6 AUG 1027", # Richard III of Normandy; birth year approximate
  1530, "28 AUG 933", "20 NOV 996", # Richard I the Fearless of Normandy
  1532, "984", "6 MAR 1052", # Emma of Normandy; birth year approximate
  1533, "966", "23 APR 1016", # Ethelred II the Unready; birth year approximate
  1535, "1025", "18 DEC 1075", # Edith (Eadgyth) of Wessex; birth year approximate
  1538, "1022", "14 OCT 1066", # Harold II; birth year approximate
  1541, "1053", "19 MAY 1125", # Vladimir of Kiev Monomakh; birth year approximate
  1543, "990", "30 NOV 1016", # Edmund II Ironside; birth year approximate
  1545, "1016", "19 APR 1057", # Edward Atheling / Edward the Exile; birth year approximate
  1548, "995", "12 NOV 1035", # Canute II the Great; birth year approximate
  1550, "1016", "17 MAR 1040", # Harold I Harefoot; birth year approximate
  1551, "1016", "1035", # Sweyn Knutsson / Svein of Norway; approximate year-level dates
  1552, "1018", "8 JUN 1042", # Hardicanute / Harthacnut; approximate birth year
  1553, "1001", "1066", # Herluin of Conteville; approximate year-level dates
  1554, "1035", "15 FEB 1097", # Odo of Bayeux; approximate birth year and approximate mid-month death date
  1555, "1031", "8 DEC 1095", # Robert, Count of Mortain; approximate birth year
  1556, "1063", "13 OCT 1119", # Alan IV of Brittany Fergant; approximate birth year
  1557, "1153", "11 NOV 1189", # William II the Good of Sicily; birth month uncertain in source summaries
  1558, "27 OCT 1156", "2 AUG 1222", # Raymond VI of Toulouse
  1559, "5 NOV 1881", "11 AUG 1958", # Nicholas / Nikolai Kulikovsky
  1560, "25 AUG 1917", "8 APR 1993", # Tikhon Nikolaevich Kulikovsky
  1561, "23 APR 1919", "11 SEP 1984", # Guri / Goury Nikolaevich Kulikovsky
  1562, "24 JAN 1897", "8 MAY 1981", # Andrew
  1563, "23 DEC 1898", "30 NOV 1968", # Theodore
  1564, "17 JAN 1900", "12 SEP 1974", # Nikita
  1565, "15 AUG 1901", "7 JUL 1980", # Dimitri
  1566, "24 NOV 1902", "31 JUL 1978", # Rostislav
  1567, "7 JUL 1907", "24 JUN 1989", # Vassily
  1568, "7 JUN 1869", "2 MAY 1870", # Alexander Alexandrovich Romanov, Gregorian/New Style; Old Style birth = 26 MAY 1869
  1569, "14 NOV 1847", "15 FEB 1922", # Catherine Dolgorukova / Princess Yurievskaya; source convention for birth date should be retained as checked
  1570, "12 MAY 1872", "13 SEP 1913", # George Alexandrovich Yurievsky / Romanov, Gregorian/New Style
  1571, "8 NOV 1873", "10 AUG 1925", # Olga Alexandrovna Yurievskaya / Romanov, Gregorian/New Style
  1572, "20 SEP 1878", "22 DEC 1959", # Catherine Alexandrovna Yurievskaya / Romanov, Gregorian/New Style
  1573, "10 MAY 1883", "28 MAY 1957", # Alexandra Zarnekau
  1581, "3 NOV 1890", "29 SEP 1978", # Serge Obelensky
  1574, "21 DEC 1900", "29 FEB 1988", # Alexander Alexandrovich Yurievsky; date source should be cross-checked if stricter certainty needed
  1575, "13 FEB 1871", "31 MAY 1948", # George Nikolaus von Merenberg / Count von Merenberg
  1576, "16 OCT 1897", "11 JAN 1965", # George Michael von Merenberg; exact dates from genealogical summaries
  1577, "3 OCT 1898", "15 SEP 1983", # Olga von Merenberg; exact dates from genealogical summaries
  1578, "1870", "1910", # Alexander V. Bariatinsky; year-level dates only
  1579, "1902", "1931", # Andrei Bariatinsky; year-level dates only
  1580, "1905", NA_character_, # Alexander Bariatinsky; birth year only; death not resolved in this pass
  1582, "1228", "9 NOV 1261", # Sanchia of Provence; approximate birth year
  1583, "1254", "17 OCT 1277", # Beatrix of Falkenburg; approximate birth year
  1584, "26 DEC 1194", "13 DEC 1250", # Frederick II of Germany / Holy Roman Emperor
  1585, "4 SEP 1241", "19 MAR 1286", # Alexander III of Scotland; Britannica notes death as 18/19 MAR 1286, selected 19 MAR
  1586, "1217", "8 OCT 1286", # John of Dreux / John I of Brittany, Earl of Richmond; approximate birth year
  1587, "20 JAN 1259", "10 NOV 1274", # Aveline de Forz
  1588, "2 SEP 1243", "7 DEC 1295", # Gilbert de Clare, Earl of Gloucester
  1589, "1270", "5 APR 1325", # Ralph de Monthermer; approximate birth year; corrects death-year placeholder
  1590, "27 SEP 1275", "27 OCT 1312", # John II, Duke of Brabant
  1591, "1284", "10 NOV 1299", # John I, Count of Holland; approximate birth year
  1592, "1276", "16 MAR 1322", # Humphrey de Bohun, Earl of Hereford; approximate birth year
  1593, "1290", "24 MAY 1326", # Mary de Ros; approximate birth year
  1594, "1339", "1 NOV 1399", # John IV (the Conqueror) of Montfort / Duke of Brittany; approximate birth year
  1595, "1385", "31 DEC 1439", # Margaret Holland; approximate birth year
  1596, "30 SEP 1404", "14 NOV 1432", # Anne of Burgundy
  1597, "16 JUL 1401", "8 OCT 1436", # Jacqueline of Holland
  1598, "1400", "7 JUL 1452", # Eleanor de Cobham; approximate birth year
  1599, "23 JAN 1378", "30 DEC 1436", # Ludwig III, Elector Palatine
  1600, "1382", "24 SEP 1459", # Eric X of Pomerania / Eric of Pomerania; birth year varies 1381/1382, selected 1382
  1601, "1368", "10 JUN 1437", # Joan of Navarre; approximate birth year; corrects death date
  1602, "1374", "28 MAY 1420", # William Bourchier, Count of Eu; approximate birth year
  1603, "30 APR 1383", "16 OCT 1438", # Anne of Gloucester
  1604, "14 AUG 1720", "31 OCT 1785", # Frederick II of Hesse-Cassel
  1605, "8 NOV 1715", "13 JAN 1797", # Elizabeth Christine of Brunswick-Wolfenbüttel
  1606, "29 MAY 1680", "2 SEP 1735", # Ferdinand Albert II of Brunswick
  1608, "1109", "12 OCT 1176", # William d'Aubigny, Earl of Arundel; approximate birth year
  1609, "11 JUN 1934", "13 FEB 2018", # Henri de Laborde de Monpezat / Prince Henrik of Denmark
  1610, "26 MAY 1968", NA_character_, # Frederik X of Denmark; living
  1611, "7 JUN 1969", NA_character_, # Joachim of Denmark; living
  1613, "4 JUL 1799", "8 JUL 1859", # Oscar I of Sweden; corrects death date
  1614, "14 MAR 1807", "7 JUN 1876", # Josephine de Beauharnais / Josephine of Leuchtenberg
  1615, "26 JAN 1763", "8 MAR 1844", # Charles XIV John of Sweden
  1616, "8 NOV 1777", "17 DEC 1860", # Désirée Clary / Queen Desideria of Sweden
  1617, "28 JAN 1768", "3 DEC 1839", # Frederick VI of Denmark
  1618, "28 OCT 1767", "21 MAR 1852", # Marie of Hesse-Cassel
  1619, "4 SEP 1729", "10 OCT 1796", # Juliana Maria of Brunswick-Wolfenbüttel
  1636, "1498", "13 MAY 1568", # Sophie / Sophia of Pomerania; approximate birth year
  1637, "7 JUL 1745", "3 JUN 1747", # Christian of Denmark
  1638, "3 JUL 1746", "21 AUG 1813", # Sophia Magdalena of Denmark
  1639, "10 JUL 1747", "14 JAN 1820", # Caroline of Denmark / Wilhelmina Caroline
  1640, "20 JAN 1750", "12 JAN 1831", # Louise of Denmark
  1641, "19 DEC 1744", "17 AUG 1836", # Charles of Hesse-Cassel
  1642, "22 SEP 1791", "23 SEP 1791", # Christian of Denmark; infant
  1643, "11 OCT 1753", "7 DEC 1805", # Frederick of Denmark
  1644, "24 AUG 1758", "29 NOV 1794", # Sophia Frederica of Mecklenburg-Schwerin
  1646, "22 NOV 1792", "29 JUN 1863", # Ferdinand of Denmark
  1648, "28 SEP 1796", "9 MAR 1881", # Caroline Amalie of Schleswig-Holstein-Sonderburg-Augustenburg
  1653, "7 JUL 1771", "13 JAN 1843", # Louise Augusta of Denmark
  1654, "28 SEP 1765", "14 JUN 1814", # Frederick Christian II of Schleswig-Holstein-Sonderburg-Augustenburg
  1655, "19 NOV 1792", "12 OCT 1793", # Marie Louise of Denmark
  1656, "28 OCT 1793", "31 MAR 1881", # Caroline of Denmark
  1657, "21 AUG 1795", "7 DEC 1795", # Louise of Denmark; infant
  1658, "1 SEP 1797", "5 SEP 1797", # Christian of Denmark; infant
  1659, "21 JAN 1802", "23 FEB 1802", # Louise Juliane of Denmark; infant
  1660, "3 JUN 1805", "14 JUL 1805", # Frederica Maria of Denmark; infant
  1661, "30 SEP 1813", "24 OCT 1878", # Charles of Schleswig-Holstein-Sonderburg-Glücksburg
  1662, "6 JUL 1832", "19 JUN 1867", # Maximilian of Austria / Emperor Maximilian of Mexico
  1663, "27 OCT 1858", "14 JAN 1939", # Valdemar of Denmark
  1664, "8 OCT 1876", "30 MAR 1949", # Harald of Denmark
  1665, "14 MAR 1880", "2 NOV 1945", # Thyra of Denmark
  1666, "4 MAR 1887", "5 OCT 1944", # Gustav of Denmark
  1667, "23 MAY 1890", "11 OCT 1961", # Dagmar of Denmark
  1668, "30 NOV 1893", "21 NOV 1978", # Jorgen Castenskiold
  1669, "1 JUN 1888", "30 JUN 1962", # Helena Adelaide of Schleswig-Holstein
  1670, "27 APR 1912", "12 DEC 1995", # Caroline-Mathilde of Denmark
  1671, "27 JUL 1900", "14 JUN 1976", # Knud, Hereditary Prince of Denmark
  1672, "29 APR 1944", NA_character_, # Benedikte of Denmark; living
  1673, "29 OCT 1934", "13 MAR 2017", # Richard of Sayn-Wittgenstein-Berleburg
  1674, "8 MAY 1935", "19 JUN 2018", # Elizabeth of Denmark / Princess Elisabeth
  1675, "17 FEB 1940", NA_character_, # Ingolf of Rosenborg; living
  1676, "22 OCT 1942", "21 MAY 2013", # Christian of Rosenborg
  1677, "3 OCT 1947", "2 JAN 2014", # Anne Dorte Maltoft-Nielsen / Countess of Rosenborg
  1681, "28 MAR 1800", "21 MAR 1806", # Charlotte of the Netherlands
  1682, "9 MAY 1810", "29 MAY 1883", # Marianne of the Netherlands
  1683, "6 JUL 1836", "23 JAN 1846", # William of the Netherlands
  1684, "5 JUL 1841", "22 JUN 1910", # Marie of the Netherlands
  1685, "22 AUG 1845", "22 OCT 1907", # William of Wied
  1686, "8 APR 1930", "18 AUG 2010", # Carlos Hugo of Bourbon-Parma
  1687, "30 APR 1939", NA_character_, # Pieter van Vollenhoven; living
  1688, "1 AUG 1946", NA_character_, # Jorge Guillermo; living or death not found in this pass
  1689, "2 AUG 1818", "20 FEB 1848", # Alexander of the Netherlands
  1690, "13 JUN 1820", "13 JAN 1879", # Henry of the Netherlands
  1691, "21 MAY 1822", "22 OCT 1822", # Ernest Casimir of the Netherlands; infant
  1692, "8 APR 1824", "23 MAR 1897", # Sophie of the Netherlands
  1693, "24 JUN 1818", "5 JAN 1901", # Charles Alexander of Saxe-Weimar
  1694, "18 OCT 1654", "22 MAR 1686", # John Frederick of Brandenburg-Ansbach
  1697, "24 JUL 1833", "16 MAY 1834", # Leopold of Belgium; infant son of Leopold I
  1699, "7 JUN 1840", "19 JAN 1927", # Marie Charlotte of Belgium / Carlota of Mexico
  1700, "22 OCT 1781", "4 JUN 1789", # Louis Joseph, Dauphin of France
  1702, "9 JUL 1786", "19 JUN 1787", # Sophie Beatrix / Sophie Hélène Béatrice of France
  1703, "19 DEC 1778", "19 OCT 1851", # Marie Thérèse of Angoulême
  1704, "769", "811", # Pepin the Hunchback; year-level dates approximate
  1705, "26 APR 1782", "24 MAR 1866", # Marie Amelie of Bourbon / Queen of France
  1706, "21 DEC 1919", "20 DEC 2000", # Alexander Ramsay of Mar
  1707, "18 OCT 1930", NA_character_, # Flora Fraser; living
  1709, "4 APR 1900", "4 FEB 1984", # Henry Somerset, Duke of Beaufort
  1710, "22 DEC 1879", "11 OCT 1932", # John Evelyn Gibbs / J. E. Gibbs
  1711, "8 MAR 1900", "24 JAN 1993", # Henry Abel Smith
  1712, "12 JAN 1960", "12 JAN 1960", # John Spencer; infant
  1714, "29 FEB 1904", "4 JUN 1979", # James Hamilton, Duke of Abercorn
  1716, "9 SEP 1764", "28 AUG 1819", # Charles Lennox, Duke of Richmond
  1717, "29 NOV 1737", "25 MAR 1805", # George Henry Lennox
  1718, "30 NOV 1739", "1830", # Louisa Kerr; death year only
  1719, "18 MAY 1701", "8 AUG 1750", # Charles Lennox, 2nd Duke of Richmond
  1720, "29 JUL 1672", "27 MAY 1723", # Charles Lennox, 1st Duke of Richmond
  1721, "24 JUN 1703", "20 OCT 1789", # Anne Lennox / Countess of Albemarle
  1722, "6 JUL 1766", "20 OCT 1839", # John Russell, Duke of Bedford
  1723, "15 NOV 1739", "2 NOV 1768", # Elizabeth Keppel / Marchioness of Tavistock
  1724, "1740", "1778", # Caroline Darcy / Marchioness of Lothian; year-level dates approximate
  1725, "1688", "1751", # Frederica Schomberg / Countess of Holderness; year-level dates
  1726, "30 JUN 1641", "5 JUL 1719", # Meinhardt Schomberg, Duke of Schomberg
  1728, "4 DEC 1764", "30 JUN 1839", # Richard Bingham, Earl of Lucan
  1729, "22 SEP 1735", "29 MAR 1799", # Charles Bingham, Earl of Lucan
  1730, "1799", "10 APR 1851", # Elizabeth Poyntz; approximate birth year
  1731, "27 OCT 1835", "13 AUG 1910", # John Poyntz Spencer, 5th Earl Spencer
  1732, "28 SEP 1835", "31 OCT 1903", # Charlotte Seymour / Countess Spencer
  1733, "11 JUN 1889", "18 JUL 1981", # Delia Peel; date source should be cross-checked if stricter certainty needed
  1734, "29 SEP 1899", "7 FEB 1955", # Lavinia Annaly / Lady Lavinia Spencer
  1735, "13 MAY 1708", "19 JUN 1746", # John Spencer of Althorp
  1736, "3 JUN 1743", "27 FEB 1821", # William IX of Hesse-Cassel
  1737, "21 JUN 1818", "22 AUG 1893", # Ernest II of Saxe-Coburg-Saalfeld
  1742, "4 MAR 1188", "27 NOV 1252", # Blanche of Castile; source conflict with Britannica death as 12 NOV 1252, selected 27 NOV
  1743, "18 JUN 1294", "1 FEB 1328", # Charles IV the Fair
  1746, "1199", "30 MAY 1252", # Ferdinand III of Castile; approximate birth year
  1747, "1 JUN 1180", "8 NOV 1246", # Berengaria of Castile
  1748, "1134", "31 AUG 1158", # Sancho III of Castile; approximate birth year
  1749, "23 NOV 1221", "4 APR 1284", # Alfonso X the Wise
  1751, "12 MAY 1258", "25 APR 1295", # Sancho IV of Castile
  1753, "986", "25 JUN 1014", # Athelstan Ætheling; approximate birth year
  1754, "1001", "1005", # Egbert / Ecgberht, son of Æthelred II; approximate year-level dates
  1756, "1003", "1017", # Edwy / Eadwig, son of Æthelred II; approximate year-level dates
  1763, "975", "15 AUG 1038", # Stephen I of Hungary; approximate birth year
  1767, "1005", "5 FEB 1036", # Alfred Aetheling; approximate birth year
  1768, "1020", "18 JUL 1038", # Gunhilda of Denmark; approximate birth year
  1776, "17 APR 963", "3 FEB 1014", # Sweyn Forkbeard
  1778, "930", "25 MAY 992", # Mieszko I of Poland; approximate birth year
  1779, "943", "8 JUL 975", # Edgar the Peaceful; birth year approximate 943/944
  1780, "945", "17 NOV 1000", # Ælfthryth / Elfrida; approximate birth year
  1782, "962", "18 MAR 978", # Edward the Martyr; approximate birth year
  1786, "921", "26 MAY 946", # Edmund I the Elder; approximate birth year
  1788, "940", "1 OCT 959", # Eadwig / Edwy; approximate birth year
  1792, "874", "17 JUL 924", # Edward the Elder; approximate birth year
  1794, "923", "23 NOV 955", # Eadred; approximate birth year
  1799, "894", "27 OCT 939", # Æthelstan; approximate birth year
  1801, "877", "927", # Sihtric Cáech / Sihtric of Northumbria; approximate birth year, death year; identity/title wording should be checked because row title says King of Denmark
  1802, "878", "920", # Ælfflæd, wife of Edward the Elder; approximate birth and death years
  1803, "904", "16 OCT 922", # Æthelweard, son of Edward the Elder; approximate birth year
  1804, "902", "933", # Edwin, son of Edward the Elder; approximate year-level dates
  1806, "902", "26 DEC 955", # Eadgifu / Edgiva of Kent; approximate birth year
  1808, "910", "26 JAN 937", # Eadhild / Edhilda, daughter of Edward the Elder; approximate birth year
  1809, "910", "26 JAN 946", # Eadgyth / Edith of England, wife of Otto I; approximate birth year
  1811, "17 SEP 879", "7 OCT 929", # Charles III the Simple, King of West Francia
  1812, "898", "16 JUN 956", # Hugh the Great; approximate birth year
  1813, "23 NOV 912", "7 MAY 973", # Otto I the Great
  1814, "932", "7 FEB 999", # Boleslaus II, Duke of Bohemia; birth year only
  1815, "20 OCT 1496", "12 APR 1550", # Claude, Duke of Guise
  1817, "11 SEP 1476", "22 SEP 1531", # Louise of Savoy; replaces year-placeholder dates
  1827, "1 FEB 1426", "21 MAY 1481", # Christian I of Denmark
  1828, "1430", "25 NOV 1495", # Dorothea of Brandenburg / Queen of Denmark; birth year only
  1830, "1303", "12 AUG 1332", # Robert Bruce of Liddesdale; approximate birth year; killed at Dupplin Moor
  1831, "1253", "9 NOV 1292", # Margaret/Marjorie of Carrick; approximate birth year; row name may be variant
  1832, "1243", "4 APR 1304", # Robert de Brus, 6th Lord of Annandale; approximate birth year, death before/around 4 APR 1304
  1833, "8 NOV 1226", "10 JUL 1264", # Isabel de Clare; death date sometimes given as after 10 JUL 1264
  1834, "1180", "25 OCT 1230", # Gilbert de Clare, Earl of Gloucester; approximate birth year
  1835, "1210", "31 MAR 1295", # Robert de Brus, 5th Lord of Annandale; approximate birth year
  1836, "1199", "1251", # Isobel/Isabella of Huntingdon; approximate year-level dates
  1837, "1144", "17 JUN 1219", # David of Scotland, Earl of Huntingdon; approximate birth year
  1838, "1171", "6 JAN 1233", # Matilda of Chester; approximate birth year
  1839, "1147", "30 JUN 1181", # Hugh de Kevelioc, Earl of Chester; approximate birth year
  1842, "1194", "1228", # Margaret of Huntingdon; approximate year-level dates
  1843, "1180", "1234", # Alan of Galloway; approximate year-level dates
  1844, "1210", "28 JAN 1290", # Dervorguilla of Galloway; approximate birth year
  1845, "1208", "25 OCT 1268", # John de Balliol; approximate birth year
  1846, "1249", "25 NOV 1314", # John Balliol, King of Scots; approximate birth year
  1847, "1253", "1292", # Isabel de Warenne; approximate year-level dates; identity inferred from Balliol/Warenne placement
  1848, "1231", "27 SEP 1304", # John de Warenne, 6th Earl of Surrey; approximate birth year
  1849, "1283", "15 JAN 1364", # Edward Balliol; approximate birth year and mid-month death date
  1850, "1012", "1 SEP 1067", # Baldwin V of Flanders; approximate birth year
  1853, "25 OCT 1102", "28 JUL 1128", # William Clito of Flanders
  1854, "1112", "1165", # Sibyl of Anjou; approximate year-level dates
  1855, "1092", "13 NOV 1143", # Fulk V of Anjou; approximate birth year
  1857, "1162", "1183", # Rainier of Montferrat; approximate year-level dates
  1859, "1060", "25 JAN 1100", # Godfrey of Bouillon / Lower Lorraine; approximate birth year
  1860, "1090", "25 NOV 1120", # Matilda of Blois; approximate birth year
  1866, "1 JUN 1134", "27 JUL 1158", # Geoffrey VI of Anjou; exact date completion
  1867, "22 JUL 1136", "30 JAN 1164", # William, Count of Poitou; exact date completion
  1868, "1099", "9 APR 1137", # William X of Aquitaine; birth year only
  1869, "1120", "18 SEP 1180", # Louis VII of France; birth year commonly 1120
  1870, "1071", "11 MAY 1138", # William de Warenne, 2nd Earl of Surrey; approximate birth year
  1871, "1099", "17 JAN 1168", # Thierry/Theodore of Flanders; approximate birth year
  1872, "21 APR 1132", "27 JUN 1194", # Sancho VI of Navarre
  1873, "1184", "10 AUG 1241", # Eleanor of Brittany; approximate birth year
  1874, "1138", "20 FEB 1171", # Conan IV of Brittany; approximate birth year
  1875, "1170", "26 OCT 1232", # Ranulf de Blondeville, Earl of Chester; approximate birth year
  1876, "1155", "13 APR 1213", # Guy of Thouars; approximate birth year
  1877, "23 NOV 1116", "23 NOV 1183", # William FitzRobert, Earl of Gloucester
  1878, "1100", "14 SEP 1144", # Geoffrey de Mandeville; approximate birth year
  1879, "1170", "12 MAY 1243", # Hubert de Burgh; approximate birth year
  1880, "1183", "5 JUN 1249", # Hugh X of Lusignan / Hugh de la Marche; approximate birth year
  1881, "1198", "19 AUG 1245", # Raymond Berengar IV of Provence; approximate birth year
  1882, "2 NOV 1235", "13 MAR 1271", # Henry of Almain
  1883, "1146", "14 MAY 1219", # William Marshal, Earl of Pembroke; approximate birth year
  1884, "1198", "19 AUG 1245", # Raymond Berengar IV of Provence; duplicate/variant row likely; approximate birth year
  1886, "24 AUG 1198", "6 JUL 1249", # Alexander II of Scotland
  1887, "26 DEC 1249", "25 SEP 1300", # Edmund, Earl of Cornwall
  1888, "1252", "1296", # Richard of Cornwall; approximate year-level dates
  1890, "4 AUG 1222", "15 JUL 1262", # Richard de Clare, Earl of Gloucester
  1891, "15 AUG 1171", "24 SEP 1230", # Alfonso IX of León; row title says Castile but identity appears Alfonso IX
  1893, "1191", "26 MAR 1242", # William de Forz, Count of Aumale/Albemarle; approximate birth year
  1894, "25 SEP 1216", "8 FEB 1250", # Robert I, Count of Artois
  1896, "1212", "30 OCT 1248", # Yolande of Dreux; approximate birth year
  1897, "1241", "12 NOV 1282", # Robert IV, Count of Dreux; approximate birth year
  1903, "1268", "29 NOV 1314", # Philip IV the Fair; birth year only retained
  1904, "11 NOV 1328", "26 FEB 1360", # Roger Mortimer, 2nd Earl of March
  1905, "1302", "16 DEC 1331", # Edmund Mortimer; approximate birth year
  1906, "25 APR 1287", "29 NOV 1330", # Roger Mortimer, 1st Earl of March
  1907, "1251", "17 JUL 1304", # Edmund Mortimer, 2nd Baron Mortimer; approximate birth year
  1908, "1231", "27 OCT 1282", # Roger Mortimer, 1st Baron Mortimer; approximate birth year
  1909, "1224", "23 MAR 1301", # Maud de Braose; approximate birth year
  1910, "1190", "6 AUG 1246", # Ralph de Mortimer; approximate birth year
  1911, "1206", "1251", # Gwladus Ddu; approximate year-level dates
  1912, "1178", "9 JUN 1228", # Reginald de Braose; approximate birth year
  1913, "1173", "11 APR 1240", # Llywelyn the Great; approximate birth year
  1914, "20 JUN 1760", "26 SEP 1842", # Richard Wellesley, 1st Marquess Wellesley
  1915, "19 JUL 1735", "22 MAY 1781", # Garret Wesley/Wellesley, 1st Earl of Mornington
  1916, "23 JUN 1742", "10 SEP 1831", # Anne Hill / Countess of Mornington
  1917, "1 MAY 1769", "14 SEP 1852", # Arthur Wellesley, 1st Duke of Wellington
  1918, "24 DEC 1738", "19 JUN 1770", # Arthur Hill-Trevor, Viscount Dungannon
  1919, "1144", "9 AUG 1211", # William de Braose; approximate birth year
  1920, "1112", "1192", # William de Braose; approximate year-level dates
  1921, "1130", "1170", # Bertha of Hereford / de Gloucester; approximate year-level dates
  1922, "1097", "24 DEC 1143", # Miles of Gloucester, Earl of Hereford; approximate birth year
  1923, "1092", "1143", # Sibyl de Neufmarché; approximate year-level dates
  1924, "1050", "1125", # Bernard de Neufmarché; approximate year-level dates
  1927, "1059", "1136", # Nest ferch Gruffydd; approximate year-level dates
  1928, "1010", "5 AUG 1063", # Gruffydd ap Llywelyn; approximate birth year
  1929, "28 OCT 1016", "5 OCT 1056", # Henry III, Holy Roman Emperor
  1930, "980", "1023", # Llywelyn ap Seisyll; approximate year-level dates
  1931, "982", "1058", # Angharad ferch Maredudd; approximate year-level dates
  1932, "978", "1023", # Cynfyn ap Gwerstan; approximate year-level dates
  1933, "1025", "1075", # Bleddyn ap Cynfyn; approximate year-level dates
  1934, "938", "999", # Maredudd ab Owain; approximate year-level dates
  1935, "913", "987", # Owain ap Hywel Dda; approximate year-level dates
  1936, "880", "950", # Hywel Dda; approximate year-level dates
  1937, "854", "909", # Cadell ap Rhodri; approximate year-level dates
  1938, "820", "878", # Rhodri Mawr; approximate year-level dates
  1939, "857", "916", # Anarawd ap Rhodri; approximate year-level dates
  1940, "883", "942", # Idwal Foel; approximate year-level dates
  1942, "920", "979", # Iago ab Idwal; approximate year-level dates
  1943, "920", "988", # Ieuaf ab Idwal; approximate year-level dates
  1945, "950", "985", # Hywel ap Ieuaf; approximate year-level dates
  1946, "950", "986", # Cadwallon ap Ieuaf; approximate year-level dates
  1948, "975", "996", # Idwal ap Meurig; approximate year-level dates
  1949, "1000", "1039", # Iago ap Idwal; approximate year-level dates
  1951, "1055", "1137", # Gruffydd ap Cynan; approximate year-level dates
  1952, "1100", "28 NOV 1170", # Owain Gwynedd; approximate birth year
  1953, "1145", "1174", # Iorwerth Drwyndwn; approximate year-level dates
  1955, "1145", "1203", # Dafydd ab Owain Gwynedd; approximate year-level dates
  1957, "1198", "1 MAR 1244", # Gruffydd ap Llywelyn Fawr; approximate birth year
  1958, "1212", "25 FEB 1246", # Dafydd ap Llywelyn; approximate birth year
  1959, "1212", "1256", # Angharad ferch Llywelyn; approximate year-level dates
  1960, "1282", "7 JUN 1337", # Gwenllian ferch Llywelyn; approximate birth year
  1961, "1223", "11 DEC 1282", # Llywelyn ap Gruffudd; approximate birth year
  1964, "849", "26 OCT 899", # Alfred the Great; birth year sometimes given 847-849, selected 849
  1965, "852", "5 DEC 902", # Ealhswith of Mercia; approximate birth year
  1966, "795", "13 JAN 858", # Æthelwulf of Wessex; approximate birth year
  1968, "825", "852", # Æthelstan of Kent; approximate year-level dates
  1969, "834", "20 DEC 860", # Æthelbald of Wessex; approximate birth year
  1970, "843", "870", # Judith of Flanders; approximate year-level dates
  1971, "835", "865", # Æthelberht of Wessex; approximate year-level dates
  1972, "847", "15 APR 871", # Æthelred I of Wessex; approximate birth year and mid-month death date
  1973, "771", "839", # Egbert of Wessex; approximate birth year
  1975, "825", "852", # Æthelstan of Kent; approximate year-level dates; likely duplicate/variant to personID 1968
  1977, "838", "888", # Æthelswith of Mercia; approximate year-level dates
  1978, "860", "898", # Æthelhelm, son of Æthelred I; approximate year-level dates
  1979, "868", "13 DEC 902", # Æthelwold ætheling; approximate birth year
  1980, "830", "874", # Burgred of Mercia; approximate year-level dates
  1982, "880", "16 OCT 922", # Æthelweard, son of Alfred; approximate birth year
  1985, "870", "12 JUN 918", # Æthelflæd, Lady of the Mercians; approximate birth year
  1986, "875", "896", # Æthelgifu of Shaftesbury; approximate year-level dates
  1987, "877", "7 JUN 929", # Ælfthryth of Wessex; approximate birth year
  1988, "865", "10 SEP 918", # Baldwin II of Flanders; approximate birth year
  1989, "845", "911", # Æthelred, Lord of the Mercians; approximate year-level dates
  1991, "467", "534", # Cerdic of Wessex; approximate year-level dates
  1992, "500", "560", # Cynric of Wessex; approximate year-level dates
  1993, "540", "593", # Ceawlin of Wessex; approximate year-level dates
  2022, "570", "597", # Ceolric of Wessex; approximate year-level dates
  2023, "560", "611", # Ceolwulf of Wessex; approximate year-level dates
  2024, "600", "642", # Cynegils of Wessex; approximate birth year
  2025, "620", "676", # Æscwine of Wessex; approximate year-level dates
  2029, "600", "636", # Cwichelm of Wessex; approximate year-level dates
  2030, "615", "672", # Cenwealh of Wessex; approximate birth year
  2031, "625", "685", # Centwine of Wessex; approximate year-level dates
  2035, "630", "674", # Seaxburh / Sexburh of Wessex; approximate year-level dates
  2036, "604", "5 AUG 642", # Oswald of Northumbria; approximate birth year
  2042, "659", "20 APR 689", # Cædwalla of Wessex; approximate birth year
  2043, "660", "687", # Mul of Kent; approximate year-level dates
  2046, "670", "726", # Ine of Wessex; approximate year-level dates
  2050, "672", "718", # Ingild of Wessex; approximate year-level dates
  2456, "1070", "14 FEB 1117", # Bertrada de Montfort
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
  2956, "14 MAY 1772", "30 OCT 1849", # William Charles Keppel, 4th Earl of Albemarle
  2985, "26 APR 1924", "14 DEC 1997", # Gerald Legge
  3011, "15 APR 1240", "1271" # Simon de Montfort the Younger
)

# notes:
# 1051 likely duplicates the Andreas of Leiningen already represented at personID == 911.
#1125 likely duplicates Jean of Luxembourg already represented at personID == 586.

# Non-date cautions surfaced while auditing:

#  1801: the row title says “King of Denmark,” but the likely identity is Sihtric Cáech/Sihtric of Northumbria or Dublin. I included the date data and noted the title/identity caution in the comment.

#1847: likely Isabel de Warenne based on the Balliol/Warenne placement, but the row’s given name alone is underspecified. I included the date data and flagged the inference.

#1884: appears to duplicate or variant-match 1881 Raymond Berengar IV of Provence. I included the date data and flagged it.

#1891: the row title says “King of Castile,” but Alfonso IX is King of León. I included the date data and flagged the title mismatch.

#1975: likely duplicates or variant-matches 1968 Æthelstan of Kent. I included the date data and flagged it.


name_overrides <- tribble(
  ~personID, ~name_override,
  12, "Alexandra of Denmark (Alix)",
  27, "Victoria Eugenie (Ena)",
  39, "Alexandra Fedorovna (Alix)",
  41, "Dagmar (Marie) of Denmark",
  82, "Sigismund of Prussia",
  84, "Elizabeth (Ella)",
  85, "Mary (May)",
  136, "Mary Adelaide (Fat Mary)",
  155, "Michael (Mischa) Alexandrovich Romanov",
  220, "Charlotte Augusta Louisa Hanover",
  304, "Claudine Rhédey de Kis-Rhéde",
  345, "Frederick William of Schleswig-Holstein-Sonderburg-Glücksburg",
  354, "Elizabeth Alexandra of Saxe-Altenburg",
  359, "Charles William Frederick Cavendish-Bentinck",
  402, "Augusta Victoria of Schleswig-Holstein-Sonderburg-Augustenburg",
  549, "Alexandra Victoria of Schleswig-Holstein-Sonderburg-Glücksburg",
  760, "Louise Eleonore of Hohenlohe-Langenburg",
  785, "Richard Curzon-Howe",
  788, "James Hamilton",
  812, "Marian Louisa Montagu-Douglas-Scott",
  932, "Michaela of Prussia",
  1137, "Augusta Wilhelmine of Hesse-Darmstadt",
  1176, "Sophia Louise of Mecklenburg-Schwerin",
  1197, "Karl Theodor (Gackl)",
  1200, "Sophie Charlotte Auguste",
  1212, "Gösta von dem Bussche-Haddenhausen",
  1213, "Frederick Francis II of Mecklenburg-Schwerin",
  1297, "Elizabeth Alexeievna (Louise of Baden)",
  1373, "William IX",
  1419, "Charles Frederick of Schleswig-Holstein-Gottorp",
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
  2956, "William Charles Keppel",
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
      personID == 146 ~ "Countess of Strathmore and Kinghorne",
      personID == 359 ~ "Reverend",
      personID == 914 ~ "Grand Duchess of Russia",
      personID == 932 ~ "Princess of Prussia",
      personID == 1373 ~ "Count of Poitiers",
      personID == 2943 ~ "Duke of Buccleuch",
      personID == 2956 ~ "Earl of Albemarle",

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
        932,
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
