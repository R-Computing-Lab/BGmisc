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
  ) %>%
  addPersonToPed(
    personID = 3012,
    name = "Elizabeth Alexandrovna of Russia",
    sex = "F",
    momID = 1297,
    dadID = 1296,
    overwrite = FALSE
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
  659, "30 APR 1909", "20 MAR 2004", # Juliana of Netherlands
  683, "28 NOV 1857", "25 NOV 1885", # Alfonso XII
  691, "3 MAY 1905", "8 JUL 1996", # Albrecht (Albert)
  812, "16 JUN 1908", "11 DEC 1996", # Marian Louisa Montagu-Douglas-Scott
  897, "9 OCT 1757", "6 NOV 1836", # Charles X
  924, "25 AUG 1944", "11 JUL 1977", # Louis Ferdinand of Prussia, 1944-1977
  930, "30 JAN 1978", NA_character_, # Cornelie-Cécile of Prussia
  953, "23 SEP 1893", "16 FEB 1992", # Charles of Southesk
  1097, "5 JAN 1909", "21 JAN 1991", # Ileana Hohenzollern
  1121, "4 AUG 1906", "27 JAN 2001", # Marie Jose
  1147, "15 NOV 1238", "4 AUG 1265", # Henry de Montfort
  1149, "7 OCT 1816", "12 APR 1817", # Theodolinde
  1197, "9 AUG 1839", "30 NOV 1909", # Karl Theodor (Gackl)
  1297, "24 JAN 1779", "16 MAY 1826", # Elizabeth Alexeievna (Louise of Baden), New Style
  1298, "8 MAY 1779", "27 JUN 1831", # Konstantin Pavlovich Romanov,New Style
  1304, "29 DEC 1709", "5 JAN 1762", # Elizabeth Petrovna Romanov
  1358, "5 DEC 1905", "27 DEC 1981", # Natalie Romanov
  1373, "17 AUG 1153", "1156",  #William IX, count of Poitiers
  1409, "8 MAY 1909", "21 DEC 2004", # Lennart Gustaf Nicholas
  1562, "24 JAN 1897", "8 MAY 1981", # Andrew
  1563, "23 DEC 1898", "30 NOV 1968", # Theodore
  1564, "17 JAN 1900", "12 SEP 1974", # Nikita
  1565, "15 AUG 1901", "7 JUL 1980", # Dimitri
  1566, "24 NOV 1902", "31 JUL 1978", # Rostislav
  1567, "7 JUL 1907", "24 JUN 1989", # Vassily
  1573, "10 MAY 1883", "28 MAY 1957", # Alexandra Zarnekau
  1581, "3 NOV 1890", "29 SEP 1978", # Serge Obelensky
  1668, "30 NOV 1893", "21 NOV 1978", # Jorgen Castenskiold
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
  3011, "15 APR 1240", "1271", # Simon de Montfort the Younger
  3012, "15 NOV 1806", "12 MAY 1808" # Elizabeth Alexandrovna of Russia
)


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
      personID == 1373 ~ "Count of Poitiers",
      personID == 2943 ~ "Duke of Buccleuch",
      personID == 2956 ~ "Earl of Albemarle",
      personID == 3012 ~ "Grand Duchess of Russia",
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
