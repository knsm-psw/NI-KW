# Narodowy Instytut KoronaWirusa

Projekt zbierania i analizowania publicznych danych
nt pandemii wirusa COVID19.

* MZN.csv 
  -- zarażeni/zmarli wg województw. Dane dzienne pozyskiwane
  z komunikatów MinZdrowia https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2

* MZ_units_beds.csv 
  -- łóżka/respiratory/osoby na kwarantannie/pod nadzorem/
  z komunikatów MinZdrowia publikowanych codziennie na twitter/@MZ_GOV_PL

* testy_wg_wojewodztw_true.csv
  -- raporty o liczbie testów zaczęły być wysyłane od 11 maja 
  2020. Do 3 sierpnia raportowane były zawyżone dane dla województwa świętokrzyskiego.  10 sierpnia MinZ
  wydało komunikat o błędzie i skorygował liczbę testów dla woj. świętokrzystkiego o ponad 230 tys.
  Tyle, że nie skorygowano błędnych danych w tygodniach 3.8 i wcześniejszych. Zakładając
  że 13303 (11.5) oraz 54249 (10.8) są poprawne  (zamiast 292239) i przyjmując
  ok 12% jako wzrost tydzień/tydzień otzymujemy w miarę sensowne wyniki. Nic lepszego się nie wymyśli.



