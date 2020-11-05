# Narodowy Instytut KoronaWirusa

Projekt zbierania i analizowania publicznych danych
nt pandemii wirusa COVID19.

* MZN.csv 
  -- zarażeni/zmarli wg województw. Dane dzienne pozyskiwane
  z komunikatów MinZdrowia https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2

* MZ_units_beds.csv 
  -- łóżka/respiratory/osoby na kwarantannie/pod nadzorem/
  z komunikatów MinZdrowia publikowanych codziennie na twitter/@MZ_GOV_PL

* MZ_testy_wg_wojewodztw_PL.csv
  -- testy wg województw. Dane tygodniowe (co poniedziałek) pozyskiwane
  z komunikatów MinZdrowia https://www.gov.pl/web/zdrowie/liczba-wykonanych-testow
  (oraz twitter/@MZ_GOV_PL)

* pomorskie_zgony_i.csv/pomorskie_zgony_d.csv
  Zgony w woj. pomorskim na podstawie komunikatów WSSE (wiek zmarłego/płeć)

Uwaga: raporty o liczbie testów zaczęły być wysyłane od 11 maja 
2020. Do 3 sierpnia raportowane były zawyżone dane dla województwa świętokrzyskiego.  10 sierpnia MinZ
wydało komunikat o błędzie i skorygował liczbę testów dla woj. świętokrzystkiego o ponad 230 tys.
Tyle, że nie skorygowano błędnych danych w tygodniach 3.8 i wcześniejszych. Zakładając
że 13303 (11.5) oraz 54249 (10.8) są poprawne  (zamiast 292239) i przyjmując
ok 12% jako wzrost tydzień/tydzień otrzymujemy w miarę sensowne wyniki. Nic lepszego się nie wymyśli.
  


