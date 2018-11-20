- - - - - - - - - - - - - - - - - - -
Korisni direktorijumi:
- - - - - - - - - - - - - - - - - - -
	Čitav program i propratni materijali se nalaze u direktorijumu "fuzzer". 
	- Korisni poddirektorijumi su:
	  - "PDFcorpus": sadrži validne PDF datoteke koje ce program mutirati. U ovaj direktorijum se po želji mogu smeštati nove i brisati postojeće datoteke
	  - "PDF_citaci": sadrži PDF čitače koji su ponuđeni u programu
	  - "heuristics": sadrži tekstualne datoteke sa vrednostima različitih tipova koje često izazivaju greške u programima
	- Pri pronalasku greške u čitaču, informacije o tome se čuvaju u sledeća dva direktorijuma:
		- "fuzzedPDFError": sadrži PDF datoteke koja su izazvale greške u radu čitača
		- "results": sadrži tekstualne datoteke u kojima se nalaze informacije o standardnom izlazu, standardnom izlazu za greške i izlaznoj vrednosti čitača
  
	- Korisne datoteke su:
		- "fuzzer_jar.jar": izvršna datoteka implementiranog programa
		- "help.txt": datoteka sa uputstvom koje sadrži detalje o programu: svrhu programa, način njegovog rada i informacije o ulaznim parametrima.
		  Sadržaj ove datoteke može biti ispisan i pri pokretanju programa: pogledati u nastavku pokretanje programa sa uputstvom.
	  
- - - - - - - - - - - - - - - - - - -
Instalacija potrebnog softvera:
- - - - - - - - - - - - - - - - - - -
	Program je moguce pokretati na operativnim sistemima Windows i Linux. 
	
	1. U slučaja obe platforme potrebno je imati instaliranu Javu. Java se može skinuti i instalirati po uputstvima sa zvaničnog Oracle sajta: 
	https://www.oracle.com/technetwork/java/javase/downloads/index.html
	
    2. U slučaju pokretanja programa sa Linux operativnih sistema, potrebno je omogućiti izvršavanje Windows izvršnih datoteka na Linux
	platformama. To se postiže pomoću softvera Wine koji se instalira iz komandne linije sledećim komandama:
	sudo apt update
	sudo apt-get install wine
	sudo dpkg --add-architecture i386
	
- - - - - - - - - - - - - - - - - - -
Pokretanje programa:
- - - - - - - - - - - - - - - - - - -
Program se pokrece iz komandne linije. Iz komandne linije potrebno je pozicionirati se u direktorijum "fuzzer" i pokrenuti komandu:
	1. java -jar fuzzer_jar.jar                             ili:
	2. java -jar fuzzer_jar.jar --help                      u slučaju da želite ispisivanje uputstva koje sadrži detalje o programu:
	                                                        svrhu programa, način njegovog rada i informacije o ulaznim parametrima.
	