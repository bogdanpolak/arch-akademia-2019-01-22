# Przygotowanie bazy danych

1. Skopiowanie pliku bazy do wybranego folderu
	* plik z baz� danych: books.sdb
	* serwer: SQLite 3
2. Dodanie aliasu FireDAC (definicja po��czenia)
	* nazwa aliasu: SQLite_Books
	* Parametry aliasu:
		* Database: <�cie�ka do pliku  books.sdb>
		* User_Name: SYSDBA (mo�na u�y� dowolnej nazwy)
		* OpenMode: ReadWrite
3. Test po��czenie
4. Uruchomienie aplikacji
