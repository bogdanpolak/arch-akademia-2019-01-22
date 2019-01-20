# Przygotowanie bazy danych

1. Skopiowanie pliku bazy do wybranego folderu
	* plik z baz¹ danych: books.sdb
	* serwer: SQLite 3
2. Dodanie aliasu FireDAC (definicja po³¹czenia)
	* nazwa aliasu: SQLite_Books
	* Parametry aliasu:
		* Database: <œcie¿ka do pliku  books.sdb>
		* User_Name: SYSDBA (mo¿na u¿yæ dowolnej nazwy)
		* OpenMode: ReadWrite
3. Test po³¹czenie
4. Uruchomienie aplikacji
