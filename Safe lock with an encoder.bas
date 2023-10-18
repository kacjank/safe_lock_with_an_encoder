$regfile = "m32def.dat"
$crystal = 16000000
$hwstack = 40
$swstack = 16
$framesize = 32

Dim Li As Byte
Dim I As Byte                                     'Liczba pomocnicza
Dim Liczba As Byte
Dim Znak As String * 1
Dim Znaki As String * 4
Dim Kod As String * 4
Dim Flaga_wypisz_gwiazdke As Bit
Dim Flaga_wys_komunikat As Bit
Dim Flaga_zamianu_kodu As Bit
Dim Liczba_str As String * 1
Dim Dlug_znaki As Byte
Dim Pozycja As Byte
Dim Liczba_prob As Byte
Dim Licz_zmian_kod As Byte                        'Dlugosc trzymania przycisku PINA.2
Dim B As Byte

$eeprom

Kod = "1111"
Pozycja = 6
Flaga_wypisz_gwiazdke = 1                         'Zabezpieczenie wypisywania gwiazdek
Flaga_wys_komunikat = 0                           'Zabezpieczenie wprowadzania cyfry za pomoc¹ PINA.2
Flaga_zamianu_kodu = 0


''''Pierwszy zapis i odczyt z EEPROM''''
B = 5
Writeeeprom B , 100
Readeeprom B , 100
''''''''''''''''''''''''''''''''''''''''


Config Lcd = 16 * 2
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.5 , Db6 = Portb.6 , Db7 = Portb.7 , E = Portb.2 , Rs = Portb.3
Cursor Off

Deflcdchar 0 , 224 , 224 , 238 , 241 , 254 , 240 , 238 , 228       'Deklaracja litery ê

'$sim                                              '''na potrzebê symulacji

Config Porta.0 = Input
Porta.0 = 1
Config Porta.1 = Input
Porta.1 = 1
Config Porta.2 = Input
Porta.2 = 1


'''''''''na potrzebê symulacji musimy dodaæ:
'Pina.0 = 1
'Pina.1 = 1
'Pina.2 = 1
''''''''''''


Do
   If Flaga_wys_komunikat = 0 Then Gosub Wys_poc  'Wyswietlanie pocz¹tkowych napisów przy starcie programu lub przy b³êdnym/poprawnym wpisaniu kodu

   Li = Encoder(pina.0 , Pina.1 , Lewo , Prawo , 0)       'PINA.0, PINA.1 - definicja gdzie zosta³y pod³¹czone koñcówki enkodera
   Waitus 100

   If Pina.2 = 0 And Flaga_zamianu_kodu = 1 Then
      Gosub Zmien_kod                             'Rozpoczecie zmiany kodu
   End If
   If Pina.2 = 0 And Flaga_zamianu_kodu = 0 Then
      Gosub Spr_kod                               'idziemy wtedy do podprogramu
   End If
Loop
End

'*******************  Podprogramy  ******************
Lewo:
   If Li = 0 Then                                 'sprawdzamy, czy zosta³a wykonana funkcja enkoder (czyli wprost - czy ruszyliœmy wgl osi¹ enkodera)
   Liczba = Liczba - 1
   If Liczba = 255 Then Liczba = 9
   Gosub Wys
   End If
Return

Prawo:
   If Li = 0 Then
   Liczba = Liczba + 1
   If Liczba = 10 Then Liczba = 0
   Gosub Wys
   End If
Return

Wys:
   Locate 2 , Pozycja
   If Flaga_wypisz_gwiazdke = 0 Then
      Lcd "*"
      Pozycja = Pozycja + 1
      Flaga_wypisz_gwiazdke = 1                   'ciekawostka - brak cls ¿eby ekran nie czyœci³ siê przy ka¿dym najmniejszym ruchu enkodera
   End If
   Lcd Liczba
       '" " s³u¿y nadpisaniu 0 z liczby 10 spacj¹, aby przy zmianie na np 7 nie zosta³o 0
Return


'Wypisz polecenie pocz¹tkowe
Wys_poc:
   If Flaga_zamianu_kodu = 0 Then
     Cls
     Lcd "Wpisz haslo"
     Lowerline
     Lcd "Znak: "
     Flaga_wys_komunikat = 1
   End If

   If Flaga_zamianu_kodu = 1 Then
      Cls
      Lcd "Podaj nowy kod"
      Lowerline
      Lcd "Znak: "
      Flaga_wys_komunikat = 0
   End If

   Gosub Wys

   Readeeprom Znaki , 1
   If Len(znaki) => 4 Then
      Kod = Znaki
   End If
   Znaki = ""
   Flaga_wys_komunikat = 1
Return


Spr_kod:
   Znaki = Znaki + Str(liczba)
   Dlug_znaki = Len(znaki)
   Flaga_wypisz_gwiazdke = 0

   If Dlug_znaki = 4 Then
      Flaga_wypisz_gwiazdke = 1
      If Znaki = Kod Then
         Cls
         Lcd "Otwarte"
         Portc.0 = 1
         Licz_zmian_kod = 0
         For I = 0 To 30
            If Pina.2 = 0 Then Licz_zmian_kod = Licz_zmian_kod + 1
            Waitms 100
         Next I

         Portc.0 = 0
         If Licz_zmian_kod > 10 Then Flaga_zamianu_kodu = 1
      Else
         Cls
         Lcd "Zamkni" ; Chr(0) ; "te"
         Wait 2
      End If
   Pozycja = 6
   Flaga_wys_komunikat = 0
   End If

   Do
   Loop Until Pina.2 = 1                          'Nie pozwala kontynuowaæ dopuki PINA.2 wcisniety

   Gosub Wys
Return


Zmien_kod:
   Znaki = Znaki + Str(liczba)
   Dlug_znaki = Len(znaki)
   Flaga_wypisz_gwiazdke = 0

   Do
   Loop Until Pina.2 = 1

   If Dlug_znaki = 4 Then
     Kod = Znaki
     Cls
     Lcd "Zmieniono kod"
     Wait 3
     Znaki = ""
     Pozycja = 6
     Flaga_wys_komunikat = 0
     Flaga_zamianu_kodu = 0
   End If

   Gosub Wys

Return