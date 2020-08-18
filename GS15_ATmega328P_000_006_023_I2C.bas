$regfile = "m328pdef.dat"
'$crystal = 7372800                                          ' Baudrate used crystal frequency
'$crystal = 7375258                                          ' after measuring deviation: +6s/5h
'$crystal = 7375551                                          ' after measuring deviation: +3s/21h
'$crystal = 7375496                                          ' after measuring deviation: -1s/37h
'$crystal = 7375476                                          ' after measuring deviation: -1s/103h
'$crystal = 7375616                                          ' after measuring deviation: +2s/26h
$crystal = 7375582                                          ' after measuring deviation: -2s/120h
$hwstack = 128
$swstack = 128
$framesize = 128
$baud = 38400                                               ' Baudrate UART 38400
$version 0 , 6 , 24

'LCD Samsung KS0066 20x4
Config Lcdpin = Pin , Rs = Portd.7 , E = Portb.0 , Db4 = Portb.1 , Db5 = Portb.2 , Db6 = Portb.3 , Db7 = Portb.4
Config Lcd = 20 * 4
Initlcd
Cursor Off Noblink
'Benutzerdefinierte Zeichen
Deflcdchar 0 , 2 , 5 , 2 , 32 , 32 , 32 , 32 , 32           ' ° (Grad Celcius)
Deflcdchar 1 , 4 , 4 , 14 , 14 , 4 , 4 , 14 , 31            ' DCF77 OK
Cls                                                         ' It is important that a CLS follows the DEFLCDCHAR statement(s)
Gosub Lcd_info                                              ' Anzeige Infotext
Wait 3

'we use the TWI pins of the Mega328P
$lib "i2c_twi.lbx"                                          ' we do not use software emulated I2C but the TWI
Config Scl = Portc.5                                        ' we need to provide the SCL pin name
Config Sda = Portc.4                                        ' we need to provide the SDA pin name
Config Twi = 100000
I2cinit

'Timer 0 für Torzeit Freqenzmessung
Config Timer0 = Timer , Prescale = 1024                     ' Timer 0 Torzeit für Frequenzzähler
On Timer0 Tmr0_isr                                          ' ISR
Enable Timer0 : Stop Timer0
Dim Tmr0_preload As Byte
'Timer 1 als Frequenzzähler für Messung Frequenz LCO, SRCO und TRCO (Anschluss PORTD.5 - T1)
Config Timer1 = Counter , Edge = Rising                     ' Timer 1 als Zähler
Enable Timer1 : Stop Timer1
'Timer 2 für Rückstellen LED
Config Timer2 = Timer , Prescale = 1024                     ' Timer 2 Rückstellen LED
On Timer2 Tmr2_isr                                          ' ISR alle 35,555 mSec
Enable Timer2 : Stop Timer2

'LED
Config Portd.6 = Output : Led_rt Alias Portd.6              ' LED rot
Config Portb.5 = Output : Led_bl Alias Portb.5              ' LED blau
Dim Led_count As Byte                                       ' Zähler für Einschaltdauer LED
Dim Led_an As Byte                                          ' Einschaltdauer LED

'Taster
Config Pinc.0 = Input : Portc.0 = 1                         ' Taster oben, Pullup ein
Config Pinc.1 = Input : Portc.1 = 1                         ' Taster links, Pullup ein
Config Pinc.2 = Input : Portc.2 = 1                         ' Taster rechts, Pullup ein
Config Pinc.3 = Input : Portc.3 = 1                         ' Taster unten, Pullup ein

'Piezo
Config Portd.2 = Output : Piezo Alias Portd.2               ' Piezo-Summer
Dim Sound_dist As Byte                                      ' Tonausgabe bei Störung ein/aus
Dim Sound_dist_eep As Eram Byte                             ' Tonausgabe bei Störung EEPROM
Sound_dist = Sound_dist_eep
If Sound_dist >= 2 Then                                     ' Fehler abfangen
   Sound_dist = 1 : Sound_dist_eep = 1                      ' Startwert Tonausgabe ein
End If
Dim Sound_light As Byte                                     ' Tonausgabe bei Blitzen ein/aus
Dim Sound_light_eep As Eram Byte                            ' Tonausgabe bei Blitzen EEPROM
Sound_light = Sound_light_eep
If Sound_light >= 2 Then                                    ' Fehler abfangen
   Sound_light = 1 : Sound_light_eep = 1                    ' Startwert Tonausgabe ein
End If

'serielle Schnittstelle
Dim Ser_ausgabe As Byte                                     ' Ausgabe seriell ein/aus
Dim Ser_ausgabe_eep As Eram Byte                            ' Ausgabe seriell EEPROM
Ser_ausgabe = Ser_ausgabe_eep
If Ser_ausgabe >= 2 Then                                    ' Fehler abfangen
   Ser_ausgabe = 1 : Ser_ausgabe_eep = 1                    ' Startwert Ausgabe ein
End If
Dim Ser_baudindex As Byte                                   ' Geschwindigkeit serielle Ausgabe
Dim Ser_baudindex_eep As Eram Byte                          ' Geschwindigkeit serielle Ausgabe EEPROM
Ser_baudindex = Ser_baudindex_eep
If Ser_baudindex >= 9 Then                                  ' Fehler abfangen
   Ser_baudindex = 4 : Ser_baudindex_eep = 4                ' Startwert Geschwindigkeit seriell 38400
End If
Gosub Ser_set_baud                                          ' Einstellung Übertragungsrate serielle Schnittstelle
Dim Rxchar_uart As Byte                                     ' Keyboard data
Dim Rxstr_uart As String * 35                               ' empfangener String
Dim Rxary_uart(6) As String * 15                            ' empfangener String gesplittet
Dim Rxspl_uart As Byte                                      ' Anzahl Teilstrings
On Urxc Int_rx_uart                                         ' define serial receive ISR
Enable Urxc                                                 ' enable Serial RX complete interrupt

'AS3935 Lightning Sensor
Const As3935_adr_w = &H06                                   ' Device Address for write
Const As3935_adr_r = &H07                                   ' Device Address for read
Config Pind.5 = Input : As3935int Alias Pind.5              ' Interrupt vom AS3935
Pcmsk2 = &B00100000                                         ' Pin-CHange-Interrupt 2 Bit 5
Enable Pcint2 : On Pcint2 Pcint2_int
Dim As3935_r0x00 As Byte                                    ' Register 00
Dim As3935_r0x01 As Byte                                    ' Register 01
Dim As3935_r0x02 As Byte                                    ' Register 02
Dim As3935_r0x03 As Byte                                    ' Register 03
Dim As3935_r0x04 As Byte                                    ' Register 04
Dim As3935_r0x05 As Byte                                    ' Register 05
Dim As3935_r0x06 As Byte                                    ' Register 06
Dim As3935_r0x07 As Byte                                    ' Register 07
Dim As3935_r0x08 As Byte                                    ' Register 08
Dim As3935_r0x3a As Byte                                    ' Register 3A
Dim As3935_r0x3b As Byte                                    ' Register 3B
Dim As3935_reg As Byte                                      ' Register Adresse
Dim As3935_data As Byte                                     ' Daten
Dim As3935_afe_gb As Byte                                   ' AFE Gain Boost
Dim As3935_pwd As Byte                                      ' Power-down
Dim As3935_nf_lev As Byte                                   ' Noise Floor Level
Dim As3935_wdth As Byte                                     ' Watchdog threshold
Dim As3935_cl_stat As Byte                                  ' Clear statistics
Dim As3935_min_num_ligh As Byte                             ' Minimum number of lightning
Dim As3935_srej As Byte                                     ' Spike rejection
Dim As3935_lco_fdiv As Byte                                 ' Frequency division ration for antenna tuning
Dim As3935_int As Byte                                      ' Interrupt
Dim As3935_mask_dist As Byte                                ' Mask Disturber
Dim As3935_s_lig As Dword                                   ' Energy of the Single Lightning
Dim As3935_distance As Byte                                 ' Distance estimation
As3935_distance = 63
Dim As3935_tun_cap As Byte                                  ' Internal Tuning Capacitors (from 0 to 120pf In Steps Of 8pf)
Dim As3935_afe_gb_eep As Eram Byte                          ' AFE Gain Boost EEPROM
Dim As3935_nf_lev_eep As Eram Byte                          ' Noise Floor Level EEPROM
Dim As3935_wdth_eep As Eram Byte                            ' Watchdog threshold EEPROM
Dim As3935_min_num_ligh_eep As Eram Byte                    ' Minimum number of lightning EEPROM
Dim As3935_srej_eep As Eram Byte                            ' Spike rejection EEPROM
Dim As3935_mask_dist_eep As Eram Byte                       ' Mask Disturber EEPROM
Dim Disturber As Word                                       ' Zähler Störungen gesamt
Dim Disturber_m(60) As Word                                 ' Array Zähler Störungen pro Minute
Dim Lightning As Word                                       ' Zähler Blitze
Dim Lightning_tx As Word                                    ' Zähler Blitze senden via TX433
Dim Interrupt As Byte                                       ' Interrupt ausgelöst
Dim Distance_tx As Word                                     ' Distance estimation to send via TX433
Distance_tx = 63
Dim Energie_tx As Dword                                     ' Energy to send via TX433

Dim Gewitter_zeiger As Byte                                 ' Zeiger für aktuelles Gewitter
Dim Gewitter_zeiger_eep As Eram Byte                        ' Zeiger für aktuelles Gewitter EEPROM
Gewitter_zeiger = Gewitter_zeiger_eep                       ' Zeiger laden
If Gewitter_zeiger >= 36 Then : Gewitter_zeiger = 2 : End If       ' unsinnige Werte abfangen
Dim Gewitter_dauer_s As Long                                ' Dauer Gewitter
Dim Gewitter_dauer_m As Word                                ' Dauer Gewitter
Dim Gewitter_alter As Word                                  ' Alter Gewitter
Dim Gewitter_dauer_eep(35) As Eram Word                     ' Dauer Gewitter EEPROM
Dim Gewitter_blitze_eep(35) As Eram Word                    ' Anzahl Blitze EEPROM
Dim Gewitter_syssec_eep(35) As Eram Long                    ' Datum und Uhrzeit Gewitter EEPROM
Dim Timestamp_blitz_1 As Long                               ' Zeitstempel erster Blitz
Dim Timestamp_blitz_last As Long                            ' Zeitstempel letzter Blitz

'DS1621 Thermometer
Const Ds1621_adr_w = &H90                                   ' Address of DS1621 Thermometer for write
Const Ds1621_adr_r = &H91                                   ' Address of DS1621 Thermometer for read
Dim Ds_lsb As Byte                                          ' LSB DS1621
Dim Ds_msb As Byte                                          ' MSB DS1621
Dim C_p_c As Byte                                           ' Count per C - Read Slope [A9h]
Dim C_r As Byte                                             ' Count Remain - Read Counter [A8h]
Dim Temperatur As Integer                                   ' Temperatur

'************************* Variablen TX 433 MHz ********************************
Config Portd.4 = Output : Tx433 Alias Portd.4               ' LED rot
Dim Tx_dbl As Double                                        ' 64 Bit Sendepuffer
Dim Tx_bit As Byte                                          ' zu sendendes Bit
Dim Tx_bit_nr As Byte                                       ' Nummer zu sendendes Bit
Dim Tx_byte As Byte                                         ' zu sendendes Byte
Dim Tx_byte_nr As Byte                                      ' Nummer zu sendendes Byte
Dim Check As Byte                                           ' Prüfsumme XOR Typ bis Check muss 0 ergeben
Dim Ersumme As Byte                                         ' Prüfsumme errechnet
Dim Potenz As Byte                                          ' Faktor
Dim S_adresse As Byte                                       ' Sensoradresse
S_adresse = 4                                               ' höchste Adresse bei 3 zu sendenden Werten (0, 2 , 4)
Dim Tx_sek As Byte                                          ' Zähler Sekunden Messwerte senden
Dim Tx_abstand As Byte                                      ' Abstand des Sendens der Sensorwerte
Tx_abstand = S_adresse / 2
Tx_abstand = 157 - Tx_abstand                               ' Abstand des Sendens der Sensorwerte
'Tx_abstand = 5

'Variablen für Menu
Dim Menu As Byte : Menu = 1                                 ' Menunummer
Dim Menu_old As Byte
Dim Menubeginn As Byte                                      ' erster Menupunkt
Dim Menuende As Byte                                        ' letzter Menupunkt
Dim Menuback As Byte                                        ' übergeordneter Menupunkt
Dim Menuverlassen As Byte                                   ' Zähler um ins Hauptmenu zurück zu springen
Dim Menunext As Byte                                        ' übergeordneter Menupunkt
Dim Wertmax As Word                                         ' maximal einstellbarer Wert
Dim Wert As Word                                            ' Wert hoch/runter
Dim Wertrefresh As Byte

'Variablen für Ereignisspeicher
Const Ereignis_max = 21
Dim Ereignis As Byte                                        ' Ereignis
Ereignis = 1                                                ' Neustart
Dim Esp_zusatz_1 As Byte                                    ' Ereignis Zusatz 1
Esp_zusatz_1 = 255                                          ' Ereignis Zusatz 1 zurück setzen
Dim Esp_zusatz_2 As Byte                                    ' Ereignis Zusatz 2
Esp_zusatz_2 = 255                                          ' Ereignis Zusatz 2 zurück setzen
Dim Esp_zeiger As Byte                                      ' Zeiger für aktuelles Ereignis
Dim Esp_zeiger_eep As Eram Byte                             ' Zeiger für aktuelles Ereignis EEPROM
Esp_zeiger = Esp_zeiger_eep                                 ' Zeiger laden
If Esp_zeiger >= 101 Then : Esp_zeiger = 2 : End If         ' unsinnige Werte abfangen
Dim Ereignis_eep(100) As Eram Byte                          ' Ereignis EEPROM
Dim Esp_zusatz_1_eep(100) As Eram Byte                      ' Ereignis Zusatz 1 EEPROM
Dim Esp_zusatz_2_eep(100) As Eram Byte                      ' Ereignis Zusatz 2 EEPROM
Dim Ereignis_syssec_eep(100) As Eram Long                   ' Datum und Uhrzeit Ereignis EEPROM

'Variablen temporär
Dim X As Byte , Y As Byte , Z As Byte                       ' temporär
Dim X1 As Byte , Y1 As Byte , Z1 As Byte                    ' temporär
Dim D As Dword                                              ' temporär
Dim W As Word , W1 As Word , W2 As Word                     ' temporär
Dim I As Integer , I1 As Integer                            ' temporär
Dim I_low As Byte At I Overlay : Dim I_high As Byte At I + 1 Overlay
Dim L As Long                                               ' temporär
Dim Ar(8) As Byte                                           ' temporär
Dim Str8 As String * 8                                      ' temporär
Dim Str10 As String * 10                                    ' temporär
Dim Str20 As String * 20                                    ' temporär

If Ser_ausgabe = 1 Then                                     ' serielle Ausgabe eingeschaltet
   Print "__________________________________"
   Print "I2C-Bus Scan Start"
End If
For X = 0 To 254 Step 2                                     ' for all odd addresses
   I2cstart                                                 ' send start
   I2cwbyte X                                               ' send address
   If Err = 0 Then                                          ' we got an ack
      Incr Y
      If Ser_ausgabe = 1 Then                               ' serielle Ausgabe eingeschaltet
         Print "I2C-Slave: " ; Y ; " hex: " ; Hex(x) ; " bin: " ; Bin(x)
      End If
   End If
   I2cstop                                                  ' free bus
Next
If Ser_ausgabe = 1 Then                                     ' serielle Ausgabe eingeschaltet
   If Y = 0 Then
      Print "I2C-Slave not found!"
   Else
      Print "I2C found " ; Y ; " Slave"
   End If
   Print "I2C-Bus Scan Complete"
End If

Enable Interrupts

'AS3935 Initialisierung
Gosub As3935_ld_default                                     ' Load Default Preset
Gosub As3935_cal_lco                                        ' LCO Calibration Start
Gosub As3935_cal_rco                                        ' RCO Calibration Start
Gosub As3935_rd_trco                                        ' Calibration of TRCO auslesen
Gosub As3935_rd_srco                                        ' Calibration of SRCO auslesen
Gosub As3935_ld_settings                                    ' Benutzereinstellungen laden
'Gosub As3935_clear_statistics                               ' clear the statistics by toggling the bit (high-low-high)
'Gosub As3935_rd_pwd                                         ' Power-down lesen
Gosub As3935_rd_cl_stat                                     ' Clear statistics lesen
Gosub As3935_rd_lco_fdiv                                    ' Frequency division ration for antenna tuning lesen
Gosub As3935_rd_s_lig                                       ' Energy of the Single Lightning lesen
Gosub As3935_rd_distance                                    ' Distance estimation lesen
Gosub As3935_rd_regall                                      ' alle Register auslesen
'Gosub As3935_rd_reg_look                                    ' Lightning Detection Look-up table auslesen

'DCF77, Check bestens, Update täglich 1 Uhr, erster 16-bit-Timer
Config Dcf77 = Pind.3 , Timer = 1 , Check = 2 , Update = 2 , Updatetime = 4 , Timer1sec = 1
'Config Dcf77 = Pind.3 , Timer = 1 , Check = 2 , Update = 1 , Updatetime = 1 , Timer1sec = 1
'Config Dcf77 = Pind.3 , Timer = 1 , Check = 2 , Timer1sec = 1
Config Date = Dmy , Separator = .                           ' deutsches Datumsformat
Dim Sec_old As Byte                                         ' vergangene Sekunde
Dim Sec_old1 As Byte                                        ' vergangene Sekunde
Dim Wt As Byte                                              ' Wochentag
Dim Dst As Byte                                             ' daylight saving time Automatik ein oder aus
Dim Dst_eram As Eram Byte                                   ' daylight saving time Automatik ein oder aus im EEPROM
Dst = Dst_eram                                              ' lese Einstellung daylight saving time Automatik aus EEPROM
If Dst >= 3 Then : Dst = 0 : Dst_eram = 0 : End If          ' Fehler abfangen (0 - Automatik aus, 1 - Winterzeit,  2 - Sommerzeit)
'DS1307 Clock
Const Ds1307_adr_w = &HD0                                   ' Address of Ds1307 Clock for write
Const Ds1307_adr_r = &HD1                                   ' Address of Ds1307 Clock for read
Dim Bday As Byte , Bmonth As Byte , Byear As Byte

'Datum und Uhrzeit aus DS1307 laden
Gosub Ds1307_rd_datetime                                    ' Datum und Uhrzeit aus DS1307 laden

'Temperatur messen
I2cstart
If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
I2cwbyte Ds1621_adr_w                                       ' Address of DS1621 Thermometer for write
If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
I2cwbyte &HEE                                               ' Temperaturmessung starten
If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
I2cstop
If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
Waitms 750                                                  ' Temperature Conversion Time
Gosub Ds1621_read                                           ' Werte auslesen und umrechnen
Menuverlassen = 1

'#################### H a u p t p r o g r a m m ####################
Do
   'Tastaturabfrage
   Debounce Pinc.0 , 0 , Taste_o , Sub                      ' Taster oben
   Debounce Pinc.1 , 0 , Taste_l , Sub                      ' Taster links
   Debounce Pinc.2 , 0 , Taste_r , Sub                      ' Taster rechts
   Debounce Pinc.3 , 0 , Taste_u , Sub                      ' Taster unten

   Select Case Menu
      Case 1                                                ' Hauptmenu
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menunext = 10
            Menuback = 0
            Menubeginn = 1
            Menuende = 7
            Cursor Off Noblink : Cls                        ' Anzeige löschen
         End If
         If Sec_old <> _sec Then                            ' einmal pro Sekunde ausführen
            Sec_old = _sec
            Wt = Dayofweek()                                ' Wochentag Zahl ermitteln
            Str10 = Lookupstr(wt , Wochentag)               ' Wochentag String laden
            X = Len(str10) : Y = 20 - X : Z = Y / 2         ' Ausgabeposition zentriert
            Str20 = Space(z)                                ' Leerzeichen vorn
            Str20 = Str20 + Str10                           ' String anhängen
            X = Y - Z : Str20 = Str20 + Space(x)            ' Leerzeichen hinten
            Upperline : Lcd Str20                           ' 1. Zeile Wochentag
            If Dcf_status.4 = 1 Then                        ' This Bit indicated that the DCF-Part is stopped
               Locate 1 , 20 : Lcd Chr(1)                   ' DCF77-Symbol anzeigen
            End If
            W = _year + 2000                                ' 4-stellig
            Locate 2 , 6 : Lcd Left(date$ , 6) ; W          ' 2. Zeile Datum Jahr 4-stellig
            Locate 3 , 5 : Lcd Time$ ; " Uhr"               ' 3. Zeile Uhrzeit
            Str8 = Str(temperatur)                          ' Temperatur übernehmen
            Str10 = Format(str8 , "0.0")                    ' String formatieren
            Str10 = Str10 + "{008}C"                        ' °C anhängen ( "{000}C" funktioniert nicht)
            X = Len(str10) : Y = 20 - X : Z = Y / 2         ' Ausgabeposition zentriert
            Str20 = Space(z)                                ' Leerzeichen vorn
            Str20 = Str20 + Str10                           ' String anhängen
            X = Y - Z : Str20 = Str20 + Space(x)            ' Leerzeichen hinten
            Fourthline : Lcd Str20                          ' 4. Zeile Temperatur
            'einmal pro Minute ausführen
            Select Case _sec
               'Case 29                                      ' ########## Sekunde 29 ##########
               '   Gosub Ds1621_mess                         ' DS1621 zum Temperaturmessen veranlassen
               Case 30                                      ' ########## Sekunde 30 ##########
                  Gosub Ds1621_read                         ' DS1621 Temperatur auslesen
            End Select
         End If
      Case 10                                               ' Einstellung Datum Tag
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 1                                    ' vorhergehender Menupunkt
            Menunext = 11                                   ' nächster Menupunkt
            Wertmax = 30                                    ' höchster Wert für aktuelle Einstellung
            Wert = _day - 1                                 ' Wert übernehmen
            Cursor On Noblink : Cls                         ' Anzeige löschen
            Locate 1 , 5 : Lcd "Einstellung"                ' 1. Zeile Überschrift
            Locate 2 , 7 : Lcd " Datum "                    ' 2. Zeile
            W = _year + 2000                                ' Jahr 4-stellig
            Locate 3 , 6 : Lcd Left(date$ , 6) ; W          ' 3. Zeile Datum Jahr 4-stellig
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _day = Wert + 1                                 ' Wert übernehmen
            Str10 = Str(_day)
            Locate 3 , 6 : Lcd Format(str10 , "00")         ' 4. Zeile
            Locate 3 , 7                                    ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 11                                               ' Einstellung Datum Monat
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 10                                   ' vorhergehender Menupunkt
            Menunext = 12                                   ' nächster Menupunkt
            Wertmax = 11                                    ' höchster Wert für aktuelle Einstellung
            Wert = _month - 1                               ' Wert übernehmen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _month = Wert + 1                               ' Wert übernehmen
            Str10 = Str(_month)
            Locate 3 , 9 : Lcd Format(str10 , "00")         ' 4. Zeile
            Locate 3 , 10                                   ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 12                                               ' Einstellung Datum Jahr
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 11                                   ' vorhergehender Menupunkt
            Menunext = 13                                   ' nächster Menupunkt
            Wertmax = 99                                    ' höchster Wert für aktuelle Einstellung
            Wert = _year                                    ' Wert übernehmen
            Locate 2 , 7 : Lcd " Datum "                    ' 2. Zeile
            Locate 3 , 5 : Lcd " " ;                        ' 3. Zeile
            Str10 = Str(_day) : Lcd Format(str10 , "00") ; ".";       ' 3. Zeile Tag
            Str10 = Str(_month) : Lcd Format(str10 , "00") ; ".";       ' 3. Zeile Monat
            W = _year + 2000                                ' Jahr 4-stellig
            Lcd W ; " "                                     ' 3. Zeile Jahr 4-stellig
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _year = Wert                                    ' Wert übernehmen
            Str10 = Str(_year)
            Locate 3 , 14 : Lcd Format(str10 , "00")        ' 4. Zeile
            Locate 3 , 15                                   ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 13                                               ' Einstellung Uhrzeit Stunde
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 12                                   ' vorhergehender Menupunkt
            Menunext = 14                                   ' nächster Menupunkt
            Wertmax = 23                                    ' höchster Wert für aktuelle Einstellung
            Wert = _hour                                    ' Wert übernehmen
            Locate 2 , 7 : Lcd "Uhrzeit"                    ' 2. Zeile
            Locate 3 , 4 : Lcd " " ; Time$ ; " Uhr"         ' 3. Zeile Datum
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _hour = Wert                                    ' Wert übernehmen
            Str10 = Str(_hour)
            Locate 3 , 5 : Lcd Format(str10 , "00")         ' 4. Zeile
            Locate 3 , 6                                    ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 14                                               ' Einstellung Uhrzeit Minute
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 13                                   ' vorhergehender Menupunkt
            Menunext = 15                                   ' nächster Menupunkt
            Wertmax = 59                                    ' höchster Wert für aktuelle Einstellung
            Wert = _min                                     ' Wert übernehmen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _min = Wert                                     ' Wert übernehmen
            Str10 = Str(_min)
            Locate 3 , 8 : Lcd Format(str10 , "00")         ' 4. Zeile
            Locate 3 , 9                                    ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 15                                               ' Einstellung Uhrzeit Sekunde
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 14                                   ' vorhergehender Menupunkt
            Menunext = 16                                   ' nächster Menupunkt
            Wertmax = 59                                    ' höchster Wert für aktuelle Einstellung
            Wert = _sec                                     ' Wert übernehmen
            Locate 2 , 2 : Lcd "     Uhrzeit      "         ' 2. Zeile
            Locate 3 , 2 : Lcd "   " ; Time$ ; " Uhr "      ' 3. Zeile Uhrzeit
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            _sec = Wert                                     ' Wert übernehmen
            Str10 = Str(_sec)
            Locate 3 , 11 : Lcd Format(str10 , "00")        ' 3. Zeile
            Locate 3 , 12                                   ' Cursor anzeigen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
         End If
      Case 16                                               ' Einstellung Sommer-/Winterzeit
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 15                                   ' vorhergehender Menupunkt
            Menunext = 1                                    ' nächster Menupunkt
            Wertmax = 2                                     ' höchster Wert für aktuelle Einstellung
            Wert = Dst                                      ' Wert übernehmen
            Locate 2 , 2 : Lcd "Sommer-/Winterzeit"         ' 2. Zeile
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Dst = Wert                                      ' Wert übernehmen
            Select Case Dst
               Case 0                                       ' Automatik aus
                  Locate 3 , 4 : Lcd "Automatik aus"        ' 3. Zeile
                  Locate 3 , 4                              ' Cursorposition
               Case 1                                       ' Winterzeit
                  Locate 3 , 4 : Lcd "  Winterzeit "        ' 3. Zeile
                  Locate 3 , 6                              ' Cursorposition
               Case 2                                       ' Sommerzeit
                  Locate 3 , 4 : Lcd "  Sommerzeit "        ' 3. Zeile
                  Locate 3 , 6                              ' Cursorposition
            End Select
            If Dst <> Dst_eram Then
               Dst_eram = Dst                               ' Einstellung speichern
            End If
         End If
      Case 2                                                ' Gewitter
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 60
            Menunext = 20
            Cursor Off Noblink : Cls                        ' Anzeige löschen
            Locate 1 , 7 : Lcd "Gewitter"                   ' 1. Zeile Überschrift
            Lowerline : Lcd "Distanz: "                     ' 2. Zeile
            If As3935_distance < 63 Then
               Lcd As3935_distance ; " km "                 ' 2. Zeile
            End If
            Thirdline : Lcd "Energie: " ; As3935_s_lig      ' 3. Zeile
            Fourthline : Lcd "Blitze:  " ; Lightning        ' 4. Zeile
         End If
         If As3935_int = &B00001000 Then                    ' Lightning interrupt
            As3935_int = 0
            Menuverlassen = 60
            Locate 2 , 10 : Lcd As3935_distance ; " km "    ' 2. Zeile Entfernung
            Locate 3 , 10 : Lcd Space(7)                    ' 3. Zeile löschen
            Locate 3 , 10 : Lcd As3935_s_lig                ' 3. Zeile Energie
            Locate 4 , 10 : Lcd Lightning                   ' 4. Zeile Anzahl Blitze
         End If
      Case 20                                               ' Speicher Gewitter anzeigen
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 2                                    ' vorhergehender Menupunkt
            Menunext = 2                                    ' nächster Menupunkt
            Wertmax = 34                                    ' 0-34 (1-35) höchster Wert für Speicher
            Wert = 0
            Cursor On Noblink : Cls                         ' Cursor ein blinkend
            Locate 1 , 5 : Lcd "Gewitter:"                  ' 1. Zeile Überschrift
            Thirdline : Lcd "Dauer:"                        ' 3. Zeile
            Fourthline : Lcd "Anzahl Blitze:"               ' 4. Zeile
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Locate 1 , 15 : X = Wert + 1 : Lcd X ; " "      ' Gewitter-Nummer 1-30 anzeigen
            Y = X + Gewitter_zeiger : Decr Y
            If Y >= 36 Then                                 ' max. 35 Einträge
               Y = Y - 35
            End If
            L = Gewitter_syssec_eep(y)                      ' Systemsekunden übernehmen
            Bday = Date(l)
            If Byear >= 15 Then
               Str10 = Date(l)                              ' Datum Ereigins übernehmen
               Str8 = Left(str10 , 6)                       ' Tag und Monat übernehmen
               Lowerline : Lcd Str8 ; "20"                  ' 2. Zeile Datum
               Str8 = Right(str10 , 2) : Lcd Str8           ' Jahr übernehmen und anzeigen
               Locate 2 , 13 : Lcd Time(l)                  ' 2. Zeile Uhrzeit
            Else
               Lowerline : Lcd Space(20)
            End If
            If Byear >= 15 Then
               W = Gewitter_dauer_eep(y)
               Locate 3 , 16 : Lcd "     "                  ' 3. Zeile letzte Zeichen löschen
               Locate 3 , 8 : Lcd W ;                       ' 3. Zeile Dauer Gewitter
               If W = 1 Then
                  Lcd " Minute"
               Else
                  Lcd " Minuten"
               End If
            Else                                            ' 3. Zeile
               Locate 3 , 8 : Lcd Space(13)                 ' 3. Zeile letzte Zeichen löschen
            End If
            If Byear >= 15 Then
               W = Gewitter_blitze_eep(y)
               Locate 4 , 16 : Lcd W ; Space(4)             ' 4. Zeile Anzahl Blitze Gewitter
            Else
               Locate 4 , 16 : Lcd Space(5)                 ' 4. Zeile letzte Zeichen löschen
            End If
            If X < 10 Then
               Locate 1 , 15                                ' Cursor anzeigen
            Else
               Locate 1 , 16                                ' Cursor anzeigen
            End If
         End If
      Case 3                                                ' Störungen
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 60
            Menunext = 3
            Cls                                             ' Anzeige löschen
            X = 0 : D = 0
            Do
               Incr X
               D = D + Disturber_m(x)                       ' Störungen pro Minute addieren
            Loop Until X >= 60
            Locate 1 , 6 : Lcd "St{239}rungen"              ' 1. Zeile Überschrift
            Lowerline : Lcd "letzte Stunde: " ; D           ' 2. Zeile
            Thirdline : Lcd "Heute gesamt:  " ; Disturber   ' 3. Zeile
         End If
         If As3935_int = &B00000100 Then                    ' Disturber detected
            As3935_int = 0
            Menuverlassen = 30
            X = 0 : D = 0
            Do
               Incr X
               D = D + Disturber_m(x)                       ' Störungen pro Minute addieren
            Loop Until X >= 60
            Locate 2 , 16 : Lcd D                           ' 2. Zeile
            Locate 3 , 16 : Lcd Disturber                   ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If As3935_int = &B00000001 Then                    ' Noise level too high
            As3935_int = 0
            Menuverlassen = 30
            Fourthline : Lcd "Noise Floor too high"         ' 4. Zeile
         End If
      Case 4                                                ' Einstellungen Sensor
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 30
            Menunext = 40
            Cursor Off Noblink : Cls                        ' Anzeige löschen
            Locate 1 , 4 : Lcd "Einstellungen"              ' 1. Zeile Überschrift
            Locate 2 , 4 : Lcd "Sensor AS3935"              ' 2. Zeile
         End If
      Case 40                                               ' Einstellung AFE Gain Boost
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 4                                    ' vorhergehender Menupunkt
            Menunext = 41                                   ' nächster Menupunkt
            Wertmax = 31                                    ' höchster Wert für aktuelle Einstellung
            Wert = As3935_afe_gb
            Cursor On Noblink                               ' Cursor ein blinkend
            Thirdline : Lcd "AFE Gain Boost:     "          ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
            As3935_afe_gb = Wert
            If As3935_afe_gb = 14 Then                      ' outdoor
               Locate 4 , 6
               Lcd As3935_afe_gb ; " outdoor"               ' 4.Zeile
               Locate 4 , 7                                 ' Cursorposition
            Elseif As3935_afe_gb = 18 Then                  ' indoor
               Locate 4 , 7
               Lcd As3935_afe_gb ; " indoor"                ' 4.Zeile
               Locate 4 , 8                                 ' Cursorposition
            Else                                            '
               Locate 4 , 10 : Lcd As3935_afe_gb            ' 4.Zeile
               If As3935_afe_gb < 10 Then
                  Locate 4 , 10                             ' Cursorposition
               Else
                  Locate 4 , 11                             ' Cursorposition
               End If
            End If
            If As3935_afe_gb <> As3935_afe_gb_eep Then
               As3935_afe_gb_eep = As3935_afe_gb            ' Einstellung speichern
               Gosub As3935_wr_afe_gb                       ' AFE Gain Boost schreiben
               Ereignis = 2                                 ' Set AFE_GB
            End If
         End If
      Case 41                                               ' Einstellung Noise Floor Level
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 40                                   ' vorhergehender Menupunkt
            Menunext = 42                                   ' nächster Menupunkt
            Wertmax = 7                                     ' höchster Wert für aktuelle Einstellung
            Wert = As3935_nf_lev                            ' Wert übernehmen
            Thirdline : Lcd "Noise Floor Level:  "          ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            As3935_nf_lev = Wert                            ' Wert übernehmen
            Locate 4 , 10 : Lcd As3935_nf_lev               ' 4.Zeile
            Locate 4 , 10                                   ' Cursor anzeigen
            If As3935_nf_lev <> As3935_nf_lev_eep Then
               As3935_nf_lev_eep = As3935_nf_lev            ' Einstellung speichern
               Gosub As3935_wr_nf_lev                       ' Noise Floor Level schreiben
               Ereignis = 3                                 ' Set NF_LEVEL
            End If
         End If
      Case 42                                               ' Einstellung Watchdog Threshold
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 41                                   ' vorhergehender Menupunkt
            Menunext = 43                                   ' nächster Menupunkt
            Wertmax = 15                                    ' höchster Wert für aktuelle Einstellung
            Wert = As3935_wdth                              ' Wert übernehmen
            Thirdline : Lcd "Watchdog Threshold: "          ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            As3935_wdth = Wert                              ' Wert übernehmen
            Locate 4 , 10 : Lcd As3935_wdth ; " "           ' 4.Zeile
            If As3935_wdth < 10 Then
               Locate 4 , 10                                ' Cursor anzeigen
            Else
               Locate 4 , 11                                ' Cursor anzeigen
            End If
            If As3935_wdth <> As3935_wdth_eep Then
               As3935_wdth_eep = As3935_wdth                ' Einstellung speichern
               Gosub As3935_wr_wdth                         ' Watchdog Threshold schreiben
               Ereignis = 5                                 ' Set WD_TRESHOLD
            End If
         End If
      Case 43                                               ' Einstellung Spike rejection
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 42                                   ' vorhergehender Menupunkt
            Menunext = 44                                   ' nächster Menupunkt
            Wertmax = 15                                    ' höchster Wert für aktuelle Einstellung
            Wert = As3935_srej                              ' Wert übernehmen
            Thirdline : Lcd "Spike Rejection:    "          ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            As3935_srej = Wert                              ' Wert übernehmen
            Locate 4 , 10 : Lcd As3935_srej ; " "           ' 4.Zeile
            If As3935_srej < 10 Then
               Locate 4 , 10                                ' Cursor anzeigen
            Else
               Locate 4 , 11                                ' Cursor anzeigen
            End If
            If As3935_srej <> As3935_srej_eep Then
               As3935_srej_eep = As3935_srej                ' Einstellung speichern
               Gosub As3935_wr_srej                         ' Spike rejection schreiben
               Ereignis = 6                                 ' Set SPIKE_REJECT
            End If
         End If
      Case 44                                               ' Einstellung Minimum Number of Lightning
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 43                                   ' vorhergehender Menupunkt
            Menunext = 45                                   ' nächster Menupunkt
            Wertmax = 3                                     ' höchster Wert für aktuelle Einstellung
            Wert = As3935_min_num_ligh                      ' Wert übernehmen
            Thirdline : Lcd "Minimum Number of   "          ' 3. Zeile
            Fourthline : Lcd "Lightning:          "         ' 4. Zeile
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            As3935_min_num_ligh = Wert                      ' Wert übernehmen
            Str10 = Lookupstr(as3935_min_num_ligh , Number_of_lightning)
            Locate 4 , 12 : Lcd Str10 ; " "
            If As3935_min_num_ligh < 3 Then
               Locate 4 , 12                                ' Cursor anzeigen
            Else
               Locate 4 , 13                                ' Cursor anzeigen
            End If
            If As3935_min_num_ligh <> As3935_min_num_ligh_eep Then
               As3935_min_num_ligh_eep = As3935_min_num_ligh       ' Einstellung speichern
               Gosub As3935_wr_min_num_ligh                 ' Minimum number of lightning schreiben
               Ereignis = 4                                 ' Set MIN_NUM_LIGH
            End If
         End If
      Case 45                                               ' Einstellung Mask Disturber
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 44                                   ' vorhergehender Menupunkt
            Menunext = 46                                   ' nächster Menupunkt
            Wertmax = 1                                     ' höchster Wert für aktuelle Einstellung
            Wert = As3935_mask_dist                         ' Wert übernehmen
            Thirdline : Lcd "Mask Disturber:     "          ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            As3935_mask_dist = Wert                         ' Wert übernehmen
            Locate 4 , 9
            If As3935_mask_dist = 0 Then
               Lcd "off"                                    ' 4.Zeile
            Else
               Lcd "on "                                    ' 4.Zeile
            End If
            Locate 4 , 9                                    ' Cursor anzeigen
            If As3935_mask_dist <> As3935_mask_dist_eep Then
               As3935_mask_dist_eep = As3935_mask_dist      ' Einstellung speichern
               Gosub As3935_wr_mask_dist                    ' Mask Disturber schreiben
               Ereignis = 7                                 ' Set MASK_DISTURB
            End If
         End If
      Case 46                                               ' Einstellung Load Preset
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 45                                   ' vorhergehender Menupunkt
            Menunext = 4                                    ' nächster Menupunkt
            Wertmax = 1                                     ' höchster Wert für aktuelle Einstellung
            Wert = 0                                        ' Wert übernehmen
            Thirdline : Lcd "Load Preset:        "          ' 3. Zeile
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Locate 4 , 7                                    ' 4. Zeile
            If Wert = 0 Then
               Lcd "User   "                                ' 4. Zeile
               Gosub As3935_ld_settings                     ' Benutzereinstellungen laden
               Ereignis = 8                                 ' "Load Preset User"
               Esp_zusatz_1 = 255                           ' Wert zurück setzen
            Else                                            ' indoor
               Lcd "Default"                                ' 4. Zeile
               Gosub As3935_ld_default                      ' Load Default Preset
               Ereignis = 9                                 ' "Load Set Default"
               Esp_zusatz_1 = 255                           ' Wert zurück setzen
            End If
            Locate 4 , 7                                    ' Cursor anzeigen
         End If
      Case 5                                                ' Einstellungen diverse
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 30
            Menunext = 50
            Cursor Off Noblink : Cls                        ' Anzeige löschen
            Locate 1 , 4 : Lcd "Einstellungen"              ' 1. Zeile Überschrift
            Locate 2 , 7 : Lcd "diverse"                    ' 2. Zeile
         End If
      Case 50                                               ' Einstellung Ausgabe seriell ein/aus
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 5                                    ' vorhergehender Menupunkt
            Menunext = 51                                   ' nächster Menupunkt
            Wertmax = 1                                     ' höchster Wert für aktuelle Einstellung
            Wert = Ser_ausgabe                              ' Wert übernehmen
            Cursor On Noblink                               ' Cursor ein blinkend
            Locate 2 , 2 : Lcd "serielle Schnittst."        ' 2. Zeile
            Thirdline : Lcd "Ausgabe: "                     ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Ser_ausgabe = Wert
            Gosub Lcd_ein_aus                               ' Anzeige ein/aus
            If Ser_ausgabe <> Ser_ausgabe_eep Then
               Ser_ausgabe_eep = Ser_ausgabe                ' Einstellungen speichern
            End If
         End If
      Case 51                                               ' Einstellung Baudrate serielle Schnittstelle
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 50                                   ' vorhergehender Menupunkt
            Menunext = 52                                   ' nächster Menupunkt
            Wertmax = 8                                     ' höchster Wert für aktuelle Einstellung
            Wert = Ser_baudindex                            ' Wert übernehmen
            Locate 2 , 2 : Lcd "serielle Schnittst."        ' 2. Zeile
            Thirdline : Lcd "Baudrate:   "                  ' 3. Zeile
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Ser_baudindex = Wert
            Str8 = Lookupstr(ser_baudindex , Table_ser_baud)
            Locate 4 , 8 : Lcd Str8                         ' 4. Zeile
            Locate 4 , 13                                   ' Cursor anzeigen
            If Ser_baudindex <> Ser_baudindex_eep Then
               Ser_baudindex_eep = Ser_baudindex            ' Einstellungen speichern
               Gosub Ser_set_baud                           ' Einstellung Übertragungsrate serielle Schnittstelle
            End If
         End If
      Case 52                                               ' Einstellung Tonausgabe bei Störung ein/aus
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 51                                   ' vorhergehender Menupunkt
            Menunext = 53                                   ' nächster Menupunkt
            Wertmax = 1                                     ' höchster Wert für aktuelle Einstellung
            Wert = Sound_dist                               ' Wert übernehmen
            Locate 2 , 2 : Lcd "      Warnton      "        ' 2. Zeile
            Thirdline : Lcd "bei St{239}rung:"              ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Sound_dist = Wert
            Gosub Lcd_ein_aus                               ' Anzeige ein/aus
            If Sound_dist <> Sound_dist_eep Then
               Sound_dist_eep = Sound_dist                  ' Einstellungen speichern
            End If
         End If
      Case 53                                               ' Einstellung Tonausgabe bei Blitzen ein/aus
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 52                                   ' vorhergehender Menupunkt
            Menunext = 5                                    ' nächster Menupunkt
            Wertmax = 1                                     ' höchster Wert für aktuelle Einstellung
            Wert = Sound_light                              ' Wert übernehmen
            Locate 2 , 2 : Lcd "      Warnton      "        ' 2. Zeile
            Thirdline : Lcd "bei Blitz:  "                  ' 3. Zeile
            Fourthline : Lcd Space(20)                      ' 4. Zeile löschen
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Sound_light = Wert
            Gosub Lcd_ein_aus                               ' Anzeige ein/aus
            If Sound_light <> Sound_light_eep Then
               Sound_light_eep = Sound_light                ' Einstellungen speichern
            End If
         End If
      Case 6                                                ' Ereignisspeicher
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 30
            Menunext = 60
            Cursor Off Noblink : Cls                        ' Anzeige löschen
            Upperline : Lcd "  Ereignisspeicher"            ' 1. Zeile Überschrift
         End If
      Case 60                                               ' Ereignisspeicher anzeigen
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Menuback = 6                                    ' vorhergehender Menupunkt
            Menunext = 6                                    ' nächster Menupunkt
            Wertmax = 99                                    ' 0-99 (1-100) höchster Wert für Ereignisspeicher
            Wert = 0
            Cursor On Noblink                               ' Cursor ein blinkend
            Upperline : Lcd "    Ereignis:     "            ' 1. Zeile Überschrift
         End If
         If Wertrefresh = 1 Then
            Wertrefresh = 0                                 ' nur einmal ausführen
            Menuverlassen = 60                              ' Menu nach 60 Sekunden verlassen
            Locate 1 , 15 : X = Wert + 1 : Lcd X ; "  "     ' Ereignis-Nummer 1-100 anzeigen
            Y = X + Esp_zeiger : Decr Y
            If Y >= 101 Then
               Y = Y - 100
            End If
            Z = Ereignis_eep(y)
            If Z <= Ereignis_max Then                       ' Maximalanzahl Ereignistexte
               L = Ereignis_syssec_eep(y)                   ' Systemsekunden übernehmen
               Str10 = Date(l)                              ' Datum Ereigins übernehmen
               Str8 = Left(str10 , 6)                       ' Tag und Monat übernehmen
               Lowerline : Lcd Str8 ; "20"                  ' 2. Zeile Datum
               Str8 = Right(str10 , 2) : Lcd Str8           ' Jahr übernehmen und anzeigen
               Locate 2 , 13 : Lcd Time(l)                  ' 2. Zeile Uhrzeit
               Str20 = Lookupstr(z , Ereignisse)            ' Ereignistext String einfügen
               Thirdline : Lcd Str20                        ' 3. Zeile Ereignistext anzeigen
               X1 = Len(str20) : X1 = 20 - X1 : Lcd Space(x1)       ' 3. Zeile Rest löschen
               Y1 = Esp_zusatz_1_eep(y)
               If Y1 < 255 Then
                  If Z = 4 Then                             ' Set Min Nr Lightning
                     If Y1 <= 3 Then
                        Str20 = Lookupstr(y1 , Number_of_lightning)
                     Else
                        Str20 = ""
                     End If
                  Elseif Z = 7 Then                         ' Set Mask Disturber
                     If Y1 = 0 Then
                        Str20 = "off"
                     Else
                        Str20 = "on"
                     End If
                  Else
                     Str20 = Str(y1)
                  End If
                  If Str20 <> "" Then
                     Str20 = Str20 + " {126} "              ' Pfeil nach rechts
                  End If
                  Z1 = Esp_zusatz_2_eep(y)
                  If Z = 4 Then                             ' Set Min Nr Lightning
                     If Y1 <= 3 Then
                        Str10 = Lookupstr(z1 , Number_of_lightning)
                     Else
                        Str10 = ""
                     End If
                  Elseif Z = 7 Then                         ' Set Mask Disturber
                     If Z1 = 0 Then
                        Str10 = "off"
                     Else
                        Str10 = "on"
                     End If
                  Else
                     Str10 = Str(z1)
                  End If
                  Str20 = Str20 + Str10
                  Fourthline : Lcd Str20                    ' 4. Zeile
                  Z = Len(str20) : Z = 20 - Z : Lcd Space(z)       ' 4. Zeile Rest löschen
               Else
                  Fourthline : Lcd Space(20)                ' 4. Zeile löschen
               End If
            Else
               Lowerline : Lcd Space(20)                    ' 2. Zeile löschen
               Thirdline : Lcd Space(20)                    ' 3. Zeile löschen
               Fourthline : Lcd Space(20)                   ' 4. Zeile löschen
            End If
            If X < 10 Then                                  ' Cursorposition
               Locate 1 , 15                                ' Cursor anzeigen
            Elseif X < 100 Then
               Locate 1 , 16                                ' Cursor anzeigen
            Else
               Locate 1 , 17                                ' Cursor anzeigen
            End If
         End If
      Case 7                                                ' Info
         If Menu <> Menu_old Then
            Menu_old = Menu
            Menuverlassen = 30
            Menunext = 70
            Cls
            Gosub Lcd_info                                  ' Anzeige Infotext
         End If
      Case 70                                               ' Neustart
         If Menu <> Menu_old Then
            Menu_old = Menu                                 ' nur einmal ausführen
            Menuverlassen = 30                              ' Menu nach 30 Sekunden verlassen
            Menuback = 7                                    ' vorhergehender Menupunkt
            Menunext = 71                                   ' nächster Menupunkt
            Cls
            Lowerline : Lcd "    ! Neustart !"              ' 1. Zeile Überschrift
         End If
      Case 71                                               ' Neustart
         Goto 0
   End Select

   If Interrupt = 1 Then                                    ' AS3935 Interrupt ausgelöst
      Interrupt = 0                                         ' Interrupt Reset
      'Disable Pcint2                                        ' AS3935 Interrupt ausschalten                                      '
      Waitms 2                                              ' wait 2ms before reading the interrupt register
      Gosub As3935_rd_int                                   ' Interrupt lesen
      Select Case As3935_int
         Case &B00000001                                    ' Noise level too high
            Set Led_rt : Led_an = 225 : Start Timer2        ' rote LED für 8 Sekunden einschalten
            If Ser_ausgabe = 1 Then                         ' serielle Ausgabe eingeschaltet
               Print "Noise_Floor_too_high"
            End If
            Sound Piezo , 200 , 333                         ' 50 mS, 4 kHz
            Ereignis = 14                                   ' Ereignis "Noise Floor too high"
            Esp_zusatz_1 = 255                              ' Ereignis Zusatz 1 zurück setzen
            If Menu = 1 Then : Menu = 3 : End If
         Case &B00000100                                    ' Disturber detected
            Set Led_rt : Led_an = 85 : Start Timer2         ' rote LED für 3 Sekunden einschalten
            Incr Disturber                                  ' Zähler Störungen gesamt erhöhen
            X = _min + 1                                    ' 1-60
            Incr Disturber_m(x)                             ' Zähler Störungen pro Minute erhöhen
            If Ser_ausgabe = 1 Then                         ' serielle Ausgabe eingeschaltet
               Print "Disturber " ; Disturber ; " detected"
            End If
            If Sound_dist >= 1 Then                         ' Tonausgabe bei Störung ein
               Sound Piezo , 200 , 333                      ' 50 mS, 4 kHz
            End If
            If Menu = 1 Then : Menu = 3 : End If
         Case &B00001000                                    ' Lightning interrupt
            Set Led_bl : Led_an = 30 : Start Timer2         ' blaue LED für 1 Sekunde einschalten
            Incr Lightning                                  ' Zähler Blitze erhöhen
            Incr Lightning_tx                               ' Zähler Blitze TX433 erhöhen
            If Lightning = 1 Then                           ' 1. Blitz
               Timestamp_blitz_1 = Syssec()                 ' Zeitstempel erster Blitz
            End If
            Timestamp_blitz_last = Syssec()                 ' Zeitstempel aktueller Blitz
            Gosub As3935_rd_s_lig                           ' Energy of the Single Lightning lesen
            Gosub As3935_rd_distance                        ' Distance estimation lesen
            If As3935_s_lig > Energie_tx Then               ' Maximum übernehmen
               Energie_tx = As3935_s_lig                    ' Energie TX433 übernehmen
            End If
            Distance_tx = Distance_tx + As3935_distance     ' Entfernung für Durchschnitt addieren
            If Ser_ausgabe = 1 Then                         ' serielle Ausgabe eingeschaltet
               Print "Lightning " ; Lightning ; " detected"
            End If
            If Sound_light >= 1 Then                        ' Tonausgabe bei Blitzen ein
               Sound Piezo , 400 , 333                      ' 100 mS, 4 kHz
            End If
            If Menu = 1 Then : Menu = 2 : End If
      End Select
      'Enable Pcint2
   End If

   'Empfang serielle Schnittstelle
   If Interrupt = 2 Then                                    ' Zeichen von UART empfangen
      Interrupt = 0                                         ' Interrupt Reset
      X = Len(rxstr_uart)                                   ' Länge des empfangenen Strings ermitteln
      If Right(rxstr_uart , 1) = "{013}" Then               ' letztes Zeichen RETURN
         Y = X - 1
         Rxstr_uart = Left(rxstr_uart , Y)                  ' letztes Zeichen (RETURN) entfernen
      End If
      Rxspl_uart = Split(rxstr_uart , Rxary_uart(1) , " ")  ' empfangenen String teilen
      Select Case Rxary_uart(1)                             ' Auswahl nach erstem Teilstring
         Case "all"                                         ' Einstellungen lesen
            Ser_ausgabe = 1                                 ' serielle Ausgabe einschalteten
            Gosub As3935_rd_afe_gb                          ' AFE Gain Boost lesen
            Gosub As3935_rd_nf_lev                          ' Noise Floor Level lesen
            Gosub As3935_rd_wdth                            ' Watchdog threshold lesen
            Gosub As3935_rd_min_num_ligh                    ' Minimum number of lightning lesen
            Gosub As3935_rd_srej                            ' Spike rejection lesen
            Gosub As3935_rd_mask_dist                       ' Mask Disturber lesen
         Case "allreg"                                      ' Register lesen
            Gosub As3935_rd_regall                          ' alle Register auslesen
         Case "Save_Settings"                               ' Benutzereinstellungen speichern
            Print "Save_User_Settings"
            As3935_afe_gb_eep = As3935_afe_gb               ' AFE Gain Boost in EEPROM speichern
            As3935_nf_lev_eep = As3935_nf_lev               ' Noise Floor Level in EEPROM speichern
            As3935_wdth_eep = As3935_wdth                   ' Watchdog threshold in EEPROM speichern
            As3935_min_num_ligh_eep = As3935_min_num_ligh   ' Minimum number of lightning in EEPROM speichern
            As3935_srej_eep = As3935_srej                   ' Spike rejection in EEPROM speichern
            As3935_mask_dist_eep = As3935_mask_dist         ' Mask Disturber in EEPROM speichern
            Print "User_Settings_Saved"
            Ereignis = 10                                   ' "Save Preset User"
            Esp_zusatz_1 = 255                              ' Wert zurück setzen
         Case "Load_Settings"                               ' Benutzereinstellungen laden
            Gosub As3935_ld_settings                        ' Benutzereinstellungen laden
            Ereignis = 8                                    ' "Load Preset User"
            Esp_zusatz_1 = 255                              ' Wert zurück setzen
         Case "Load_Default"                                ' Standardeinstellungen laden
            Gosub As3935_ld_default                         ' Load Default Preset
            Ereignis = 9                                    ' "Load Set Default"
            Esp_zusatz_1 = 255                              ' Wert zurück setzen
         Case "Reset"                                       ' Reset
            Goto 0                                          ' neu starten
         Case "AFE_GB"                                      ' AFE Gain Boost
            Esp_zusatz_1 = As3935_afe_gb                    ' alten Wert übernehmen
            As3935_afe_gb = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_afe_gb                    ' neuen Wert übernhemen
            Gosub As3935_wr_afe_gb                          ' AFE Gain Boost schreiben
            Ereignis = 2                                    ' Set AFE_GB
         Case "NF_LEV"                                      ' Noise Floor Level
            Esp_zusatz_1 = As3935_nf_lev                    ' alten Wert übernehmen
            As3935_nf_lev = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_nf_lev                    ' neuen Wert übernhemen
            Gosub As3935_wr_nf_lev                          ' Noise Floor Level schreiben
            Ereignis = 3                                    ' Set NF_LEVEL
         Case "WDTH"                                        ' Watchdog threshold
            Esp_zusatz_1 = As3935_wdth                      ' alten Wert übernehmen
            As3935_wdth = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_wdth                      ' neuen Wert übernhemen
            Gosub As3935_wr_wdth                            ' Watchdog threshold schreiben
            Ereignis = 5                                    ' Set WD_TRESHOLD
         Case "MIN_NUM_LIGH"                                ' Minimum Number of Lightning
            Esp_zusatz_1 = As3935_min_num_ligh              ' alten Wert übernehmen
            As3935_min_num_ligh = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_min_num_ligh              ' neuen Wert übernhemen
            Gosub As3935_wr_min_num_ligh                    ' Minimum Number of Lightning schreiben
            Ereignis = 4                                    ' Set MIN_NUM_LIGH
         Case "SREJ"                                        ' Spike rejection
            Esp_zusatz_1 = As3935_srej                      ' alten Wert übernehmen
            As3935_srej = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_srej                      ' neuen Wert übernhemen
            Gosub As3935_wr_srej                            ' Spike rejection schreiben
            Ereignis = 6                                    ' Set SPIKE_REJECT
         Case "MASK_DIST"                                   ' Mask Disturber
            Esp_zusatz_1 = As3935_mask_dist                 ' alten Wert übernehmen
            As3935_mask_dist = Val(rxary_uart(2))
            Esp_zusatz_2 = As3935_mask_dist                 ' neuen Wert übernhemen
            Gosub As3935_wr_mask_dist                       ' Mask Disturber schreiben
            Ereignis = 7                                    ' Set MASK_DISTURB
         Case "DATETIME"                                    ' Datum und Uhrzeit stellen
            Str8 = Left(rxary_uart(2) , 8)
            Date$ = Str8                                    ' Datum übernehmen
            Str8 = Left(rxary_uart(3) , 8)
            Time$ = Str8                                    ' Uhrzeit übernehmen
            Gosub Ds1307_wr_datetime                        ' Datum und Uhrzeit in DS1307 schreiben
            Menu = 1
            Ereignis = 20                                   ' Ereignis "Datum Uhrzeit von PC"
            Esp_zusatz_1 = 255                              ' Wert zurück setzen
         Case "ESP"                                         ' Ereignisspeicher seriell ausgeben
            Print "Datum;Uhrzeit;Ereignis;Zusatz 1;Zusatz 2"
            X = Esp_zeiger : Y = 0
            Do
               L = Ereignis_syssec_eep(x)                   ' Systemsekunden übernehmen
               Str8 = Date(l)                               ' Datum Ereigins übernehmen
               Str10 = Left(str8 , 6)                       ' Tag und Monat übernehmen
               Str10 = Str10 + "20"
               Str8 = Right(str8 , 2)
               Str10 = Str10 + Str8                         ' Jahr übernehmen
               Print Str10 ; ";";                           ' Datum ausgeben
               Print Time(l) ; ";" ;                        ' Uhrzeit ausgeben
               Z = Ereignis_eep(x)                          ' Ereignis aus EEPROM laden
               Str20 = Lookupstr(z , Ereignisse)            ' Ereignistext String
               Print Str20 ; ";" ;                          ' Ereignistext ausgeben
               Z = Esp_zusatz_1_eep(x)                      ' Zusatz 1 aus EEPROM laden
               Print Z ; ";";
               Z = Esp_zusatz_2_eep(x)                      ' Zusatz 2 aus EEPROM laden
               Print Z ; ";"
               Incr X : Incr Y
               If X >= 101 Then : X = X - 100 : End If
            Loop Until Y >= 100
         Case "GSP"                                         ' Speicher Gewitter seriell ausgeben
            Print "Datum;Uhrzeit;Dauer (min);Anzahl Blitze"
            X = Gewitter_zeiger : Y = 0
            Do
               L = Gewitter_syssec_eep(x)                   ' Systemsekunden übernehmen
               Str8 = Date(l)                               ' Datum Gewitter übernehmen
               Str10 = Left(str8 , 6)                       ' Tag und Monat übernehmen
               Str10 = Str10 + "20"
               Str8 = Right(str8 , 2)
               Str10 = Str10 + Str8                         ' Jahr übernehmen
               Print Str10 ; ";";                           ' Datum ausgeben
               Print Time(l) ; ";" ;                        ' Uhrzeit ausgeben
               W = Gewitter_dauer_eep(x)
               Print W ; ";" ;                              ' Dauer Gewitter ausgeben
               W = Gewitter_blitze_eep(x)
               Print W                                      ' Anzahl Blitze ausgeben
               Incr X : Incr Y
               If X >= 36 Then : X = X - 35 : End If        ' max. 35 Einträge
            Loop Until Y >= 35                              ' 35 Einträge
      End Select
      X = 0
      Do
         Incr X
         Rxary_uart(x) = ""                                 ' Strings löschen
      Loop Until X = Rxspl_uart                             ' Anzahl Strings erreicht
      Rxstr_uart = ""                                       ' zurück setzen
      Wertrefresh = 1
   End If                                                   ' ########## Ende Empfang seriell ##########

   'eigene Messwerte (Protokoll FS10 Sensor Strahlungsleistung) senden
   If Tx_sek >= Tx_abstand Then                             ' 155
      'Disable Pcint2                                        ' AS3935 Interrupt ausschalten                                      '
      Disable Urxc                                          ' Serial RX complete interrupt ausschalten
      Select Case Tx_sek
         Case 155
            D = Lightning_tx                                ' Wert Gewitter Anzahl Blitze übernehmen
            S_adresse = 4                                   ' Sensoradresse übernehmen
            Incr Tx_abstand                                 ' 156
         Case 156
            If Distance_tx >= 64 Then
               Distance_tx = Distance_tx - 63               ' Ausgangswert abziehen
               D = Distance_tx / Lightning_tx               ' Durchschnitt Gewitter Entfernung Blitze übernehmen
            Else
               D = Distance_tx
            End If
            Lightning_tx = 0                                ' Wert zurück setzen
            Distance_tx = 63                                ' Wert zurück setzen
            S_adresse = 2                                   ' Sensoradresse übernehmen
            Incr Tx_abstand                                 ' 157
         Case 157
            D = Energie_tx                                  ' Wert Gewitter-Energie übernehmen
            Energie_tx = 0                                  ' Wert zurück setzen
            S_adresse = 0                                   ' Sensoradresse übernehmen
            Tx_sek = 0
            Tx_abstand = 155
      End Select
      Select Case D
         Case 0 To 4095                                     ' es werden 12 Bit gesendet
            Potenz = 0                                      ' 0 = Faktor 1
         Case 4096 To 40950
            Potenz = 1                                      ' 1 = Faktor 10
            D = D / 10
         Case 40951 To 409500
            Potenz = 2                                      ' 2 = Faktor 100
            D = D / 100
         Case Else
            Potenz = 3                                      ' 3 = Faktor 1000
            D = D / 1000
      End Select
      Ar(1) = 6                                             ' Sensortyp 6 - Strahlungsleistung
      Ar(2) = S_adresse                                     ' Sensoradresse übernehmen
      W = Loww(d)                                           ' untere 16 Bit übernehmen
      X = Low(w)                                            ' untere 8 bit aus Helligkeit übernehmen
      Ar(3) = X And &B00001111                              ' LSN Bit 0-3 übernehmen (obere 4 Bit auf 0 setzen)
      Shift X , Right , 4                                   ' obere 4 Bit nach rechts schieben
      Ar(4) = X                                             ' MID Bit 4-7 übernehmen
      X = High(w)                                           ' obere 8 bit aus Word übernehmen
      Ar(5) = X And &B00001111                              ' MSN Bit 8-11 übernehmen (obere 4 Bit auf 0 setzen)
      Ar(6) = Potenz                                        ' Faktor übernehmen
      Gosub Tx_433_send                                     ' Daten senden
      'Enable Pcint2                                         ' AS3935 Interrupt einschalten
      Enable Urxc                                           ' Serial RX complete interrupt einschalten
      'If As3935int = 1 Then                                 ' Eingang high
      '   Interrupt = 1                                      ' Interrupt ausgelöst
      'End If
   End If

   If Menu = 1 Then
      If Dcf_status.4 = 0 Then                              ' This Bit indicated that the DCF-Part is stopped
         Led_rt = Dcf_status.0                              ' LED rot zeigt DCF-Empfang
      End If
   End If

   'jede Sekunde einmal ausführen
   If Sec_old1 <> _sec Then                                 ' ########## einmal pro Sekunde ausführen ##########
      Sec_old1 = _sec
      Incr Tx_sek                                           ' Zähler Sekunden Messwerte senden erhöhen
      If Menuverlassen >= 1 Then                            ' Untermenu verlassen nach "Menuverlassen" Sekunden
         Decr Menuverlassen                                 ' Sekunden bis verlassen herunter zählen
         If Menuverlassen = 0 Then
            Menu = 1                                        ' Homescreen Uhrzeit
            Wertmax = 0 : Esp_zusatz_1 = 255
         End If
      End If
      If Lightning >= 1 Then                                ' ########## Gewitter in Speicher schreiben ##########
         Gewitter_alter = Syssec() - Timestamp_blitz_last   ' Alter letzter Blitz errechnen
         If Gewitter_alter >= 1800 Then                     ' letzter Blitz vor 30 Minuten
            Decr Gewitter_zeiger                            ' Zeiger verringern
            If Gewitter_zeiger = 0 Then : Gewitter_zeiger = 35 : End If       ' 35 Gewitter speichern
            Gewitter_zeiger_eep = Gewitter_zeiger           ' Zeiger in EEPROM speichern
            Gewitter_dauer_s = Timestamp_blitz_last - Timestamp_blitz_1       ' Dauer Gewitter errechnen
            Gewitter_dauer_m = Gewitter_dauer_s / 60        ' Dauer Gewitter Minuten errechnen
            Gewitter_dauer_eep(gewitter_zeiger) = Gewitter_dauer_m       ' Dauer Gewitter in EEPROM speichern
            Gewitter_blitze_eep(gewitter_zeiger) = Lightning       ' Anzahl Blitze in EEPROM speichern
            Gewitter_syssec_eep(gewitter_zeiger) = Timestamp_blitz_1       ' Zeitstempel 1. Blitz in EEPROM speichern
            Lightning = 0                                   ' Zähler Blitze zurück setzen
            As3935_distance = 63                            ' Entfernung zurück setzen
            As3935_s_lig = 0                                ' Energie zurück setzen
            'Gosub As3935_clear_statistics                   ' clear the statistics by toggling the bit (high-low-high)
         End If
      End If
      Select Case _sec
         Case 0                                             ' ########## Sekunde 0 ##########
            'Umschaltung Winter- auf Sommerzeit
            If Wt = 6 Then                                  ' Sonntag
               If _day >= 25 Then                           ' letztes Wochenende im Monat
                  If _hour = 2 Then
                     If _month = 3 Then                     ' März
                        If Dst = 1 Then                     ' Winterzeit
                           Incr _hour                       ' Stunde +1
                           Dst = 2 : Dst_eram = 2           ' Umschaltung von Winter- auf Sommerzeit
                           Gosub Ds1307_wr_datetime         ' Datum und Uhrzeit in RTC schreiben
                           Ereignis = 12                    ' Sommerzeit
                           Esp_zusatz_1 = 255               ' Ereignis Zusatz 1 zurück setzen
                        End If
                     End If
                  End If
                  If _hour = 3 Then
                     If _month = 10 Then                    ' Oktober
                        If Dst = 2 Then                     ' Sommerzeit
                           Decr _hour                       ' Stunde -1
                           Dst = 1 : Dst_eram = 1           ' Umschaltung von Sommer- auf Winterzeit
                           Gosub Ds1307_wr_datetime         ' Datum und Uhrzeit in RTC schreiben
                           Ereignis = 13                    ' Winterzeit
                           Esp_zusatz_1 = 255               ' Ereignis Zusatz 1 zurück setzen
                        End If
                     End If
                  End If
               End If
            End If
            X = _min + 1
            Disturber_m(x) = 0                              ' Zähler Störungen aktuelle Minute zurück setzen
            If _hour = 0 Then                               ' Mitternacht
               If _min = 0 Then                             ' Mitternacht
                  Disturber = 0                             ' Zähler Störungen zurück setzen
               End If
            End If
         Case 30                                            ' ########## Sekunde 30 ##########
            If Dcf_status.7 = 1 Then                        ' Bit 7 zeigt empfangenes DCF-Zeitsignal
               Reset Dcf_status.7                           ' Bit 7 DCF-Status zurück setzen
               Dst = Dcf77timezone()                        ' Zeitzone übernehmen
               Dst_eram = Dst                               ' Zeitzone in EEPROM speichern
               Gosub Ds1307_wr_datetime                     ' Datum und Uhrzeit in DS1307 schreiben
               Ereignis = 11                                ' Ereignis DCF-Empfang OK
               Esp_zusatz_1 = 255                           ' Ereignis Zusatz 1 zurück setzen
               Reset Led_rt                                 ' LED rot ausschalten
            End If
      End Select
   End If

   If Ereignis >= 1 Then                                    ' ########## Ereignis in Speicher schreiben ##########
      Decr Esp_zeiger                                       ' Zeiger verringern
      If Esp_zeiger = 0 Then : Esp_zeiger = 100 : End If    ' 100 Ereignisse
      Esp_zeiger_eep = Esp_zeiger                           ' Zeiger in EEPROM speichern
      Ereignis_eep(esp_zeiger) = Ereignis                   ' Ereignis in EEPROM speichern
      If Esp_zusatz_1 = 255 Then
         Esp_zusatz_2 = 255                                 ' Ereignis Zusatz zurück setzen
      End If
      Esp_zusatz_1_eep(esp_zeiger) = Esp_zusatz_1           ' Ereignis Zusatz in EEPROM speichern
      Esp_zusatz_2_eep(esp_zeiger) = Esp_zusatz_2           ' Ereignis Zusatz in EEPROM speichern
      L = Syssec()                                          ' Systemsekunden übernehmen
      Ereignis_syssec_eep(esp_zeiger) = L                   ' Systemsekunden in EEPROM speichern
      Ereignis = 0                                          ' Ereignis zurück setzen
      Esp_zusatz_1 = 255                                    ' Ereignis Zusatz zurück setzen
      Esp_zusatz_2 = 255                                    ' Ereignis Zusatz zurück setzen
   End If

   If Led_count >= Led_an Then
      Led_count = 0
      Stop Timer2
      Reset Led_rt                                          ' LED rot ausschalten
      Reset Led_bl                                          ' LED blau ausschalten
   End If

'Waitms 10
'Config Powermode = Standby
Loop

End

'#################### Interruptroutinen ####################
Int_rx_uart:                                                ' Empfang serielle Schnittstelle
   Rxchar_uart = Udr                                        ' read UDR only once
   Rxstr_uart = Rxstr_uart + Chr(rxchar_uart)               ' String ergänzen
   If Rxchar_uart = 13 Then                                 ' RETURN
      Interrupt = 2                                         ' Interrupt ausgelöst
   End If
Return

Pcint2_int:                                                 ' Pin Change Interrupt 2
   If As3935int = 1 Then                                    ' Eingang high
      Interrupt = 1                                         ' Interrupt ausgelöst
   End If
Return

Tmr0_isr:                                                   ' Torzeit für Frequenzzähler
   Timer0 = Tmr0_preload                                    ' 20 mS @ 7,3728 MHz
   Incr X
Return

Tmr2_isr:                                                   ' Rückstellung LED, Interrupt alle 35 mSek
   Incr Led_count
Return

'#################### Unterprogramme ####################
'senden dauert: 62,22 mS (Helligkeit), 74,42 mS (Thermo/Hygro)
Tx_433_send:
   Tx_dbl = 0                                               ' alle Bits auf 0 setzen
   X = 10                                                   ' 10 Bit Präambel
   Do                                                       ' Bit 10 bis Anzahl Step 5 muß 1 sein
      Tx_dbl.x = 1                                          ' Bit auf 1 setzen
      X = X + 5                                             ' Array beginnt bei 1
   Loop Until X >= 51                                       ' Helligkeit (S2500H)

   'Bits übernehmen
   Check = 0                                                ' Checksumme zurück setzen
   Ersumme = 0                                              ' Prüfsumme zurück setzen
   Tx_byte_nr = 0
   Do                                                       ' Sensortyp, Adresse und Werte senden
      Incr Tx_byte_nr                                       ' beginnt mit 1
      Tx_byte = Ar(tx_byte_nr)                              ' Byte übernehmen
      Gosub Tx_433_byte                                     ' Bit 0-3 übernehmen
   Loop Until Tx_byte_nr >= 6                               ' Anzahl zu sendende Bytes fertig
   Incr Tx_byte_nr                                          ' nächstes Byte
   Tx_byte = Check                                          ' Checkbyte übernehmen
   Gosub Tx_433_byte                                        ' Byte übernehmen
   Ersumme = Ersumme + 5                                    ' Prüfsumme errechnen
   Tx_byte = Ersumme And &B00001111                         ' obere 4 Bit auf 0 setzen
   Incr Tx_byte_nr                                          ' nächstes Byte
   Gosub Tx_433_byte                                        ' Byte senden
   'Bits senden
   X = 0                                                    ' Beginne mit Bit 0
   Do
      If Tx_dbl.x = 1 Then                                  ' 1 senden
         Set Tx433                                          ' Ausgang high
         Waitus 366                                         ' 366 µS warten
         Reset Tx433                                        ' Ausgang low
         Waitus 854                                         ' 854 µS warten
         'Waitus 780                                         ' 854 µS warten, Rest braucht Programm @ 1 MHz
      Else                                                  ' 0 senden
         Set Tx433                                          ' Ausgang high
         Waitus 854                                         ' 854 µS warten
         Reset Tx433                                        ' Ausgang low
         Waitus 366                                         ' 366 µS warten
         'Waitus 304                                         ' 366 µS warten, Rest braucht Programm @ 1 MHz
      End If
      Incr X                                                ' nächstes Bit
   Loop Until X >= 51                                       ' Ende mit gesetzter Anzahl Bits
Return

Tx_433_byte:
   Tx_bit_nr = Tx_byte_nr * 5                               ' 5, 10, 15...
   Tx_bit_nr = Tx_bit_nr + 6                                ' 11, 16, 21...
   X = 0
   Do
      Tx_dbl.tx_bit_nr = Tx_byte.x                          ' Bit aus Byte übernehmen
      Incr Tx_bit_nr : Incr X                               ' nächstes Bit
   Loop Until X >= 4                                        ' 4 Bit
   Check = Check Xor Tx_byte                                ' Check
   Ersumme = Ersumme + Tx_byte                              ' Prüfsumme bilden
Return

Lcd_info:                                                   ' Anzeige Info
   Upperline : Lcd "Gewitter Sensor GS15"                   ' 1. Zeile
   Lowerline : Lcd "Detektor AMS AS3935"                    ' 2. Zeile
   Thirdline : Lcd "Version: " ; Version(2)                 ' 3. Zeile
   Fourthline : Lcd Version(1)                              ' 4. Zeile
Return

Lcd_ein_aus:                                                ' Anzeige ein/aus
   Locate 4 , 9
   If Wert = 0 Then
      Lcd "aus"                                             ' 4.Zeile
   Else
      Lcd "ein"                                             ' 4.Zeile
   End If
   Locate 4 , 9                                             ' Cursor anzeigen
Return

Taste_l:                                                    ' Taste links
   If Menu >= 10 Then
      Menu = Menuback                                       ' zurück zum vorhergehenden Menu
   Elseif Menu <> 0 Then
      Menu = 1                                              ' Homescreen Datum, Uhrzeit und Temperatur
   End If
   Wertrefresh = 1 : Wertmax = 0 : Esp_zusatz_1 = 255
Return

Taste_r:
   If Menunext >= 1 Then                                    ' Taste rechts
      Menu = Menunext                                       ' nächstes Untermenu
   End If
   Wertrefresh = 1 : Wertmax = 0 : Esp_zusatz_1 = 255
Return

Taste_o:                                                    ' Taste oben
   If Wertmax >= 1 Then
      Esp_zusatz_1 = Wert
      Incr Wert
      If Wertmax < Wert Then
         Wert = 0
      End If
      Esp_zusatz_2 = Wert
   Elseif Menubeginn < Menu Then                            ' Taste oben nicht am Menubeginn
      Decr Menu
   End If
   Wertrefresh = 1
Return

Taste_u:                                                    ' Taste unten
   If Wertmax >= 1 Then
      Esp_zusatz_1 = Wert
      Decr Wert
      If Wertmax < Wert Then
         Wert = Wertmax
      End If
      Esp_zusatz_2 = Wert
   Elseif Menu < Menuende Then                              ' Taste unten nicht am MenuenDe
      Incr Menu
   End If
   Wertrefresh = 1
Return

Ser_set_baud:                                               ' Einstellung Übertragungsrate serielle Schnittstelle
   Select Case Ser_baudindex
      Case 0
         Baud = 2400
      Case 1
         Baud = 4800
      Case 2
         Baud = 9600
      Case 3
         Baud = 19200
      Case 4
         Baud = 38400
      Case 5
         Baud = 57600
      Case 6
         Baud = 115200
      Case 7
         Baud = 230400
      Case 8
         Baud = 460800
   End Select
Return

Ds1621_read:
   I2cstart                                                 ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_w                                    ' Address of DS1621 Thermometer for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &HAA                                            ' Temperaturmessung Lesekommando
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_r                                    ' Address of DS1621 Thermometer for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte Ds_msb , Ack                                    ' MSB holen
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte Ds_lsb , Nack                                   ' LSB holen
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstart                                                 ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_w                                    ' Address of DS1621 Thermometer for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &HA8                                            ' Count Remain anfordern
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_r                                    ' Address of DS1621 Thermometer for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte C_r , Nack                                      ' Count Remain holen
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstart                                                 ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_w                                    ' Address of DS1621 Thermometer for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &HA9                                            ' Count per C anfordern
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' I2C-Kommunikation starten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1621_adr_r                                    ' Address of DS1621 Thermometer for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte C_p_c , Nack                                    ' Count per C holen
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I_low = Ds_msb
   If I_low.7 = 1 Then                                      ' negativ
      I_high = 255
   Else
      I_high = 0
   End If
   I = I * 100 : I = I - 25
   I1 = C_p_c - C_r
   I1 = I1 * 100
   I1 = I1 / C_p_c
   Temperatur = I + I1
   Temperatur = Temperatur / 10
Return

Ds1307_wr_datetime:                                         ' Datum und Uhrzeit in DS1307 schreiben
   I2cstart                                                 ' Generate start code
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1307_adr_w                                    ' Write Addresses of DS1307 clock
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &H00                                            ' starting address in DS1307
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_sec)                                        ' seconds
   I2cwbyte X                                               ' write second
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_min)                                        ' minuts
   I2cwbyte X                                               ' write minut
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_hour)                                       ' hours
   I2cwbyte X                                               ' write hour
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(wt)                                          ' weekday
   I2cwbyte X                                               ' write weekday
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_day)                                        ' days
   I2cwbyte X                                               ' write day
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_month)                                      ' months
   I2cwbyte X                                               ' write month
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   X = Makebcd(_year)                                       ' year
   I2cwbyte X                                               ' write year
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
Return

Ds1307_rd_datetime:                                         ' Datum und Uhrzeit aus DS1307 laden
   I2cstart                                                 ' Generate start code
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1307_adr_w                                    ' Write Addresses of Ds1307 clock
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &H00                                            ' start address in DS1307
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte Ds1307_adr_r                                    ' Read Addresses of Ds1307 clock
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte X , Ack                                         ' Sekunde
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _sec = Makedec(x)
   I2crbyte X , Ack                                         ' Minute
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _min = Makedec(x)
   I2crbyte X , Ack                                         ' Stunde
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _hour = Makedec(x)
   I2crbyte X , Ack                                         ' Wochentag
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   Wt = Makedec(x)
   I2crbyte X , Ack                                         ' Tag
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _day = Makedec(x)
   I2crbyte X , Ack                                         ' Monat
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _month = Makedec(x)
   I2crbyte X , Nack                                        ' Jahr
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   _year = Makedec(x)
   I2cstop
Return

As3935_rd_afe_gb:                                           ' AFE Gain Boost lesen
   As3935_reg = &H00                                        ' Register 0x00
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_afe_gb = As3935_data And &B00111110               ' Bit 1-5 maskieren
   Shift As3935_afe_gb , Right , 1
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "AFE_GB: " ; As3935_afe_gb
   End If
Return

As3935_rd_pwd:                                              ' Power-down lesen
   As3935_reg = &H00                                        ' Register 0x00
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_pwd = As3935_data And &B00000001                  ' Bit 0 maskieren
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "PWD: " ; As3935_pwd
   End If
Return

As3935_rd_nf_lev:                                           ' Noise Floor Level lesen
   As3935_reg = &H01                                        ' Register 0x01
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_nf_lev = As3935_data And &B01110000               ' Bit 4-6 maskieren
   Shift As3935_nf_lev , Right , 4
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "NF_LEV: " ; As3935_nf_lev
   End If
Return

As3935_rd_wdth:                                             ' Watchdog threshold lesen
   As3935_reg = &H01                                        ' Register 0x01
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_wdth = As3935_data And &B00001111                 ' Bit 0-3 maskieren
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "WDTH: " ; As3935_wdth
   End If
Return

As3935_rd_cl_stat:                                          ' Clear statistics lesen
   As3935_reg = &H02                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_cl_stat = As3935_data.6                           ' Bit 6 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "CL_STAT: " ; As3935_cl_stat
   End If
Return

As3935_rd_min_num_ligh:                                     ' Minimum number of lightning lesen
   As3935_reg = &H02                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_min_num_ligh = As3935_data And &B00110000         ' Bit 4-5 maskieren
   Shift As3935_min_num_ligh , Right , 4
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "MIN_NUM_LIGH: " ; As3935_min_num_ligh
   End If
Return

As3935_rd_srej:                                             ' Spike rejection lesen
   As3935_reg = &H02                                        ' Register 0x01
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_srej = As3935_data And &B00001111                 ' Bit 0-3 maskieren
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "SREJ: " ; As3935_srej
   End If
Return

As3935_rd_lco_fdiv:                                         ' Frequency division ration for antenna tuning lesen
   As3935_reg = &H03                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_lco_fdiv = As3935_data And &B11000000             ' Bit 6-7 maskieren
   Shift As3935_lco_fdiv , Right , 6
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "LCO_FDIV: " ; As3935_lco_fdiv
   End If
Return

As3935_rd_mask_dist:                                        ' Mask Disturber lesen
   As3935_reg = &H03                                        ' Register 0x03
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_mask_dist = As3935_data.5                         ' Bit 5 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "MASK_DIST: " ; As3935_mask_dist
   End If
Return

As3935_rd_int:                                              ' Interrupt lesen
   As3935_reg = &H03                                        ' Register 0x03
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_int = As3935_data And &B00001111                  ' Bit 0-3 Interrupt maskieren
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "INT: " ; As3935_int
   End If
Return

As3935_rd_s_lig:                                            ' Energy of the Single Lightning lesen
   As3935_reg = &H06                                        ' Register 0x06 (MMSBYTE)
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_s_lig = As3935_data And &B00011111                ' Bit 0-4 maskieren
   Shift As3935_s_lig , Left , 8
   As3935_reg = &H05                                        ' Register 0x05 (MSBYTE)
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_s_lig = As3935_s_lig + As3935_data
   Shift As3935_s_lig , Left , 8
   As3935_reg = &H04                                        ' Register 0x04 (LSBYTE)
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_s_lig = As3935_s_lig + As3935_data
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "S_LIG: " ; As3935_s_lig
   End If
Return

As3935_rd_distance:                                         ' Distance estimation lesen
   As3935_reg = &H07                                        ' Register 0x07
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_distance = As3935_data And &B00111111             ' Bit 0-5 maskieren
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "DISTANCE: " ; As3935_distance
   End If
Return

As3935_wr_afe_gb:                                           ' AFE Gain Boost schreiben
   As3935_reg = &H00                                        ' Register 0x00
   Gosub As3935_rd_reg                                      ' Register auslesen
   X = As3935_afe_gb
   Shift X , Left , 1
   Y = 1                                                    ' Beginn mit Bit 1
   Do
      As3935_data.y = X.y                                   ' Bit 1-5 übernehmen
      Incr Y
   Loop Until Y > 5                                         ' Ende bei Bit 5
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x00 = As3935_data                               ' Register 0x00 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "AFE_GB: " ; As3935_afe_gb
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_wr_nf_lev:                                           ' Noise Floor Level schreiben
   As3935_reg = &H01                                        ' Register 0x01
   Gosub As3935_rd_reg                                      ' Register auslesen
   X = As3935_nf_lev
   Shift X , Left , 4
   Y = 4                                                    ' Beginn mit Bit 4
   Do
      As3935_data.y = X.y                                   ' Bit 4-6 übernehmen
      Incr Y
   Loop Until Y > 6                                         ' Ende bei Bit 6
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x01 = As3935_data                               ' Register 0x01 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "NF_LEV: " ; As3935_nf_lev
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_wr_wdth:                                             ' Watchdog threshold schreiben
   As3935_reg = &H01                                        ' Register 0x01
   Gosub As3935_rd_reg                                      ' Register auslesen
   Y = 0                                                    ' Beginn mit Bit 0
   Do
      As3935_data.y = As3935_wdth.y                         ' Bit 0-3 übernehmen
      Incr Y
   Loop Until Y > 3                                         ' Ende bei Bit 3
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x01 = As3935_data                               ' Register 0x01 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "WDTH: " ; As3935_wdth
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_wr_min_num_ligh:                                     ' Minimum Number of Lightning schreiben
   As3935_reg = &H02                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_data.4 = As3935_min_num_ligh.0                    ' Bit 4 übernehmen
   As3935_data.5 = As3935_min_num_ligh.1                    ' Bit 5 übernehmen
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x02 = As3935_data                               ' Register 0x02 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "MIN_NUM_LIGH: " ; As3935_min_num_ligh
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_clear_statistics:                                    ' clear the statistics by toggling the bit (high-low-high)
   As3935_reg = &H02                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_data.6 = 1                                        ' clear the statistics by toggling the bit REG0x02[6]
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_data.6 = 0                                        ' clear the statistics by toggling the bit REG0x02[6]
   Gosub As3935_wr_byte                                     ' Register schreiben
   Waitms 15
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_data.6 = 1                                        ' clear the statistics by toggling the bit REG0x02[6]
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
Return

As3935_wr_srej:                                             ' Spike rejection schreiben
   As3935_reg = &H02                                        ' Register 0x02
   Gosub As3935_rd_reg                                      ' Register auslesen
   Y = 0                                                    ' Beginn mit Bit 0
   Do
      As3935_data.y = As3935_srej.y                         ' Bit 0-3 übernehmen
      Incr Y
   Loop Until Y > 3                                         ' Ende bei Bit 3
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x02 = As3935_data                               ' Register 0x02 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "SREJ: " ; As3935_srej
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_wr_mask_dist:                                        ' Mask Disturber schreiben
   As3935_reg = &H03                                        ' Register 0x03
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_data.5 = As3935_mask_dist.0                       ' Bit 0 übernehmen
   Gosub As3935_wr_byte                                     ' Register schreiben
   Gosub As3935_rd_reg                                      ' Register auslesen
   As3935_r0x03 = As3935_data                               ' Register 0x03 übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "MASK_DIST: " ; As3935_mask_dist
   End If
   Gosub As3935_rd_int                                      ' Interrupt lesen
Return

As3935_rd_trco:                                             ' Calibration of TRCO auslesen
   Gosub As3935_disp_trco                                   ' Timer RCO (TRCO) 32.768 Khz Is Displayed On The Irq Pin
   Cls : Upperline : Lcd "TRCO Calibrating"                 ' 1. Zeile
   Set Led_bl                                               ' LED blau einschalten
   Tmr0_preload = &H70                                      ' 20 mS @ 7,3728 MHz
   Config Timer0 = Timer , Prescale = 1024                  ' Timer 0 Torzeit für Frequenzzähler
   Start Timer0 : Start Timer1                              ' Timer starten
   Timer1 = 0 : Timer0 = Tmr0_preload                       ' Timer zurück setzen
   X = 0 : Do : Loop Until X >= 50                          ' 1000 mSec warten (Timer 0 erhöht X alle 20 mS)
   W = Timer1                                               ' Zaehlerstand Timer 1 übernehmen
   Stop Timer0 : Stop Timer1                                ' Timer stoppen
   Gosub As3935_disp_rco_off                                ' Displayed Frequenzy On The Irq Pin Off
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "TRCO Frequenz: " ; W ; " Hz"
   End If
   Str10 = Str(w)
   Lowerline : Lcd "TRCO: " ; Format(str10 , "00.000") ; " Khz"       ' 2. Zeile
   As3935_reg = &H3A                                        ' Calibration of TRCO
   Gosub As3935_rd_reg                                      ' ein Register auslesen
   As3935_r0x3a = As3935_data                               ' in Register übernehmen
   X = As3935_r0x3a And &B11000000                          ' Bit 6 und 7 maskieren
   Y = X
   Shift X , Right , 6
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "TRCO Data: " ; X
   End If
   Select Case Y
      Case &B00000000                                       ' Calibration of TRCO not done
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "TRCO Calibrating Missing"
         End If
         Thirdline : Lcd "Calibrating"                      ' 3. Zeile
         Fourthline : Lcd "Missing"                         ' 4. Zeile
         Ereignis = 18                                      ' Ereignis "TRCO Calibr. Missing"
         Esp_zusatz_1 = 255                                 ' Wert zurück setzen
      Case &B10000000                                       ' Calibration of TRCO done, successful
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "TRCO Calibrating OK"
         End If
         Thirdline : Lcd "Calibrating OK"                   ' 3. Zeile
         Reset Led_bl                                       ' LED blau ausschalten
      Case &B11000000                                       ' Calibration of TRCO done, not successful
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "TRCO Calibrating Error"
         End If
         Thirdline : Lcd "Calibrating"                      ' 3. Zeile
         Fourthline : Lcd "Error"                           ' 4. Zeile
         Ereignis = 16                                      ' Ereignis "TRCO Calibrating NOK"
         Esp_zusatz_1 = 255                                 ' Wert zurück setzen
   End Select
   Wait 1
Return

As3935_rd_srco:                                             ' Calibration of SRCO auslesen
   Gosub As3935_disp_srco                                   ' System RCO (SRCO) 1.125 MHz Is Displayed On The Irq Pin
   Cls : Upperline : Lcd "SRCO Calibrating"                 ' 1. Zeile
   Set Led_bl                                               ' LED blau einschalten
   Tmr0_preload = &H70                                      ' 20 mS @ 7,3728 MHz
   Config Timer0 = Timer , Prescale = 1024                  ' Timer 0 Torzeit für Frequenzzähler
   Start Timer0 : Start Timer1                              ' Timer starten
   Timer1 = 0 : Timer0 = Tmr0_preload                       ' Timer zurück setzen
   X = 0 : Do : Loop Until X >= 2                           ' 40 mSec warten (Timer 0 erhöht X alle 20 mS)
   W = Timer1                                               ' Zaehlerstand Timer 1 übernehmen
   Stop Timer0 : Stop Timer1                                ' Timer stoppen
   Gosub As3935_disp_rco_off                                ' Displayed Frequenzy On The Irq Pin Off
   D = W * 25
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "SRCO Frequenz: " ; D ; " Hz"
   End If
   D = D / 100
   Str10 = Str(d)
   Lowerline : Lcd "SRCO: " ; Format(str10 , "0.0000") ; " Mhz"       ' 2. Zeile
   As3935_reg = &H3B                                        ' Calibration of SRCO
   Gosub As3935_rd_reg                                      ' ein Register auslesen
   As3935_r0x3b = As3935_data                               ' in Register übernehmen
   X = As3935_r0x3b And &B11000000                          ' Bit 6 und 7 maskieren
   Y = X
   Shift X , Right , 6
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "SRCO Data: " ; X
   End If
   Select Case Y
      Case &B00000000                                       ' Calibration of SRCO not done
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "SRCO Calibrating Missing"
         End If
         Thirdline : Lcd "Calibrating"                      ' 3. Zeile
         Fourthline : Lcd "Missing"                         ' 4. Zeile
         Ereignis = 19                                      ' Ereignis "SRCO Calibr. Missing"
         Esp_zusatz_1 = 255                                 ' Wert zurück setzen
      Case &B10000000                                       ' Calibration of SRCO done, successful
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "SRCO Calibrating OK"
         End If
         Thirdline : Lcd "Calibrating OK"                   ' 3. Zeile
         Reset Led_bl                                       ' LED blau ausschalten
      Case &B11000000                                       ' Calibration of SRCO done, not successful
         If Ser_ausgabe = 1 Then                            ' serielle Ausgabe eingeschaltet
            Print "SRCO Calibrating Error"
         End If
         Thirdline : Lcd "Calibrating"                      ' 3. Zeile
         Fourthline : Lcd "Error"                           ' 4. Zeile
         Ereignis = 17                                      ' Ereignis "SRCO Calibrating NOK"
         Esp_zusatz_1 = 255                                 ' Wert zurück setzen
   End Select
   Wait 1
Return

As3935_ld_settings:                                         ' Benutzereinstellungen laden
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "Load_User_Settings"
   End If
   As3935_afe_gb = As3935_afe_gb_eep                        ' AFE Gain Boost aus EEPROM laden
   If As3935_afe_gb > 31 Then
      As3935_afe_gb = 18 : As3935_afe_gb_eep = 18           ' Default Preset
   End If
   Gosub As3935_wr_afe_gb                                   ' AFE Gain Boost schreiben
   As3935_nf_lev = As3935_nf_lev_eep                        ' Noise Floor Level aus EEPROM laden
   If As3935_nf_lev > 7 Then
      As3935_nf_lev = 2 : As3935_nf_lev_eep = 2             ' Default Preset
   End If
   Gosub As3935_wr_nf_lev                                   ' Noise Floor Level schreiben
   As3935_wdth = As3935_wdth_eep                            ' Watchdog threshold aus EEPROM laden
   If As3935_wdth > 15 Then
      As3935_wdth = 2 : As3935_wdth_eep = 2                 ' Default Preset
   End If
   Gosub As3935_wr_wdth                                     ' Watchdog threshold schreiben
   As3935_min_num_ligh = As3935_min_num_ligh_eep            ' Minimum number of lightning aus EEPROM laden
   If As3935_min_num_ligh > 3 Then
      As3935_min_num_ligh = 0 : As3935_min_num_ligh_eep = 0 ' Default Preset
   End If
   Gosub As3935_wr_min_num_ligh                             ' Minimum number of lightning schreiben
   As3935_srej = As3935_srej_eep                            ' Spike rejection aus EEPROM laden
   If As3935_srej > 15 Then
      As3935_srej = 2 : As3935_srej_eep = 2                 ' Default Preset
   End If
   Gosub As3935_wr_srej                                     ' Spike rejection schreiben
   As3935_mask_dist = As3935_mask_dist_eep                  ' Mask Disturber aus EEPROM laden
   If As3935_mask_dist > 1 Then
      As3935_mask_dist = 0 : As3935_mask_dist_eep = 0       ' Default Preset
   End If
   Gosub As3935_wr_mask_dist                                ' Mask Disturber schreiben
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "User_Settings_Loaded"
   End If
Return

As3935_ld_default:                                          ' Load Default Preset
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "Load_Default_Preset"
   End If
   As3935_reg = &H3C                                        ' PRESET_DEFAULT
   As3935_data = &H96                                       ' Send Direct Command Byte
   Gosub As3935_wr_byte
   Gosub As3935_rd_afe_gb                                   ' AFE Gain Boost lesen
   Gosub As3935_rd_nf_lev                                   ' Noise Floor Level lesen
   Gosub As3935_rd_wdth                                     ' Watchdog threshold lesen
   Gosub As3935_rd_min_num_ligh                             ' Minimum number of lightning lesen
   Gosub As3935_rd_srej                                     ' Spike rejection lesen
   Gosub As3935_rd_mask_dist                                ' Mask Disturber lesen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "Default_Preset_Loaded"
   End If
Return

As3935_cal_lco:                                             ' LCO Calibration Start
   Disable Pcint2                                           ' AS3935 Interrupt ausschalten
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "LCO Calibrating Start"
   End If
   Cls : Upperline : Lcd "LCO Calibrating"                  ' 1. Zeile
   Lowerline : Lcd "LCO Tune Cap:"                          ' 2. Zeile
   Thirdline : Lcd "LCO:         kHz"                       ' 3. Zeile
   As3935_tun_cap = 0 : W1 = 0 : W2 = 0
   Tmr0_preload = &H70                                      ' 20 mS @ 7,3728 MHz
   Config Timer0 = Timer , Prescale = 1024                  ' Timer 0 Torzeit für Frequenzzähler
   Start Timer0 : Start Timer1                              ' Timer starten
   Do
      Gosub As3935_disp_lco                                 ' Antenna's Resonance Frequency (LCO) Is Displayed On The Irq Pin
      Toggle Led_rt                                         ' LED rot blinkt
      Timer1 = 0 : Timer0 = Tmr0_preload                    ' Timer zurück setzen
      X = 0 : Do : Loop Until X >= 50                       ' 1000 mSec warten (Timer 0 erhöht X alle 20 mS)
      W = Timer1                                            ' Zaehlerstand Timer 1 übernehmen
      If Ser_ausgabe = 1 Then                               ' serielle Ausgabe eingeschaltet
         Print "LCO TUN_CAP: " ; As3935_tun_cap
      End If
      Locate 2 , 16 : Lcd As3935_tun_cap ; " "              ' 2. Zeile
      D = W * 16                                            ' Frequenz errechnen
      If Ser_ausgabe = 1 Then                               ' serielle Ausgabe eingeschaltet
         Print "LCO Frequenz: " ; D ; " Hz"
      End If
      Str10 = Str(d) : Locate 3 , 6 : Lcd Format(str10 , "000.000")       ' 3. Zeile
      If W = 31250 Then                                     ' Sollfrequenz 31,250 kHz * 16 = 500 kHz
         Exit Do
      Elseif W > 31250 Then                                 ' Frequenz zu hoch
         W1 = W - 31250                                     ' 1. Differenz Frequenz
      Else                                                  ' Frequenz zu niedrig
         W2 = 31250 - W                                     ' 2. Differenz Frequenz
         If W1 <= W2 Then                                   ' 1. Differenz kleiner
            If As3935_tun_cap >= 1 Then
               Decr As3935_tun_cap                          ' vorhergehenden Wert übernehmen
            End If
         End If
         Exit Do
      End If
      Incr As3935_tun_cap                                   ' nächster Versuch
   Loop Until As3935_tun_cap >= 16                          ' maximal 15 Werte (0-15)
   If As3935_tun_cap >= 16 Then
      Decr As3935_tun_cap
   End If
   If W1 >= 1094 Or W2 >= 1094 Then                         ' max. 3,5 % Abweichung
      Set Led_rt                                            ' rote LED einschalten
      If Ser_ausgabe = 1 Then                               ' serielle Ausgabe eingeschaltet
         Print "LCO Calibrating ERROR"
      End If
      Fourthline : Lcd "Calibrating  NOK"                   ' 4. Zeile
      Ereignis = 15                                         ' Ereignis "LCO Calibrating NOK"
      Esp_zusatz_1 = 255                                    ' Wert zurück setzen
   Else
      Reset Led_rt                                          ' rote LED ausschalten
      If Ser_ausgabe = 1 Then                               ' serielle Ausgabe eingeschaltet
         Print "LCO Calibrating OK"
      End If
      Fourthline : Lcd "Calibrating OK"                     ' 4. Zeile
   End If
   Gosub As3935_disp_lco                                    ' Antenna's Resonance Frequency (LCO) Is Displayed On The Irq Pin
   Timer1 = 0 : Timer0 = Tmr0_preload                       ' Timer zurück setzen
   X = 0 : Do : Loop Until X >= 50                          ' 1000 mSec warten (Timer 0 erhöht X alle 20 mS)
   W = Timer1                                               ' Zaehlerstand Timer 1 übernehmen
   Stop Timer0 : Stop Timer1                                ' Timer stoppen
   D = W * 16                                               ' Frequenz übernehmen
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "LCO TUN_CAP: " ; As3935_tun_cap
   End If
   Locate 2 , 16 : Lcd As3935_tun_cap ; " "                 ' 2. Zeile
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "LCO Frequenz: " ; D ; " Hz"
   End If
   Str10 = Str(d) : Locate 3 , 6 : Lcd Format(str10 , "000.000")       ' 3. Zeile
   Gosub As3935_disp_rco_off                                ' Displayed Frequenzy On The Irq Pin Off
   Wait 1
Return

As3935_cal_rco:                                             ' RCO Calibration Start
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "RCO Calibrating Start"
   End If
   As3935_reg = &H3D                                        ' CALIB_RCO
   As3935_data = &H96                                       ' Send Direct Command Byte
   Gosub As3935_wr_byte
Return

As3935_disp_lco:                                            ' Antenna's Resonance Frequency (LCO) Is Displayed On The Irq Pin
   Disable Pcint2                                           ' AS3935 Interrupt ausschalten
   As3935_reg = &H08                                        ' Register 0x08
   As3935_data = &B10000000 Or As3935_tun_cap               ' Display LCO on IRQ pin
   Gosub As3935_wr_byte
   Waitms 2                                                 ' Time needed by The LCO To Start-up
Return

As3935_disp_srco:                                           ' System RCO (SRCO) 1.125 MHz Is Displayed On The Irq Pin
   Disable Pcint2                                           ' AS3935 Interrupt ausschalten
   As3935_reg = &H08                                        ' Register 0x08
   As3935_data = &B01000000 Or As3935_tun_cap               ' Display SRCO on IRQ pin
   Gosub As3935_wr_byte
   Waitms 2
Return

As3935_disp_trco:                                           ' Timer RCO (TRCO) 32.768 Khz Is Displayed On The Irq Pin
   Disable Pcint2                                           ' AS3935 Interrupt ausschalten
   As3935_reg = &H08                                        ' Register 0x08
   As3935_data = &B00100000 Or As3935_tun_cap               ' Display TRCO on IRQ pin
   Gosub As3935_wr_byte
   Waitms 2
Return

As3935_disp_rco_off:                                        ' Displayed Frequenzy On The Irq Pin Off
   As3935_reg = &H08                                        ' Register 0x08
   As3935_data = &B00000000 Or As3935_tun_cap               ' Display TRCO on IRQ pin off
   Gosub As3935_wr_byte
   Pcifr.pcif2 = 1                                          ' External Interrupt Flag Register
   Enable Pcint2                                            ' AS3935 Interrupt einschalten
Return

As3935_wr_byte:
   I2cstart                                                 ' START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_w                                    ' Device Address for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_reg                                      ' Register
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_data                                     ' Daten
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
Return

As3935_rd_reg:                                              ' ein Register auslesen
   I2cstart                                                 ' START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_w                                    ' Device Address for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_reg                                      ' Word address
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' Repeated START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_r                                    ' Device Address for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_data , Nack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "REG 0x" ; Hex(as3935_reg) ; " hex: " ; Hex(as3935_data) ; " bin: " ; Bin(as3935_data)
   End If
Return

As3935_rd_regall:                                           ' alle Register auslesen
   I2cstart                                                 ' START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_w                                    ' Device Address for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &H00                                            ' Word address
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' Repeated START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_r                                    ' Device Address for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x00 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x01 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x02 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x03 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x04 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x05 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x06 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x07 , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x08 , Nack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   I2cstart                                                 ' START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_w                                    ' Device Address for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte &H3A                                            ' Word address
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' Repeated START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_r                                    ' Device Address for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x3a , Ack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crbyte As3935_r0x3b , Nack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cstop
   If Ser_ausgabe = 1 Then                                  ' serielle Ausgabe eingeschaltet
      Print "REG 0x00 hex: " ; Hex(as3935_r0x00) ; " bin: " ; Bin(as3935_r0x00)
      Print "REG 0x01 hex: " ; Hex(as3935_r0x01) ; " bin: " ; Bin(as3935_r0x01)
      Print "REG 0x02 hex: " ; Hex(as3935_r0x02) ; " bin: " ; Bin(as3935_r0x02)
      Print "REG 0x03 hex: " ; Hex(as3935_r0x03) ; " bin: " ; Bin(as3935_r0x03)
      Print "REG 0x04 hex: " ; Hex(as3935_r0x04) ; " bin: " ; Bin(as3935_r0x04)
      Print "REG 0x05 hex: " ; Hex(as3935_r0x05) ; " bin: " ; Bin(as3935_r0x05)
      Print "REG 0x06 hex: " ; Hex(as3935_r0x06) ; " bin: " ; Bin(as3935_r0x06)
      Print "REG 0x07 hex: " ; Hex(as3935_r0x07) ; " bin: " ; Bin(as3935_r0x07)
      Print "REG 0x08 hex: " ; Hex(as3935_r0x08) ; " bin: " ; Bin(as3935_r0x08)
      Print "REG 0x3A hex: " ; Hex(as3935_r0x3a) ; " bin: " ; Bin(as3935_r0x3a)
      Print "REG 0x3B hex: " ; Hex(as3935_r0x3b) ; " bin: " ; Bin(as3935_r0x3b)
   End If
Return

As3935_rd_reg_look:                                         ' Lightning Detection Look-up table auslesen
   X = &H09
   I2cstart                                                 ' START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_w                                    ' Device Address for write
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte X                                               ' Word address
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2crepstart                                              ' Repeated START
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   I2cwbyte As3935_adr_r                                    ' Device Address for read
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   Do
      I2crbyte Y , Ack
      If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
      Print "REG 0x" ; Hex(x) ; " hex: " ; Hex(y) ; " bin: " ; Bin(y)
      Incr X
   Loop Until X > &H31
   I2crbyte Y , Nack
   If Err = 1 Then : Ereignis = 21 : Esp_zusatz_1 = 255 : End If       ' "I2C-Bus Fehler"
   Print "REG 0x" ; Hex(x) ; " hex: " ; Hex(y) ; " bin: " ; Bin(y)
   I2cstop
Return

'################################   D A T E N   ################################
Wochentag:
   Data "Montag" , "Dienstag" , "Mittwoch" , "Donnerstag" , "Freitag" , "Sonnabend" , "Sonntag"

Number_of_lightning:
   Data "1" , "5" , "9" , "16"

Table_ser_baud:                                             ' Baudrate seriell
   Data "  2400" , "  4800" , "  9600" , " 19200"
   Data " 38400" , " 57600" , "115200" , "230400"
   Data "460800"

Ereignisse:                                                 ' Ereignistexte
  'Data "12345678901234567890" , "12345678901234567890"
   Data "" , "Neustart"                                     '  0,  1
   Data "Set AFE Gain Boost" , "S. Noise Floor Level"       '  2,  3
   Data "Set Min Nr Lightning" , "Set Watchdog Thresh."     '  4,  5
   Data "Set Spike Rejection" , "Set Mask Disturber"        '  6,  7
   Data "Load Preset User" , "Load Setup Default"           '  8,  9
   Data "Save Preset User" , "DCF77 Empfang OK"             ' 10, 11
   Data "Sommerzeit" , "Winterzeit"                         ' 12, 13
   Data "Noise Floor too high" , "LCO Calibrating NOK"      ' 14, 15
   Data "TRCO Calibrating NOK" , "SRCO Calibrating NOK"     ' 16, 17
   Data "TRCO Calibr. Missing" , "SRCO Calibr. Missing"     ' 18, 19
   Data "Datum Uhrzeit von PC" , "I2C-Bus Fehler"           ' 20, 21

'GS15_ATmega328P_000_004_069_CL-STAT
'Clear Statistics nach jedem Gewitter eingefügt
'manchmal keinerlei Reaktion mehr, weder auf Störungen noch Blitze

'GS15_ATmega328P_000_004_070_CL-STAT
'nach TX433 Abfrage Interrupt-Eingang AS3935 eingefügt
'manchmal keinerlei Reaktion mehr, weder auf Störungen noch Blitze

'GS15_ATmega328P_000_004_071_Ports
'Eingänge Taster Pullup ein geändert: PINC.0 in PORTC.0 usw.

'GS15_ATmega328P_000_004_075_Stack
'Werte Stacks und Frame erhöht: 80 -> 128

'GS15_ATmega328P_000_004_077_DS1621
'DS1621_mess entfernt, da IC default kontinuierlich mißt
'Menupunkt "Neustart" hinzugefügt
'Menupunkt 30 "Störungen" löschen entfernt

'GS15_ATmega328P_000_004_088_PCINT
'Disable/Enable PCINT2 entfernt
'nach TX433 Abfrage Interrupt-Eingang AS3935 auskommentiert

'GS15_ATmega328P_000_004_089_PCINT
'As3935_clear_statistics nach Speichern Gewitter komplett entfernt
'Fehler, das nach Gewitter keine Reaktion mehr erfolgt, ist damit beseitigt

'GS15_ATmega328P_000_004_092_CL-STAT
'As3935_clear_statistics verändert (low-high)
'funktioniert nicht

'GS15_ATmega328P_000_004_093_CL-STAT
'As3935_clear_statistics verändert (low-waitms5-high)

'GS15_ATmega328P_000_004_103_CL-STAT
'As3935_clear_statistics komplett entfernt (Start und nach Gewitter)
'funktioniert

'GS15_ATmega328P_000_005_001_serial
'serielle Ausgabe ein/aus hinzugefügt

'GS15_ATmega328P_000_005_008_I2C-Error
'I2C-Fehlermeldung hinzugefügt

'GS15_ATmega328P_000_006_021_I2C
'lt. AS3935_datasheet_v1-04.pdf Seite 24 I²C Clock Speed 100kHz und I2CD Pull Up 10k