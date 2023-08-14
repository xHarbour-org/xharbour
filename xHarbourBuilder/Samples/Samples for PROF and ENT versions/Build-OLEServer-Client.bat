..\Bin\xBuild SampleOLEServer.dll.xbp -all

regsvr32.exe sampleoleserver.dll

..\Bin\xBuild Test_OLEServer.exe.xbp -all

Test_OLEServer.exe