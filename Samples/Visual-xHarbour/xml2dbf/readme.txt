About the Project
============

xml2dbf is designed to read XML files filled with table data, and to import selectively the columns into an existing DBF file.

Version 1.0 of the project is ready to read and convert UTF-8 encoding and ANSI codepages with some limitations:
- the utf8 data is converted into ansi/oem codepages, thus when an utf8 string contains a mixture of characters coming from different language families ( for example latin1 and latin2 ), some of the national characters will be altered
- in case the utf8 data contains text in other than English language, it's necessary to set the project's language accordingly (  in the Object Manager select Application -> Properties -> SetLanguage )
- in case the XML file's real encoding is an ansi codepage, which is not the same as the computer's ansi codepage ( the computer where xml2dbf.exe is running ), some national characters will be altered

The application detects the wrong utf8 byte sequences and when it encounters more such sequences, it proceeds the xml data as having ansi encoding.

In case the xml file contains DATE values in other than year-month-day format ( export format used by SQL engines ), the ::DateStyle variable needs to be set accordingly.


User Interface
=========

Suggested test:

1. Use the appropiate "..." button for selecting an XML file ( which contains table data with at least two "row-alike" tags, like partners, employees, products etc. )

2. Click button "Load XML data" ( a progressbar is shown during the process )

3. Click button "Show XML data" - a Grid is shown with some sample data extracted from the beginning of the XML file ( at present 20 rows and 16 columns, each column limited to 20 characters )

4. Use the appropiate "..." button for selecting a DBF file ( which contains the field definitions for the data you want to extract from the XML file )

5. Click button "DBF columns", now you can use the matching grid

6. Select an XML item for each DBF field you want to populate with data extracted from the XML file.
For example if you have an EMPLOYEE field:
- click on the long white cell in row containing the EMPLOYEE field
- now is shown a "Select XML Item" dialog with a list of XML items
- click on the XML item, which matches your "employee" field
- the program adds your choice to the matching grid
- in order to deselect a match: click on it and chose the first BLANK line from the "Select XML Item" dialog

You can use the "Show XML data" button for checking the sample data and XML Item names - listed as column headers.

7. In case you consider that the file selection was wrong, you can use the "Go Back to Files" button:
- the matching grid is cleaned up - in order to keep correct the data collected via the user interface
- if case you do an XML file selection, you need to load it as at [2]

8. When you consider that the files are ok and you have selected all the needed matches between XML items and DBF fields, click "Start Data Import" ( a progressbar is shown while the data is converted and written into the DBF file ).

After the end of the importing process you are taken into the "Files" group of the user interface, and there is possible to select other DBF or XML file or both.



















