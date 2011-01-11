
What Is This?
-------------

During the last years I met many people formulating such needs:
- a robot program doing so and so - which starts once in 20 ( 30, 40,...) minutes during business hours
- a program downloading periodically Forex, Sports Bets... data
- a program starting some server-side script, and optionally receiving the results

Microsoft supports executables registered as services, but users often need a custom Task Scheduler, 
which is coded and updated according to their current business logic.

CustomSession is a class designed to support such task schedulers.



Folders
-------


[1]. Class - contains a .prg file, it should be compiled as a library, and linked in the Client(s) and in the Server

[2]. Client - contains a .prg file, it should be compiled as an executable ( [1] should be added to the project )
Note: this is a sample client, the real-life client might be an xHarbour or VxH module

[3] Server - contains a VxH project, it should be built as an executable ([1] should be added to the project )
Note: this is a sample server, the real-life server will implement a concrete business logic



How To Build
-------------

1. Copy the CustomSession sample project in the desired target folder, for example  C:\Test

2. Use xBuildW: in subfolder Class build cs.prg as a LIB

3. Use xBuildW: in subfolder Client build an executable by using console.prg and cs.LIB

4. Use VxH:
- open Server.vxh
- in the source code update the path to console.exe as needed
- add cs.LIB to the project ( File menu => Add to project => Binary Files )
- release


