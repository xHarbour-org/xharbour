PROCEDURE Main()
   
   LOCAL sString := "123456789ABCDEF"
    
   ? At( "7", sString    ) // 7
   ? At( "7", sString, 0 ) // 7 
   ? At( "7", sString, 1 ) // 7  
   ? At( "7", sString, 7 ) // 7 
   ? At( "7", sString, 8 ) // 0 
    
   ? At( "7", sString, 7, 7 ) // 7  
   ? At( "7", sString, 7, 6 ) // 0   

   ? At( "7", sString, 7, -9 ) // 7   
   ? At( "7", sString, 7, -10 ) // 0   

   ? At( "7", sString, -9 ) // 7 
   ? At( "7", sString, -8 ) // 0 
    
RETURN    
     