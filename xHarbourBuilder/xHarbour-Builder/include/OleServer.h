const char *g_RegTable[][3] =
{
   //format is {key, value name, value }
   { "CLSID\\" CLS_ID,                    0,                CLS_Name         },
   { "CLSID\\" CLS_ID "\\InprocServer32", 0,                (const char*) -1 },
   { "CLSID\\" CLS_ID "\\InprocServer32", "ThreadingModel", "Apartment"      },
   { "CLSID\\" CLS_ID "\\ProgId",         0,                CLS_Name         },
   { CLS_Name,                            0,                CLS_Name         },
   { CLS_Name "\\CLSID",                  0,                CLS_ID           }
};

BOOL IsExportedSymbol( PHB_DYNS pDyn )
{
   if( pDyn->pSymbol >= symbols && pDyn->pSymbol <= symbols + ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ) )
   {
      return TRUE;
   }

   return FALSE;
}

PHB_SYMB OleServerSymbols( USHORT *puiSymbols )
{
   *puiSymbols = (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) );
   return symbols_table;
}
