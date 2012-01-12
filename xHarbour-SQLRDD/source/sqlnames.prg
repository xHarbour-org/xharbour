/* $CATEGORY$SQLRDD/Utils$FILES$sql.lib$
* SQLRDD Namespaces
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "compat.ch"
#include "sqlrdd.ch"
#include "msg.ch"

#ifdef __XHARBOUR__
#ifndef SQLRDD_COMPAT_2007

NAMESPACE SQLRDD

Function File( cFile )
Return sr_file( cFile )

Function dbStruct()
Return SR_dbStruct()

END

#endif
#endif
