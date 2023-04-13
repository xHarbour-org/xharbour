#===============================================================================
# $Id$
#===============================================================================

MORE_INCLUDE =-I"$(RDDSQL_DIR)" -I"$(HBSQLIT3_DIR)"

!if !("$(HB_DIR_ADS)"=="")
MORE_INCLUDE =-I"$(HB_DIR_ADS)" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(RDDADS_LIB)
!endif

!if !("$(HB_DIR_OPENSSL)"=="")
MORE_INCLUDE =-I"$(HB_DIR_OPENSSL)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(TIPSSL_LIB) $(HBSSL_LIB)
!endif

!if !("$(HB_DIR_CURL)"=="")
MORE_INCLUDE =-I"$(HB_DIR_CURL)\include" -I"$(HB_DIR_CURL)\include\curl" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(HBCURL_LIB)
!endif

!if !("$(HB_DIR_CAIRO)"=="")
MORE_INCLUDE =-I"$(HB_DIR_CAIRO)\include\cairo" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(HBCAIRO_LIB)
!endif

!if !("$(HB_DIR_POSTGRESQL)"=="")
MORE_INCLUDE =-I"$(HB_DIR_POSTGRESQL)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(PGSQL_LIB) $(SDDPG_LIB)
!endif

!if !("$(HB_DIR_OCILIB)"=="")
MORE_INCLUDE =-I"$(HB_DIR_OCILIB)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(SDDOCI_LIB)
!endif

!if !("$(HB_DIR_MYSQL)"=="")
MORE_INCLUDE =-I"$(HB_DIR_MYSQL)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(MYSQL_LIB) $(SDDMY_LIB)
!endif

!if !("$(HB_DIR_FIREBIRD)"=="")
MORE_INCLUDE =-I"$(HB_DIR_FIREBIRD)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(FIREBIRD_LIB) $(SDDFB_LIB)
!endif

!if !("$(HB_DIR_MAGIC)"=="")
MORE_INCLUDE =-I"$(HB_DIR_MAGIC)\include" $(MORE_INCLUDE)
OPTIONAL_PRJ =$(OPTIONAL_PRJ) $(HBMAGIC_LIB)
!endif

INCLUDE_DIR  =$(MORE_INCLUDE) $(INCLUDE_DIR)

