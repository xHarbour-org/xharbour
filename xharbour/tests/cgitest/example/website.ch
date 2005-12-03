
#include "html.ch"
#include "htmlform.ch"

// addresses
#define WEB_ROOT          IniGet("general","webSiteRoot", "c:\apache2\htdocs")
#define WEB_EMAIL         IniGet("info","email")

// images
#define COMMON_BGIMAGE    IniGet("common", "bgImage")
#define COMMON_BGCOLOR    IniGet("common","bgColor")
#define COMMON_EMAIL      IniGet("common", "email")
#define COMMON_SIDEBAR    IniGet("common","sideBar")
#define COMMON_LOGO       IniGet("common","logo")
#define COMMON_COUNTER    "/images/counters/counter0/"
