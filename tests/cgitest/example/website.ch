#include "html.ch"
#include "htmlform.ch"
#include "htmlclrs.ch"
// addresses
#define WEB_ROOT          "c:\apache2.2\htdocs"
#define WEB_EMAIL         IniGet("info","email")

// images
#define COMMON_BGIMAGE    IniGet("common", "bgImage")
#define COMMON_BGCOLOR    IniGet("common","bgColor")
#define COMMON_EMAIL      IniGet("common", "Email")
#define COMMON_SIDEBAR    IniGet("common","SideBar")
#define COMMON_LOGO       IniGet("common","logo")
#define COMMON_COUNTER    "/images/counters/counter0/"
