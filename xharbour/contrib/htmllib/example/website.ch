#include "default.ch"
#include "html.ch"
#include "forms.ch"

// addresses
#define WEB_ROOT          soIni:get("general","webSiteRoot", "c:\web\")
#define WEB_EMAIL         soIni:get("info","email")

// images
#define COMMON_BGIMAGE    soIni:get("common", "bgImage")
#define COMMON_BGCOLOR    soIni:get("common","bgColor")
#define COMMON_EMAIL      soIni:get("common", "email")
#define COMMON_SIDEBAR    soIni:Get("common","sideBar")
#define COMMON_LOGO       soIni:Get("common","logo")
#define COMMON_COUNTER    "/images/counters/counter0/"
