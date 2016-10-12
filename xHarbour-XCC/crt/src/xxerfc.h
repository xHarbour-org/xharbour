/****************************************************************************
 *                                                                          *
 * File    : xxerfc.h                                                       *
 *                                                                          *
 * Purpose : Common erfc[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

FTYPE (FNAME(erfc))(FTYPE x);

extern FTYPE FNAME(erf_one);
extern FTYPE FNAME(erf_small);

/* coefficients */
#if FBITS <= 26
#define DPOLY0(x)   ((((c0[0] * x + c0[1]) * x + c0[2]) * x + c0[3]) * x + c0[4])
#define NPOLY0(x)   ((((s0[0] * x + s0[1]) * x + s0[2]) * x + s0[3]) * x + s0[4])

static const FTYPE s0[] = {  /* 4/4, -1.523/2 <= x < 1.523/2 */
    FLIT( 0.2045518622e-1),
    FLIT(-0.1404513379),
    FLIT( 0.3836504205),
    FLIT(-0.4918288351),
    FLIT( 0.2486650716),
};

static const FTYPE c0[] = {
    FLIT( 0.4786968771e-1),
    FLIT( 0.6074318885e-1),
    FLIT( 0.3816347264),
    FLIT( 0.2354897310),
    FLIT( 0.8833120886),
};
#elif FBITS <= 53
#define DPOLY0(x)   FNAME(poly)(x, c0, sizeof(c0) / sizeof(c0[0]) - 1)
#define NPOLY0(x)   FNAME(poly)(x, s0, sizeof(s0) / sizeof(s0[0]) - 1)

static const FTYPE s0[] = {  /* 9/8, -1.523/2 <= x < 1.523/2 */
    FLIT(-0.28454577312109560143e-5),
    FLIT( 0.18394563813824520319e-3),
    FLIT(-0.14626203913403887246e-2),
    FLIT( 0.43593787078872065739e-2),
    FLIT(-0.53570683967451820420e-2),
    FLIT( 0.15196755993269900948e-1),
    FLIT(-0.10490853682502501625),
    FLIT( 0.32025483312757518503),
    FLIT(-0.44611565875600305283),
    FLIT( 0.24204907508916822407),
};

static const FTYPE c0[] = {
    FLIT( 0.31308180481511147383e-3),
    FLIT( 0.10464185023961616679e-2),
    FLIT( 0.85156847127648798891e-2),
    FLIT( 0.19285250070876908577e-1),
    FLIT( 0.89695774059476213762e-1),
    FLIT( 0.13381620644981387056),
    FLIT( 0.44267679797866257759),
    FLIT( 0.34512438277725710134),
    FLIT( 0.85981067673572546097),
};
#elif FBITS <= 66
#define DPOLY0(x)   FNAME(poly)(x, c0, sizeof(c0) / sizeof(c0[0]) - 1)
#define NPOLY0(x)   FNAME(poly)(x, s0, sizeof(s0) / sizeof(s0[0]) - 1)

static const FTYPE s0[] = {  /* 11/10, -1.523/2 <= x < 1.523/2 */
    FLIT(-0.1847961517823880210114656e-5),
    FLIT( 0.1532574995934123720855224e-4),
    FLIT(-0.5664453975356429576789467e-4),
    FLIT( 0.2638207954009617543499223e-3),
    FLIT(-0.1518824884590013010731582e-2),
    FLIT( 0.5053111064107753178918659e-2),
    FLIT(-0.8801622105963802425532622e-2),
    FLIT( 0.1971603102033820645235903e-1),
    FLIT(-0.9707096501743785287530431e-1),
    FLIT( 0.2918641111088126366707373),
    FLIT(-0.4201727722572197764341640),
    FLIT( 0.2371075675410971894778244),
};

static const FTYPE c0[] = {
    FLIT( 0.2380083841076712333917077e-4),
    FLIT( 0.1022877078157265704695646e-3),
    FLIT( 0.9180901956047104062443466e-3),
    FLIT( 0.2796105189381228234455421e-2),
    FLIT( 0.1436092379531775412459311e-1),
    FLIT( 0.3177732120308627794268644e-1),
    FLIT( 0.1165604442472019529923002),
    FLIT( 0.1761422787850945716023216),
    FLIT( 0.4902396601910771906643807),
    FLIT( 0.3978811733690149640399374),
    FLIT( 0.8422573729379873298462059),
};
#elif FBITS <= 116
#define DPOLY0(x)   FNAME(poly)(x, c0, sizeof(c0) / sizeof(c0[0]) - 1)
#define NPOLY0(x)   FNAME(poly)(x, s0, sizeof(s0) / sizeof(s0[0]) - 1)

static const FTYPE s0[] = {  /* 17/17, -1.523/2 <= x < 1.523/2 */
    FLIT(-0.11329309158464025109871375253902740e-9),
    FLIT( 0.17438248618232068279646846102699136e-8),
    FLIT(-0.93169276926468052741118039736224129e-8),
    FLIT( 0.23809322889414106830044798115930191e-7),
    FLIT(-0.20538909228985518927470238797817589e-6),
    FLIT( 0.22129714370263534738557688208694612e-5),
    FLIT(-0.10568032279190851299354483834875291e-4),
    FLIT( 0.23172833000950494769628658249222208e-4),
    FLIT(-0.62028807028938473672302946420298695e-4),
    FLIT( 0.53610023396619879874263300406627537e-3),
    FLIT(-0.26734280128870337560810569822143559e-2),
    FLIT( 0.63587056029737271681713716817031211e-2),
    FLIT(-0.85440071597827130025564671039770501e-2),
    FLIT( 0.27366888768283584057310622131825422e-1),
    FLIT(-0.13692191558227221910673444767586865),
    FLIT( 0.35972783019392542452871752912637438),
    FLIT(-0.46465348725739864967972154546352928),
    FLIT( 0.24216075149676871315291429051812913),
};

static const FTYPE c0[] = {
    FLIT(-0.11559936518594082894463298681595437e-9),
    FLIT( 0.15374619777967933895795735358169380e-9),
    FLIT(-0.50587845287409095676896081004339701e-8),
    FLIT( 0.33368579945056299249370201991083921e-7),
    FLIT(-0.14254701316075305393766102397564309e-7),
    FLIT( 0.19471260739308939109558272262591707e-5),
    FLIT( 0.40950635721772373809205317590987225e-5),
    FLIT( 0.58784241952115981553711865342861141e-4),
    FLIT( 0.13618094681063754881342545239053576e-3),
    FLIT( 0.10878874013863626533585085446263620e-2),
    FLIT( 0.22442307128855287750380772591832711e-2),
    FLIT( 0.12984744065458638905112666432496609e-1),
    FLIT( 0.21595427654529871120943279836616770e-1),
    FLIT( 0.98592071562541193763951583046578824e-1),
    FLIT( 0.11711669005170627061240481750846790),
    FLIT( 0.43641429891543433290922687855686834),
    FLIT( 0.28016438619819940610319322615137846),
    FLIT( 0.86020737549426841480510219210239517),
};
#else
#error erfc small has insufficient precision
#endif

#if FBITS <= 24
#define DPOLY1(x)   (((c1[0] * x + c1[1]) * x + c1[2]) * x + c1[3])
#define NPOLY1(x)   (((s1[0] * x + s1[1]) * x + s1[2]) * x + s1[3])

static const FTYPE s1[] = {  /* 3/3, 3.82 <= |x| */
    FLIT(4.0),
    FLIT(40.0),
    FLIT(87.0),
    FLIT(24.0),
};

static const FTYPE c1[] = {
    FLIT(8.0),
    FLIT(84.0),
    FLIT(210.0),
    FLIT(105.0),
};
#elif FBITS <= 53
#define DPOLY1(x)   FNAME(poly)(x, c1, sizeof(c1) / sizeof(c1[0]) - 1)
#define NPOLY1(x)   FNAME(poly)(x, s1, sizeof(s1) / sizeof(s1[0]) - 1)

static const FTYPE s1[] = {  /* 7/7, 5.84 <= |x| */
    FLIT(64.0),
    FLIT(3328.0),
    FLIT(63888.0),
    FLIT(570240.0),
    FLIT(2445660.0),
    FLIT(4672080.0),
    FLIT(3133935.0),
    FLIT(322560.0),
};

static const FTYPE c1[] = {
    FLIT(128.0),
    FLIT(6720.0),
    FLIT(131040.0),
    FLIT(1201200.0),
    FLIT(5405400.0),
    FLIT(11351340.0),
    FLIT(9459450.0),
    FLIT(2027025.0),
};
#elif FBITS <= 64
#define DPOLY1(x)   FNAME(poly)(x, c1, sizeof(c1) / sizeof(c1[0]) - 1)
#define NPOLY1(x)   FNAME(poly)(x, s1, sizeof(s1) / sizeof(s1[0]) - 1)

static const FTYPE s1[] = {  /* 8/8, 6.47 <= |x| */
    FLIT(128.0),
    FLIT(8640.0),
    FLIT(224224.0),
    FLIT(2862288.0),
    FLIT(19091160.0),
    FLIT(65155860.0),
    FLIT(102901050.0),
    FLIT(58437855.0),
    FLIT(5160960.0),
};

static const FTYPE c1[] = {
    FLIT(256.0),
    FLIT(17408.0),
    FLIT(456960.0),
    FLIT(5940480.0),
    FLIT(40840800.0),
    FLIT(147026880.0),
    FLIT(257297040.0),
    FLIT(183783600.0),
    FLIT(34459425.0),
};
#elif FBITS <= 113
#define DPOLY1(x)   FNAME(poly)(x, c1, sizeof(c1) / sizeof(c1[0]) - 1)
#define NPOLY1(x)   FNAME(poly)(x, s1, sizeof(s1) / sizeof(s1[0]) - 1)

static const FTYPE s1[] = {  /* 14/14, 8.69 <= |x| */
    FLIT(8192.0),
    FLIT(1658880.0),
    FLIT(145100800.0),
    FLIT(7224576000.0),
    FLIT(227203941888.0),
    FLIT(4735496451840.0),
    FLIT(66788113835520.0),
    FLIT(639497897337600.0),
    FLIT(4110613566040800.0),
    FLIT(17273896319599920.0),
    FLIT(45303944436550800.0),
    FLIT(68618077387259400.0),
    FLIT(52363496305370250.0),
    FLIT(15234653491682625.0),
    FLIT(714164561510400.0),
};

static const FTYPE c1[] = {
    FLIT(16384.0),
    FLIT(3325952.0),
    FLIT(291852288.0),
    FLIT(14592614400.0),
    FLIT(461491430400.0),
    FLIT(9691320038400.0),
    FLIT(138101310547200.0),
    FLIT(1341555588172800.0),
    FLIT(8803958547384000.0),
    FLIT(38150487038664000.0),
    FLIT(104913839356326000.0),
    FLIT(171677191673988000.0),
    FLIT(150217542714739500.0),
    FLIT(57775977967207500.0),
    FLIT(6190283353629375.0),
};
#else
#error erfc tail has insufficient precision
#endif

static const FTYPE erfc_uflow = FLIT(110.0);
static const FTYPE twobyrootpi = FLIT(1.12837916709551257389615890312154518);

/* compute erfc(x) */
FTYPE __cdecl (FFUN(erfc))(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case 0:
            return FLIT(1.0);

        case FP_INFINITE:
            return FISNEG(x) ? FLIT(2.0) : FLIT(0.0);

        default:  /* -INF or finite */
            if (x < -FNAME(erf_one))
                return FLIT(2.0);
            else if (x < -FNAME(erf_small))
                return FLIT(2.0) - FNAME(erfc)(-x);
            else if (x < FLIT(0.0))
                return FLIT(2.0) - NPOLY0(x) / DPOLY0(x);
            else if (x < FNAME(erf_small))
            {
                /* compute approximation for [0, Erf_small) */
                x -= FNAME(erf_small) * 0.5;
                return NPOLY0(x) / DPOLY0(x);
            }
            else if (x < FNAME(erf_one))
                return FNAME(erfc)(x);
            else if (x < erfc_uflow)
            {
                /* compute approximation for possibly finite tail */
                FTYPE y = x * x;
                FTYPE z = -y;

                FNAME(exp)(&z, twobyrootpi * NPOLY1(y) / (x * DPOLY1(y)), 0);
                return (z);
            }
            else
                return FLIT(0.0);
    }
}

