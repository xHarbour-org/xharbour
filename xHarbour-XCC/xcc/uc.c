
extern void mutta(unsigned char *, int *);


int main(void)
{
    unsigned char *str = "Pelle testar";
    long n = -1;
    char c = 2;

    mutta("kalle kanon", &n);
//  mutta("kalle kanon", &c);

    return 0;
}
