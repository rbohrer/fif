double ABSF(double);
int XABSF(int);
double INTF(double);
int XINTF(double);
double MODF(double,double);
int XMODF(int,int);
double MAX0F(int,int);
double MAX1F(double,double);
int XMAX0F(int,int);
int XMAX1F(double,double);
double MIN0F(int,int);
double MIN1F(double,double);
int XMIN0F(int,int);
int XIMIN1F(double,double);


typedef struct {
    char** front;
    char** back;
    int i;
} format;

void nullary_out(format* f);
void out(format* f, double arg);
void in(format* f, void** arg);
