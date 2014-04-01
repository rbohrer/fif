#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "runtime.h"

double ABSF(double x) {
    return (x > 0.0) ? x : -x;
}

int XABSF(int x) {
    return (x > 0) ? x : -x;
}

double INTF(double x) {
    return (double)(int) x;
}

int XINTF(double x) {
    return (int) x;
}

double MODF(double x, double y){
    return fmod(x, y);
}

int XMODF(int x,int y){
    return x % y;
}

double MAX0F(int x,int y) {
    return (double)(x > y? x : y);
}

double MAX1F(double x,double y) {
    return (x > y? x : y);
}
int XMAX0F(int x,int y) {
    return (x > y? x : y);
}

int XMAX1F(double x ,double y){
    return (int)(x > y? x : y);
}

double MIN0F(int x,int y) {
    return (double)(x < y? x : y);
}

double MIN1F(double x,double y) {
    return (x < y? x : y);
}
int XMIN0F(int x,int y) {
    return (x < y? x : y);
}

int XMIN1F(double x ,double y){
    return (int)(x < y? x : y);
}

void next(format* f) {
    f->i++;
    if(f->front[f->i] == NULL) {
        f->i = 0;
        if(f->back != NULL) {
            f->front = f->back;
            f->back = NULL;
        }
    }
}

void out(format* f, double arg) {
    while(1){
        if(strstr(f->front[f->i], "%") == NULL)
            printf(f->front[f->i]);
        else if(strstr(f->front[f->i], "f") != NULL)
            printf(f->front[f->i], arg);
        else if(strstr(f->front[f->i], "d") != NULL)
            printf(f->front[f->i], (int)arg);

        if(strstr(f->front[f->i], "%")!= NULL) {
            next(f);
            break;
        } else {
            next(f);
        }
    }
}

void nullary_out(format* f) {
    printf("%s", f->front[0]);
    next(f);
}

void in(format* f, void** arg) {
    int x;
    int i = 0,j=0;
    char* buf;
    while(1){
        if(strstr(f->front[f->i], "f") != NULL){
            buf = strdup(f->front[f->i]);

            for(; buf[i] != '\0'; i++) {
                while(buf[i] == '.') {
                    i++;
                    while(buf[i] >= '0' && buf[i] <= '9') {
                        i++;
                    }
                }
                buf[j++] = buf[i];
            }
            buf[j] = '\0';
            x = scanf(buf, (float*)arg);
            free(buf);
        }
        else if(strstr(f->front[f->i], "d") != NULL) {
            scanf(f->front[f->i], (int*)arg);
        }
        if(strstr(f->front[f->i], "%") != NULL) {
            next(f);
            break;
        } else {
            next(f);
        }
    }
}
