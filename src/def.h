#pragma once
#ifndef DEF_H
#define DEF_H
#define TCL_MEM_DEBUG


#include <Windows.h>
#include <tcl.h>
#include <gl/glut.h>
#include <gl/GL.h>
#include <gl/GLU.h>
#include "main.h"

typedef struct point_t
{
	GLdouble x;
	GLdouble y;
	GLdouble z;
} point_t;

typedef struct curve_t
{
	point_t* points[3];
	point_t* lines1;
	point_t* lines2;
	int line1Size;
	int line2Size;
	int pointNum;
	struct curve_t* next;
} curve_t;

typedef struct color_t
{
	GLdouble r;
	GLdouble g;
	GLdouble b;
} color_t;

typedef struct model_t
{
	float* vertex;
	float* color;
	float* normal;
	float* dsVertex; //down sample vertex
	int count;
	int dsCount; //the number of down sample vertex
	float max[3];
	float min[3];
	float dsMax[3];
	float dsMin[3];
	float delta[4];
} model_t;


enum
{
	X,
	Y,
	Z
};

typedef enum {
	NONE,
	SPECIMEN,
	LANDMARK,
	DOWN_SAMPLE,
	DOWN_SAMPLE_ONLY,
	CURVE,
	ANCHOR,
	ALL
} show_mode_t;

typedef struct dot_t
{
	point_t p;
	color_t c;
	show_mode_t type;
	struct dot_t* next;
} dot_t;

#define FREE(p) if (p != NULL) {free(p); p = NULL;}
#define D(info) printf(info"\n")
#define D1(info, arg1) printf(info"\n", arg1)
#define D2(info, arg1, arg2) printf("%s: "info"\n", __FUNCTION__, arg1, arg2)
#define D3(info, arg1, arg2, arg3) printf("%s: "info"\n", __FUNCTION__, arg1, arg2, arg3)

extern HDC dc;

float getRealZ(float x, float y, float z);

void ogl_init();
void ogl_enableLight();
void ogl_disableLight();
void ogl_getObjCoordinate(int x, int y, GLdouble* posX, GLdouble* posY, GLdouble* posZ, char* buf);
int ogl_loadDgtModel(const char* filename, model_t* model);
void ogl_loadModel(const char* filename, model_t* model);
int ogl_loadDownSampleModel(FILE* file, model_t* model);
void ogl_drawModel(model_t* model);
void ogl_drawDownSampleModel(model_t* model, GLdouble radius, point_t* downSampleOffset);
int ogl_loadLandMark(const char* filename, model_t* models, int model_id, int amount);
void ogl_drawDot(point_t* p, color_t* c, GLdouble radius);
void ogl_drawLabel(point_t* p, color_t* c, int id, GLdouble dotRadius, float z);
void ogl_drawCube(float* max, float* min);
void ogl_drawLine(point_t* line, int size);
void ogl_grid(float* max, float* min);
float ogl_calCoordinate(float value, int id, float* delta);
void ogl_DrawTriangle();

int dot_add(point_t* p, color_t* c);
int dot_select(point_t* p, float dotRadius);
int dot_selected_id();
int dot_move(point_t* p);
int dot_color(color_t* c);
int dot_del(point_t *p);
int dot_del_selected();
dot_t* dot_get(int id);
void dot_slice_amount(int amount);
int dot_slice_index(int id);
int valiadteDot(point_t* p);
void dots_free();

int anchor_add(point_t* p, color_t* c);
int anchor_select(point_t* p, float dotRadius);
int anchors_selected_id();
int anchor_move(point_t* p);
int anchor_color(color_t* c);
int anchor_del(point_t* p);
int anchor_del_selected();
dot_t* anchor_get(int id);
void anchors_slice_amount(int amount);
int anchor_slice_index(int id);
void anchors_free();


int curve_addDot(int id, dot_t* d);
curve_t* curve_get();
int curve_getDotId();
dot_t* dot_get_dot(int id, int pid);
void curve_slice_amount(int amount);
int curve_slice_index(int id);
#endif