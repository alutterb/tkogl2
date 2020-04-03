#include "def.h"
#include <string.h>
#include <stdio.h>
#include <stddef.h>

#define MAX_STEP_NAME 128
#define TCL_CMD(name) int name(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])

#define TCL_RESULT1(info, arg1) \
	char *msg = Tcl_Alloc(512); \
	sprintf(msg, info, arg1); \
	Tcl_SetResult(interp, msg, TCL_DYNAMIC);
#define TCL_RESULT2(info, arg1, arg2) \
	char *msg = Tcl_Alloc(512); \
	sprintf(msg, info, arg1, arg2); \
	Tcl_SetResult(interp, msg, TCL_DYNAMIC);
#define TCL_RESULT3(info, arg1, arg2, arg3) \
	char *msg = Tcl_Alloc(512); \
	sprintf(msg, info, arg1, arg2, arg3); \
	Tcl_SetResult(interp, msg, TCL_DYNAMIC);

int tclCmdStep(int curId, Tcl_Obj* const objv[], const char** steps, int stepNum, int stepId)
{
	if (curId > 0) return curId;

	for (int i = 1; i <= stepNum; i++) {

		const char* step = Tcl_GetStringFromObj(objv[i], NULL);
		if (strcmp(step, steps[i - 1]) != 0)
		{
			return 0;
		}
	}
	return stepId;
}

HDC dc; /*device context for windows, allows us to communicate to display for drawing*/
model_t* models = NULL;
float deltas[1000][4]; /*change this to linked list for more efficiency*/

int model_index = 0;
int model_amount = 0;
int temp_index = 0;
show_mode_t showModel = LANDMARK;

int width;
int height;

int labeled = 1;
int alabeled = 1;
int anchorPlaced = 0;
int downsampled = 0;

color_t defaultDotColor = { 1.0, 0.0, 0.0 };
color_t defaultAnchorColor = { 0.0, 1.0, 0.0 };
GLdouble dotRadius = 0.01f;
GLdouble anchorRadius = 0.01f;
point_t downSampleOffsetBeg = { 0.0 };
point_t downSampleOffset = { 0.0 };

typedef struct {
	float x;
	float y;
	float z;
	float scale;
	float rotation[3];
} context_t;
context_t* context = NULL;


int setWindowId(HWND hwnd)
{
#ifdef _WIN32
	/* Grab the HWND from Tcl. */

	/* Setup OpenGL. */
	dc = GetDC(hwnd);



	/* Windows code, setup OpenGL. */
	PIXELFORMATDESCRIPTOR pfd;
	memset(&pfd, 0, sizeof(pfd));
	pfd.nVersion = 1;
	pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	pfd.iPixelType = PFD_TYPE_RGBA;
	pfd.cColorBits = 24;
	pfd.cDepthBits = 16;
	pfd.iLayerType = PFD_MAIN_PLANE;

	GLuint pixelFormat = ChoosePixelFormat(dc, &pfd);
	SetPixelFormat(dc, pixelFormat, &pfd);

	HGLRC rc = wglCreateContext(dc);

	wglMakeCurrent(dc, rc);

#elif __linux__

	Tk_Window    __glTkwin__;
	XVisualInfo* __glVisinfo__ = (XVisualInfo*)NULL;
	int dummy;
	int attribs[] =
	{ GLX_USE_GL, GLX_RGBA, GLX_DOUBLEBUFFER, GLX_DEPTH_SIZE, 0 };
	GLXContext __glGLXContext__ = (void*)NULL;

	__glTkwin__ = Tk_NameToWindow(interp, Tcl_GetString(objv[1]), Tk_MainWindow(interp));
	if (__glTkwin__ == NULL) {
		fprintf(stderr, "__glTkwin__ is NULL\n");
		exit(-1);
	}
	Tk_MakeWindowExist(__glTkwin__);

	/* setup X11 implementation of OpenGL */
	__glDisplay__ = Tk_Display(__glTkwin__);
	if (__glDisplay__ == NULL) {
		fprintf(stderr, "__glDisplay__ is NULL\n");
		exit(-1);
	}
	if (!glXQueryExtension(__glDisplay__, &dummy, &dummy)) {
		fprintf(stderr, "require GLX Extesion\n");
		exit(-1);
	}
	__glWindow__ = Tk_WindowId(__glTkwin__);

	__glVisinfo__ =
		glXChooseVisual(__glDisplay__, DefaultScreen(__glDisplay__), attribs);

	if (__glVisinfo__ == NULL) {
		fprintf(stderr, "__glVisinfo__ is NULL\n");
		exit(-1);
	}

	__glGLXContext__ =
		glXCreateContext(__glDisplay__, __glVisinfo__, 0, GL_TRUE);
	if (__glGLXContext__ == NULL) {
		fprintf(stderr, "__glGLXContext__ is NULL\n");
		exit(-1);
	}
	glXMakeCurrent(__glDisplay__, __glWindow__, __glGLXContext__);

#endif

	ogl_init();
	return TCL_OK;
}


void resetContext(int id, float maxXY)
{



	context[id].x = 0.0;
	context[id].y = 0.0;
	context[id].z = 0.0;
	context[id].scale = 1.0;
	context[id].rotation[0] = 0.0;
	context[id].rotation[1] = 0.0;
	context[id].rotation[2] = 0.0;

	if (maxXY > 0.8)
	{
		context[id].scale = 1.0;
	}
	else if (maxXY > 0.6)
	{
		context[id].scale = 2.0;
	}
	else if (maxXY > 0.4)
	{
		context[id].scale = 3.0;
	}
	else if (maxXY > 0.4)
	{
		context[id].scale = 4.0;
	}
	else if (maxXY > 0.1)
	{
		context[id].scale = 5.0;
	}
	else if (maxXY > 0.08)
	{
		context[id].scale = 6.0;
	}
	else if (maxXY > 0.04)
	{
		context[id].scale = 12.0;
	}
}

float absd(float a, float b)
{
	float tmp = a > b ? a - b : b - a;

	return tmp > 0.0 ? tmp : -1.0 * tmp;
}

float getRealZ(float x, float y, float z)
{
	if (model_amount == 0)
	{
		return 0.0;
	}

	for (int i = 0; i < models->count * 3; i += 3)
	{
		if (absd(models->vertex[i], x) < 0.01
			&& absd(models->vertex[i + 1], y) < 0.01
			&& absd(models->vertex[i + 2], z) < 0.01)
		{
			return models->vertex[i + 2];
		}
	}
	return 0.0;
}

void drawDots()
{
	if (model_amount == 0)
	{
		return;
	}

	dot_t* n = dot_get(-1);
	int dotId = 1;
	while (n != NULL)
	{
		if (n->type == CURVE && showModel != CURVE)
		{
			n = n->next;
			continue;
		}
		color_t* c = &defaultDotColor;
		if (showModel == CURVE && n->c.r >= 0.0 && n->c.r <= 1.0)
		{
			c = &n->c;
		}
		ogl_drawDot(&n->p, c, dotRadius);
		if (labeled && n->type != CURVE)
		{
			ogl_drawLabel(&n->p, c, dotId++, dotRadius, n->p.z >= 0 ? models->max[Z] : models->min[Z]);
		}
		n = n->next;
	}


}

void drawAnchors()
{
	if (model_amount == 0)
	{
		return;
	}

	dot_t* n = anchor_get(-1);
	int dotId = 1;
	while (n != NULL)
	{
		if (n->type == CURVE && showModel != CURVE)
		{
			n = n->next;
			continue;
		}
		color_t* c = &defaultAnchorColor;
		if (showModel == CURVE && n->c.r >= 0.0 && n->c.r <= 1.0)
		{
			c = &n->c;
		}
		ogl_drawDot(&n->p, c, anchorRadius);
		if (alabeled && n->type != CURVE)
		{
			ogl_drawLabel(&n->p, c, dotId++, anchorRadius, n->p.z >= 0 ? models->max[Z] : models->min[Z]);
		}
		n = n->next;
	}


}

void drawCurves()
{
	curve_t* p = curve_get();
	while (p != NULL && p->pointNum >= 3)
	{
		ogl_drawLine(p->lines1, p->line1Size);
		ogl_drawLine(p->lines2, p->line2Size);
		p = p->next;
	}
}

void drawGrid()
{
	glLineWidth(1.5);
	glEnable(GL_LINE_SMOOTH);
	glColor3f(0.0, 0.0, 1.0);
	glBegin(GL_LINES);
	for (float i = models->min[Z]; i <= models->max[Z]; i += (models->max[Z] - models->min[Z]) / 10)
	{
		glVertex3f(models->max[0], models->max[1], i);
		glVertex3f(models->max[0], models->min[1], i);
		glVertex3f(models->min[0], models->max[1], i);
		glVertex3f(models->min[0], models->min[1], i);
		glVertex3f(models->max[0], models->max[1], i);
		glVertex3f(models->min[0], models->max[1], i);
		glVertex3f(models->max[0], models->min[1], i);
		glVertex3f(models->min[0], models->min[1], i);
	}
	glEnd();
}

void onDisplay()
{

	float dx = (float)width / height;
	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(-1.1 * dx, 1.1 * dx, -1.1, 1.1, -2, 2);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glPushMatrix();
	if (models != NULL || context != NULL)
	{
		glRotatef(context[model_index].rotation[0], 1, 0, 0);
		glRotatef(context[model_index].rotation[1], 0, 1, 0);
		glRotatef(context[model_index].rotation[2], 0, 0, 1);
		glScalef(context[model_index].scale, context[model_index].scale, context[model_index].scale);
		glTranslatef(context[model_index].x, context[model_index].y, context[model_index].z);

		glColor3f(1.0, 1.0, 1.0);
		if (showModel != NONE && showModel != DOWN_SAMPLE_ONLY)
			ogl_drawModel(models);

		switch (showModel)
		{
		case LANDMARK:
			drawDots();
			break;
		case ANCHOR:
			drawDots();
			drawAnchors();
			break;
		case DOWN_SAMPLE:
			drawDots();
			drawAnchors();
			ogl_drawDownSampleModel(models, dotRadius, &downSampleOffset);

			break;
		case CURVE:
			drawDots();
			drawCurves();
			break;
		case DOWN_SAMPLE_ONLY:
			ogl_drawDownSampleModel(models, dotRadius, &downSampleOffset);

			break;
		}
	}

	glPopMatrix();

	glFlush();
#ifdef _WIN32
	SwapBuffers(dc);
#elif __linux__
	glXSwapBuffers(__glDisplay__, __glWindow__);
#endif


}

void getSpecimenCoordinate(int x, int y, point_t* p, char* buf)
{
	if (model_amount == 0)
	{
		return;
	}

	show_mode_t oldModel = showModel;
	showModel = SPECIMEN;
	onDisplay();

	glPushMatrix();
	glRotatef(context[model_index].rotation[0], 1, 0, 0);
	glRotatef(context[model_index].rotation[1], 0, 1, 0);
	glRotatef(context[model_index].rotation[2], 0, 0, 1);
	glScalef(context[model_index].scale, context[model_index].scale, context[model_index].scale);
	glTranslatef(context[model_index].x, context[model_index].y, context[model_index].z);
	ogl_getObjCoordinate(x, y, &p->x, &p->y, &p->z, buf);
	glPopMatrix();

	showModel = oldModel;
	onDisplay();
}

void specimen_del(int id)
{
	if (models == NULL)
	{
		return;
	}
	model_t* model = models;

	if (model == NULL)
	{
		return;
	}

	Tcl_Free((char*)model);
	model = NULL;
	 
	//_CrtDumpMemoryLeaks();

}

int valiadteDot(point_t* p)
{
	if (model_amount == 0)
	{
		return 1;
	}

	if (p->x < models->min[X] || p->x > models->max[X])
		return 0;
	if (p->y < models->min[Y] || p->y > models->max[Y])
		return 0;
	if (p->z< models->min[Z] || p->z > models->max[Z])
		return 0;
	return 1;
}

TCL_CMD(add)
{
	const char* shape = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(shape, "specimen") == 0)
	{
		int id;

		Tcl_GetIntFromObj(interp, objv[3], &id);
		memset(models, 0, sizeof(model_t));
		ogl_loadModel(Tcl_GetStringFromObj(objv[2], NULL), models);
		temp_index = id;
		memcpy(&deltas[temp_index], &(models->delta), sizeof(models->delta));


		float maxXY = models->max[0] > models->max[1] ? models->max[0] : models->max[1];
		if (maxXY > 0.8)
		{
			dotRadius = 0.01f;
			anchorRadius = 0.01f;
		}
		else if (maxXY > 0.6)
		{
			dotRadius = 0.008f;
			anchorRadius = 0.008f;
		}
		else if (maxXY > 0.4)
		{
			dotRadius = 0.006f;
			anchorRadius = 0.006f;
		}
		else if (maxXY > 0.4)
		{
			dotRadius = 0.004f;
			anchorRadius = 0.004f;
		}
		else if (maxXY > 0.1)
		{
			dotRadius = 0.002f;
			anchorRadius = 0.002f;
		}
		else if (maxXY > 0.08)
		{
			dotRadius = 0.001f;
			anchorRadius = 0.001f;
		}
		else if (maxXY <= 0.08)
		{
			dotRadius = 0.001f;
			anchorRadius = 0.001f;
		}
		resetContext(id, maxXY);
		dot_slice_index(id);
		anchor_slice_index(id);

	}
	else if (strcmp(shape, "rawdot") == 0)
	{
		if (model_amount == 0)
		{
			return TCL_OK;
		}
		point_t p;
		Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
		Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
		Tcl_GetDoubleFromObj(interp, objv[4], &p.z);

		p.x = ogl_calCoordinate(p.x, 0, deltas[temp_index]);
		p.y = ogl_calCoordinate(p.y, 1, deltas[temp_index]);
		p.z = ogl_calCoordinate(p.z, 2, deltas[temp_index]);
		/*p.x = ogl_calCoordinate(p.x, 0, models->delta);
		p.y = ogl_calCoordinate(p.y, 1, models->delta);
		p.z = ogl_calCoordinate(p.z, 2, models->delta);*/

		if (!valiadteDot(&p))
		{
			TCL_RESULT3("Error: cannot add dot at %f %f %f", p.x, p.y, p.z);
			return TCL_OK;
		}

		color_t c = { -1.0, -1.0, -1.0 };
		int size = dot_add(&p, &c);

		char* msg = Tcl_Alloc(512);
		sprintf(msg, "add dot (total = %d): %f %f %f\n", size, p.x, p.y, p.z);
		Tcl_SetResult(interp, msg, TCL_DYNAMIC);


	}

	else if (strcmp(shape, "rawanchor") == 0)
	{
		if (model_amount == 0)
		{
			return TCL_OK;
		}
		point_t p;
		Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
		Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
		Tcl_GetDoubleFromObj(interp, objv[4], &p.z);//

		p.x = ogl_calCoordinate(p.x, 0, deltas[temp_index]); //original is models->delta
		p.y = ogl_calCoordinate(p.y, 1, deltas[temp_index]);
		p.z = ogl_calCoordinate(p.z, 2, deltas[temp_index]);
		/*p.x = ogl_calCoordinate(p.x, 0, models->delta);
		p.y = ogl_calCoordinate(p.y, 1, models->delta);
		p.z = ogl_calCoordinate(p.z, 2, models->delta);*/

		if (!valiadteDot(&p))
		{
			TCL_RESULT3("Error: cannot add dot at %f %f %f", p.x, p.y, p.z);
			return TCL_OK;
		}

		color_t c = { -1.0, -1.0, -1.0 };
		int size = anchor_add(&p, &c);

		char* msg = Tcl_Alloc(512);
		sprintf(msg, "add anchor (total = %d): %f %f %f\n", size, p.x, p.y, p.z);
		Tcl_SetResult(interp, msg, TCL_DYNAMIC);

		anchorPlaced = 1;


	}

	else if (strcmp(shape, "dot") == 0)
	{
		point_t p;
		Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
		Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
		Tcl_GetDoubleFromObj(interp, objv[4], &p.z);
		if (!valiadteDot(&p))
		{
			TCL_RESULT3("Error: cannot add dot at %f %f %f", p.x, p.y, p.z);
			return TCL_OK;
		}

		color_t c = { -1.0, -1.0, -1.0 };
		int size = dot_add(&p, &c);

		char* msg = Tcl_Alloc(512);
		sprintf(msg, "add dot (total = %d): %f %f %f\n", size, p.x, p.y, p.z);
		Tcl_SetResult(interp, msg, TCL_DYNAMIC);


	}
	else if (strcmp(shape, "anchor") == 0)
	{
		point_t p;
		Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
		Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
		Tcl_GetDoubleFromObj(interp, objv[4], &p.z);
		if (!valiadteDot(&p))
		{
			TCL_RESULT3("Error: cannot add dot at %f %f %f", p.x, p.y, p.z);
			return TCL_OK;
		}

		color_t c = { -1.0, -1.0, -1.0 };
		int size = anchor_add(&p, &c);
		anchorPlaced = 1;

		char* msg = Tcl_Alloc(512);
		sprintf(msg, "add dot (total = %d): %f %f %f\n", size, p.x, p.y, p.z);
		Tcl_SetResult(interp, msg, TCL_DYNAMIC);


	}
	else if (strcmp(shape, "downsample") == 0)
	{
		int id;
		Tcl_GetIntFromObj(interp, objv[3], &id);
		FILE* file = fopen(Tcl_GetStringFromObj(objv[2], NULL), "r");
		if (file)
		{

			/*if (ogl_loadDownSampleModel(file, &models[0]) != 0)
			{
				downsampled = 1;//
			}*/
			ogl_loadDownSampleModel(file, models);
			fclose(file);
		}
	}
	else if (strcmp(shape, "landmark") == 0)
	{
		int id = 0, amount = 0;
		Tcl_GetIntFromObj(interp, objv[2], &id);
		Tcl_GetIntFromObj(interp, objv[3], &amount);
		ogl_loadLandMark(Tcl_GetStringFromObj(objv[4], NULL),
			models, id, amount);
	}
	else if (strcmp(shape, "curve") == 0)
	{
		int beg, mid, end;
		Tcl_GetIntFromObj(interp, objv[2], &beg);
		Tcl_GetIntFromObj(interp, objv[3], &mid);
		Tcl_GetIntFromObj(interp, objv[4], &end);

		for (int i = 0; i < model_amount; i++)
		{
			curve_addDot(i, dot_get_dot(i, beg));
			dot_t* d = dot_get_dot(i, mid);
			d->c.r = 0.0;
			d->c.g = 0.0;
			d->c.b = 1.0;
			curve_addDot(i, d);
			curve_addDot(i, dot_get_dot(i, end));
		}
	}
	onDisplay();

	return TCL_OK;
}

int tclSubCmdID(Tcl_Obj* const objv[], int num, const char* subCmd[], int id)
{
	for (int i = 1; i <= num; i++)
	{
		const char* cmd = Tcl_GetStringFromObj(objv[i], NULL);
		if (strcmp(cmd, subCmd[i - 1]) != 0)
		{
			return 0;
		}
	}
	return id;
}

float calUnCoordinate(float value, int id, float* delta)
{
	if (delta[3] > 1.0) {
		return value * delta[3] + delta[id];
	}
	return value;
}

TCL_CMD(show)
{

	const char* shape = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(shape, "specimen") == 0)
	{
		const char* attr = Tcl_GetStringFromObj(objv[2], NULL);
		if (strcmp(attr, "xyz") == 0)
		{
			int x, y;
			Tcl_GetIntFromObj(interp, objv[3], &x);
			Tcl_GetIntFromObj(interp, objv[4], &y);

			char buf[1000] = { 0 };
			point_t p;
			getSpecimenCoordinate(x, y, &p, buf);

			char* msg = Tcl_Alloc(512);
			sprintf(msg, "%f %f %f", p.x, p.y, p.z);
			Tcl_SetResult(interp, msg, TCL_DYNAMIC);
		}


	}
	else if (strcmp(shape, "landmark") == 0)
	{
		const char* attr = Tcl_GetStringFromObj(objv[2], NULL);
		if (strcmp(attr, "xyz") == 0)
		{
			int id;
			Tcl_GetIntFromObj(interp, objv[3], &id);
			temp_index = id;

			char* msg = Tcl_Alloc(2048);
			strcpy(msg, "");

			dot_t* n = dot_get(id);
			while (n != NULL)
			{
				if (n->type == LANDMARK)
				{
					char pStr[64] = { 0 };
					snprintf(pStr, 60, "%f %f %f ",
						calUnCoordinate(n->p.x, X, deltas[temp_index]), //for original, change back to models->delta
						calUnCoordinate(n->p.y, Y, deltas[temp_index]),
						calUnCoordinate(n->p.z, Z, deltas[temp_index]));
					/*calUnCoordinate(n->p.x, X, models[id].delta),
					calUnCoordinate(n->p.y, Y, models[id].delta),
					calUnCoordinate(n->p.z, Z, models[id].delta));*/

					strcat(msg, pStr);
				}
				n = n->next;
				D1("msg=%s", msg);
			}

			Tcl_SetResult(interp, msg, TCL_DYNAMIC);



			return TCL_OK;
		}
		else if (strcmp(attr, "id") == 0)
		{
			TCL_RESULT1("%d", dot_selected_id());
			return TCL_OK;
		}
	}

	else if (strcmp(shape, "anchor") == 0)
	{
		const char* attr = Tcl_GetStringFromObj(objv[2], NULL);
		if (strcmp(attr, "xyz") == 0)
		{
			int id;
			Tcl_GetIntFromObj(interp, objv[3], &id);

			char* msg = Tcl_Alloc(2048);
			strcpy(msg, "");

			dot_t* n = anchor_get(id);

			while (n != NULL)
			{
				if (n->type == ANCHOR)
				{
					char pStr[64] = { 0 };
					snprintf(pStr, 60, "%f %f %f ",
						calUnCoordinate(n->p.x, X, deltas[temp_index]),
						calUnCoordinate(n->p.y, Y, deltas[temp_index]),
						calUnCoordinate(n->p.z, Z, deltas[temp_index]));
					/*calUnCoordinate(n->p.x, X, models[id].delta),
					calUnCoordinate(n->p.y, Y, models[id].delta),
					calUnCoordinate(n->p.z, Z, models[id].delta));*/
					strcat(msg, pStr);
				}
				n = n->next;
				D1("msg=%s", msg);
			}



			Tcl_SetResult(interp, msg, TCL_DYNAMIC);
			return TCL_OK;
		}
		else if (strcmp(attr, "id") == 0)
		{
			TCL_RESULT1("%d", anchors_selected_id());
			return TCL_OK;
		}
	}

	onDisplay();
	return TCL_OK;
}

TCL_CMD(setWindow)
{
	const char* attr = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(attr, "id") == 0)
	{
		HWND hwnd;
		Tcl_GetIntFromObj(interp, objv[2], (int*)& hwnd);

		if (models != NULL)
		{
			Tcl_Free((char*)models);
			models = NULL;
		}

		if (context != NULL)
		{
			Tcl_Free((char*)context);
			context = NULL;
		}
		model_index = 0;
		model_amount = 0;
		dot_slice_amount(0);
		anchors_slice_amount(0);

		setWindowId(hwnd);
		 
	}
	else if (strcmp(attr, "size") == 0)
	{
		Tcl_GetIntFromObj(interp, objv[2], &width);
		Tcl_GetIntFromObj(interp, objv[3], &height);

		float dx = (float)width / height;
		glViewport(0, 0, width, height);

		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho(-0.1 * dx, 0.1 * dx, -0.1, 0.1, -2, 2);
		TCL_RESULT2("Window size change to %d %d\n", width, height);
	}
	else if (strcmp(attr, "mode") == 0)
	{
		const char* mode = Tcl_GetStringFromObj(objv[2], NULL);
		if (strcmp(mode, "digitize") == 0) {
			showModel = LANDMARK;
		}
		if (strcmp(mode, "anchor") == 0) {
			showModel = ANCHOR;
		}
		else if (strcmp(mode, "surface") == 0) {
			showModel = DOWN_SAMPLE;
		}
		else if (strcmp(mode, "surfaceonly") == 0) {
			showModel = DOWN_SAMPLE_ONLY;
		}
		else if (strcmp(mode, "curve") == 0) {
			showModel = CURVE;
		}
		else if (strcmp(mode, "none") == 0) {
			showModel = NONE;
		}
	}
	onDisplay();
	return TCL_OK;
}

#define ANGLE_REDUCE(angle) while (angle > 360.0) angle -= 360.0; while (angle < -360.0) angle += 360.0
TCL_CMD(setSpecimen)
{
	const char* attr = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(attr, "angle") == 0)
	{
		if (model_amount == 0)
		{
			return TCL_OK;
		}
		const char* d = Tcl_GetStringFromObj(objv[2], NULL);
		double r;
		Tcl_GetDoubleFromObj(interp, objv[3], &r);
		/*apply rotations*/
		switch (d[0]) {
		case 'x':
			context[model_index].rotation[0] += (float)r;
			ANGLE_REDUCE(context[model_index].rotation[0]);
			break;
		case 'y':
			context[model_index].rotation[1] += (float)r;
			ANGLE_REDUCE(context[model_index].rotation[1]);
			break;
		case 'z':
			context[model_index].rotation[2] += (float)r;
			ANGLE_REDUCE(context[model_index].rotation[2]);
			break;
		}
		char* msg = Tcl_Alloc(512);
		sprintf(msg, "Rotate: %f %f %f",
			context[model_index].rotation[0],
			context[model_index].rotation[1],
			context[model_index].rotation[2]);
		Tcl_SetResult(interp, msg, TCL_DYNAMIC);


	}
	else if (strcmp(attr, "scale") == 0)
	{
		if (model_amount == 0)
		{
			return TCL_OK;
		}

		const char* value = Tcl_GetStringFromObj(objv[2], NULL);
		/*apply scaling*/
		if (strcmp(value, "in") == 0)
		{
			context[model_index].scale += 0.1;
		}
		else if (strcmp(value, "out") == 0 && context[model_index].scale > 0.1)
		{
			context[model_index].scale -= 0.1;
		}
	}
	//changed amount to allocate
	else if (strcmp(attr, "allocate") == 0)
	{

		int amount = 0;
		Tcl_GetIntFromObj(interp, objv[2], &amount);

		if (models != NULL)
		{
			Tcl_Free((char*)models);
			models = NULL;
		}
		if (context != NULL)
		{
			Tcl_Free((char*)context);
			context = NULL;
		}
		if (amount > 0) {
			models = (model_t*)Tcl_Alloc(sizeof(model_t));
			context = (context_t*)Tcl_Alloc(amount * sizeof(context_t));
			//memset(models, 0, sizeof(model_t));
			//memset(context, 0, sizeof(context_t));
			//memset(deltas, NULL, sizeof(float*));
		}

		model_index = 0;
		model_amount = amount;

		dot_slice_amount(amount);
		anchors_slice_amount(amount);
		 

		return TCL_OK;
	}
	else if (strcmp(attr, "id") == 0)
	{
		if (model_amount == 0)
		{
			return TCL_OK;
		}

		int index = 0;
		Tcl_GetIntFromObj(interp, objv[2], &index);
		if (anchorPlaced == 1) {
			if (dot_slice_index(index) >= 0 && anchor_slice_index(index) >= 0)
			{
				model_index = index;
				temp_index = index;
				float maxXY = models->max[0] > models->max[1] ? models->max[0] : models->max[1];
				resetContext(maxXY, index);
				TCL_RESULT1("Reset context for %d", model_index);
			}
		}
		else {
			if (dot_slice_index(index) >= 0)
			{
				model_index = index;
				temp_index = index;
				float maxXY = models->max[0] > models->max[1] ? models->max[0] : models->max[1];
				resetContext(maxXY, index);
				TCL_RESULT1("Reset context for %d", model_index);
			}
		}
	}
	onDisplay();
	return TCL_OK;
}


TCL_CMD(setDownSample)
{
	const char* attr = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(attr, "offsetBegin") == 0)
	{
		int x, y;
		Tcl_GetIntFromObj(interp, objv[2], &x);
		Tcl_GetIntFromObj(interp, objv[3], &y);

		getSpecimenCoordinate(x, y, &downSampleOffsetBeg, NULL);
		return TCL_OK;
	}
	else if (strcmp(attr, "offsetEnd") == 0)
	{
		int x, y;
		Tcl_GetIntFromObj(interp, objv[2], &x);
		Tcl_GetIntFromObj(interp, objv[3], &y);

		point_t p;
		getSpecimenCoordinate(x, y, &p, NULL);

		downSampleOffset.x = p.x - downSampleOffsetBeg.x;
		downSampleOffset.y = p.y - downSampleOffsetBeg.y;
		downSampleOffset.z = 0.0;//p.z - downSampleOffsetBeg.z;
	}
	onDisplay();

	return TCL_OK;
}

TCL_CMD(setDot)
{
	const char* attr = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(attr, "selected") == 0)
	{
		int x, y;
		Tcl_GetIntFromObj(interp, objv[2], &x);
		Tcl_GetIntFromObj(interp, objv[3], &y);

		char buf[1000] = { 0 };
		point_t p;
		getSpecimenCoordinate(x, y, &p, buf);

		if (showModel == LANDMARK)
		{
			if (dot_select(&p, dotRadius))
			{
				TCL_RESULT2("No dot selected at %d %d\n", x, y);
			}
		}
		else if (showModel == ANCHOR) {
			if (anchor_select(&p, anchorRadius))
			{
				TCL_RESULT2("No anchor selected at %d %d\n", x, y);
			}
		}
	}
	else if (strcmp(attr, "coordinate") == 0)
	{
		point_t p;
		Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
		Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
		Tcl_GetDoubleFromObj(interp, objv[4], &p.z);

		if (!valiadteDot(&p))
		{
			TCL_RESULT3("Error: cannot move dot to %f %f %f", p.x, p.y, p.z);
			return TCL_OK;
		}

		dot_move(&p);
	}
	else if (strcmp(attr, "dcolor") == 0)
	{
		Tcl_GetDoubleFromObj(interp, objv[2], &defaultDotColor.r);
		Tcl_GetDoubleFromObj(interp, objv[3], &defaultDotColor.g);
		Tcl_GetDoubleFromObj(interp, objv[4], &defaultDotColor.b);
	}
	/*assigns default anchor color, other anchor color is used for color selection by user*/
	else if (strcmp(attr, "acolor") == 0)
	{
		Tcl_GetDoubleFromObj(interp, objv[2], &defaultAnchorColor.r);
		Tcl_GetDoubleFromObj(interp, objv[3], &defaultAnchorColor.g);
		Tcl_GetDoubleFromObj(interp, objv[4], &defaultAnchorColor.b);
	}

	else if (strcmp(attr, "color") == 0)
	{
		color_t c;
		Tcl_GetDoubleFromObj(interp, objv[2], &c.r);
		Tcl_GetDoubleFromObj(interp, objv[3], &c.g);
		Tcl_GetDoubleFromObj(interp, objv[4], &c.b);

		dot_color(&c);
	}

	else if (strcmp(attr, "anchorColor") == 0)
	{
		color_t c;
		Tcl_GetDoubleFromObj(interp, objv[2], &c.r);
		Tcl_GetDoubleFromObj(interp, objv[3], &c.g);
		Tcl_GetDoubleFromObj(interp, objv[4], &c.b);

		anchor_color(&c);
	}

	else if (strcmp(attr, "labeled") == 0)
	{
		Tcl_GetIntFromObj(interp, objv[2], &labeled);
	}
	else if (strcmp(attr, "alabeled") == 0)
	{
		Tcl_GetIntFromObj(interp, objv[2], &alabeled);
	}

	else if (strcmp(attr, "radius") == 0)
	{
		Tcl_GetDoubleFromObj(interp, objv[2], &dotRadius);
	}

	else if (strcmp(attr, "anchorRadius") == 0)
	{
		Tcl_GetDoubleFromObj(interp, objv[2], &anchorRadius);
	}
	onDisplay();
	return TCL_OK;
}

TCL_CMD(del)
{
	const char* shape = Tcl_GetStringFromObj(objv[1], NULL);
	if (strcmp(shape, "dot") == 0)
	{
		int size = 0;
		if (objc > 2)
		{
			point_t p;
			Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
			Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
			Tcl_GetDoubleFromObj(interp, objv[4], &p.z);
			size = dot_del(&p);
		}
		else
		{
			size = dot_del_selected();
		}
		TCL_RESULT1("Number of landmarks: %d\n", size);
	}
	else if (strcmp(shape, "dots") == 0)
	{
		//dots_free();
	}
	else if (strcmp(shape, "anchor") == 0)
	{
		int size = 0;
		if (objc > 2)
		{
			point_t p;
			Tcl_GetDoubleFromObj(interp, objv[2], &p.x);
			Tcl_GetDoubleFromObj(interp, objv[3], &p.y);
			Tcl_GetDoubleFromObj(interp, objv[4], &p.z);
			size = anchor_del(&p);
		}
		else
		{
			size = anchor_del_selected();
		}
		TCL_RESULT1("Number of anchors: %d\n", size);
	}

	else if (strcmp(shape, "anchors") == 0)
	{
		//anchors_free();
	}
	else if (strcmp(shape, "specimen") == 0)
	{
		int id;
		Tcl_GetIntFromObj(interp, objv[2], &id);
		specimen_del(id);
	}

	else if (strcmp(shape, "specimens") == 0)
	{
		/*specimen_del(0);

		if (models != NULL)
		{
			Tcl_Free((char*)models);
			models = NULL;
		}
		if (context != NULL)
		{
			Tcl_Free((char*)context);
			context = NULL;
		}*/
	}
	onDisplay();



	return TCL_OK;
}

TCL_CMD(loadDgt)
{
	const char* fileName = Tcl_GetStringFromObj(objv[1], NULL);

	model_index = 0;
	//model_amount = ogl_loadDgtModel(fileName, &models);
	//memcpy(&deltas[model_index], &(models->delta), sizeof(models->delta));

	/*if (context != NULL)
	{
		Tcl_Free((char*)context);
		context = NULL;
	}*/
	if (model_amount > 0) {
		context = (context_t*)Tcl_Alloc(model_amount * sizeof(context_t));
		//context = (context_t*)Tcl_Alloc(sizeof(context_t));
		int id = 0;
		float maxXY = models->max[0] > models->max[1] ? models->max[0] : models->max[1];
		if (maxXY > 0.8)
		{
			dotRadius = 0.01f;
		}
		else if (maxXY > 0.6)
		{
			dotRadius = 0.008f;
		}
		else if (maxXY > 0.4)
		{
			dotRadius = 0.006f;
		}
		else if (maxXY > 0.4)
		{
			dotRadius = 0.004f;
		}
		else if (maxXY > 0.1)
		{
			dotRadius = 0.002f;
		}
		else if (maxXY > 0.08)
		{
			dotRadius = 0.001f;
		}
		else if (maxXY <= 0.08)
		{
			dotRadius = 0.001f;
		}
		resetContext(id, maxXY);

		 
	}



	onDisplay();
	return TCL_OK;
}

/*
* Tkogl2_Init -- Called when Tcl loads your extension.
*/
int DLLEXPORT Tkogl2_Init(Tcl_Interp* interp)
{
	if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
		return TCL_ERROR;
	}

	if (Tcl_PkgProvide(interp, "Tkogl2", "1.0") == TCL_ERROR) {
		return TCL_ERROR;
	}

	/*implement these new commands so that C understands*/
	/*Tcl_CreateObjCommand defines a new command in interp and associates it with procedure proc such that whenever name is invoked as a Tcl command
	(via Tcl_Eval), the Tcl interpreter will call proc to process the command.*/
	Tcl_CreateObjCommand(interp, "add", add, 0, 0);
	Tcl_CreateObjCommand(interp, "show", show, 0, 0);
	Tcl_CreateObjCommand(interp, "setWindow", setWindow, 0, 0);
	Tcl_CreateObjCommand(interp, "setSpecimen", setSpecimen, 0, 0);
	Tcl_CreateObjCommand(interp, "setDownSample", setDownSample, 0, 0);
	Tcl_CreateObjCommand(interp, "setDot", setDot, 0, 0);
	Tcl_CreateObjCommand(interp, "del", del, 0, 0);

	// high level interface to simple the R code
	Tcl_CreateObjCommand(interp, "loadDgt", loadDgt, 0, 0);

	return TCL_OK;
}

