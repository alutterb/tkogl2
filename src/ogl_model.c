#include "def.h"
#include <math.h>

static void dump(float* data) {
	static int dumped = 0;
	if (data == NULL) return;
	if (dumped) return;
	dumped = 1;
	for (int i = 0; i < 20; i += 4) {
		printf("%f %f %f %f\n", data[i], data[i + 1], data[i + 2], data[i + 3]);
	}
}

void ogl_DrawTriangle()
{
	glBegin(GL_TRIANGLES);
	glColor3f(0.1, 0.2, 0.3);
	glVertex3f(0, 0, 0);
	glVertex3f(1, 0, 0);
	glVertex3f(0, 1, 0);
	glEnd();
}

void ogl_drawModel(model_t* model)
{
	glEnableClientState(GL_VERTEX_ARRAY); /*enables vertex array for writing and used during rendering*/
	glEnableClientState(GL_NORMAL_ARRAY); /*enables normal array for writing and used during rendering*/
	glVertexPointer(3, GL_FLOAT, 0, model->vertex); /*defines an array of vertex, we are storing it in the model's vertex*/

	/*if color exists, write it to color matrix using model's color*/
	if (model->color != NULL) {
		ogl_disableLight();
		glEnableClientState(GL_COLOR_ARRAY);
		/*glColorPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)*/
		glColorPointer(4, GL_FLOAT, 0, model->color);
	}
	/*if not, use regular lighting*/
	else {
		ogl_enableLight();
	}

	glNormalPointer(GL_FLOAT, 0, model->normal); /*defines array of normals, using model's normal*/
	/*void glDrawArrays(GLenum mode, GLint first, GLsizei count); mode - specifies what kind of primitive to render. first - specifies the starting index in the enabled arrays. count - specifies the number of
	indices to be rendered*/
	glDrawArrays(GL_TRIANGLES, 0, model->count); /*renders the primitives from array data*/
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
}

/*max and min array of x,y,z max and min values*/
void ogl_drawCube(float* max, float* min)
{
	/* Draw a simple cube. */
	glBegin(GL_QUADS); /*treats each group of four vertices as an independent quadrilateral*/

	glColor4f(0.8f, 0.8f, 0.8f, 0.1f);
	glVertex3f(max[X], max[Y], max[Z]);
	glVertex3f(max[X], max[Y], min[Z]);
	glVertex3f(min[X], max[Y], min[Z]);
	glVertex3f(min[X], max[Y], max[Z]);

	glVertex3f(max[X], min[Y], max[Z]);
	glVertex3f(min[X], min[Y], max[Z]);
	glVertex3f(min[X], min[Y], min[Z]);
	glVertex3f(max[X], min[Y], min[Z]);

	glVertex3f(max[X], max[Y], max[Z]);
	glVertex3f(min[X], max[Y], max[Z]);
	glVertex3f(min[X], min[Y], max[Z]);
	glVertex3f(max[X], min[Y], max[Z]);

	glVertex3f(max[X], max[Y], min[Z]);
	glVertex3f(max[X], min[Y], min[Z]);
	glVertex3f(min[X], min[Y], min[Z]);
	glVertex3f(min[X], max[Y], min[Z]);

	glVertex3f(min[X], max[Y], max[Z]);
	glVertex3f(min[X], max[Y], min[Z]);
	glVertex3f(min[X], min[Y], min[Z]);
	glVertex3f(min[X], min[Y], max[Z]);

	glVertex3f(max[X], max[Y], max[Z]);
	glVertex3f(max[X], min[Y], max[Z]);
	glVertex3f(max[X], min[Y], min[Z]);
	glVertex3f(max[X], max[Y], min[Z]);

	glEnd();
}

void ogl_grid(float* max, float* min)
{
	glBegin(GL_LINES); /*treats each pair of vertices as an independent line segment*/
	glColor4f(0.8f, 0.8f, 0.8f, 0.2f);
	for (float z = min[Z]; z <= max[Z]; z += (max[Z] - min[Z]) / 10.0)
	{
		/*builds grid along Z axis*/
		glVertex3f(min[X], max[Y], z);
		glVertex3f(max[X], max[Y], z);

		glVertex3f(max[X], max[Y], z);
		glVertex3f(max[X], min[Y], z);

		glVertex3f(min[X], min[Y], z);
		glVertex3f(max[X], min[Y], z);

		glVertex3f(min[X], max[Y], z);
		glVertex3f(min[X], min[Y], z);
	}

	for (float x = min[X]; x <= max[X]; x += (max[X] - min[X]) / 10.0)
	{
		/*Builds grid along x axis*/
		glVertex3f(x, min[Y], max[Z]);
		glVertex3f(x, max[Y], max[Z]);

		glVertex3f(x, max[Y], max[Z]);
		glVertex3f(x, max[Y], min[Z]);

		glVertex3f(x, min[Y], min[Z]);
		glVertex3f(x, max[Y], min[Z]);

		glVertex3f(x, min[Y], max[Z]);
		glVertex3f(x, min[Y], min[Z]);
	}

	for (float y = min[Y]; y <= max[Y]; y += (max[Y] - min[Y]) / 10.0)
	{
		/*builds grid along y axis*/
		glVertex3f(min[X], y, max[Z]);
		glVertex3f(max[X], y, max[Z]);

		glVertex3f(max[X], y, max[Z]);
		glVertex3f(max[X], y, min[Z]);

		glVertex3f(min[X], y, min[Z]);
		glVertex3f(max[X], y, min[Z]);

		glVertex3f(min[X], y, max[Z]);
		glVertex3f(min[X], y, min[Z]);
	}

	glEnd();
}

/*raster = matrix of cells organized into rows and columns where each cell contains a value representing information, such as temperature*/
void ogl_drawLabel(point_t* p, color_t* c, int id, GLdouble dotRadius, float z)
{
	glPushMatrix(); /*Creates matrix 1 on he top*/
	/*applies the following transformations*/
	glTranslatef(p->x, p->y, z);/*multiplies current matrix by a translation matrix (x,y,z) coords of translation vector*/
	glColor3f(c->r, c->g, c->b); /*sets the color*/
	glRasterPos3f(dotRadius * 2, 0.0, p->z); /*Specifies raster position for pixel operations*/
	/*draw text*/
	if (id >= 10)
		glutBitmapCharacter(GLUT_BITMAP_9_BY_15, '0' + (id / 10));
	glutBitmapCharacter(GLUT_BITMAP_9_BY_15, '0' + (id % 10));
	glPopMatrix(); /*removes matrix 1 from stack (i.e. stop doing transformations)*/
}

void ogl_drawDot(point_t* p, color_t* c, GLdouble radius)
{
	/*creates simple shapes from respective math equation*/
	GLUquadric* qobj = gluNewQuadric();
	gluQuadricNormals(qobj, GLU_SMOOTH); /*specifies what kind of normals are desired for quadrics, here we are generating one normal for every vertex
	of the quadric*/
	glPushMatrix();/*begin transformations*/
	glTranslatef(p->x, p->y, p->z); /*position of dot*/
	glColor3f(c->r, c->g, c->b);/*color of dot*/
	glutSolidSphere(radius, 10, 10); /*construct dot*/
	glPopMatrix();/*remove transformation matrix*/
}

void ogl_drawDownSampleModel(model_t* model, GLdouble radius, point_t* downSampleOffset)
{
	/*downsample calculations*/
	double dx = 0.0, dy = 0.0, dz = 0.0;
	dx = model->dsMax[X] - model->max[X];
	dy = model->dsMax[Y] - model->max[Y];
	dz = model->dsMax[Z] - model->max[Z];
	D3("%f, %f, %f", model->delta[0], model->delta[1], model->delta[2]);
	/*begin transformation*/
	glPushMatrix();
	glColor3f(0.0, 0.0, 1.0); /*set the dot color to blue*/
	/*iterates through each downsample dot and places it on the i,i+1,i+2 vertex with a radius of 10*/
	for (int i = 0; i < model->dsCount * 3; i += 3)
	{
		double tx = model->dsVertex[i];
		double ty = model->dsVertex[i + 1];
		double tz = model->dsVertex[i + 2];

		glTranslatef(tx, ty, tz); /*will multiply current matrix by translation matrix generated by tx, ty, tz*/
		/*void glutSolidSphere(GLdouble radius, GLint slices, GLint stacks)*/
		/*slices - number of subidivisions around the Z axis (similar to lines of longitude)*/
		/*stacks - number of subdivisions along the Z axis (similar to lines of latitude)*/
		glutSolidSphere(radius, 10, 10);
		glTranslatef(0 - tx, 0 - ty, 0 - tz);
	}
	glPopMatrix(); /*end transformation*/
}
/*difference of squares*/
float dsquare(float a, float b)
{
	return (a * a - b * b);
}

/*divided difference of squares*/
float formula1(float x1, float x2)
{
	return dsquare(x1, x2) / (x2 - x1);
}

void ogl_drawCurve1(point_t* beg, point_t* mid, point_t* end)
{
	/*curve calculations*/
	float a, b, c;
	a = ((end->y - mid->y) * (mid->x - beg->x) - (mid->y - beg->y) * (end->x - mid->x))
		/ (dsquare(end->x, mid->x) * (mid->x - beg->x) - dsquare(mid->x, beg->x) * (end->x - mid->x));
	b = (mid->y - beg->y - a * dsquare(mid->x, beg->x)) / (mid->x - beg->x);
	c = beg->y - a * beg->x * beg->x - b * beg->x;
	printf("a=%f, b=%f, c=%f\n", a, b, c);

	/*draw connected line segment from vertices n and n+1, color it red*/
	glBegin(GL_LINE_STRIP);
	glColor3f(1.0f, 0.0f, 0.0f);
	/*draws the curve*/
	for (float x = beg->x; x <= end->x; x += 0.0001f) {
		float y = a * x * x + b * x + c;
		glVertex2f(x, y);
	}
	glEnd();
}

/*Used to model smooth curves that can be scaled indefinitely*/
void bezierPoint(point_t* p1, point_t* p2, point_t* p3, double t, point_t* p)
{
	/*Cubic Bezier curve*/
	double a1 = pow((1 - t), 3);
	double a2 = pow((1 - t), 2) * 3 * t;
	double a3 = 3 * t * t * (1 - t);
	double a4 = t * t * t;

	p->x = a1 * p1->x + a2 * p2->x + a3 * p2->x + a4 * p3->x;
	p->y = a1 * p1->y + a2 * p2->y + a3 * p2->y + a4 * p3->y;
}

void ogl_drawLine(point_t* line, int size)
{
	if (line == NULL || size == 0) {
		return;
	}

	glLineWidth(5);/*specify width of rasterized line*/
	glColor3f(1.0f, 0.0f, 0.0f); /*set to red*/
	/*begin line drawing*/
	glBegin(GL_LINE_STRIP);
	for (int i = 0; i < size; i++)
	{
		//D3("%f,%f,%f", line[i].x, line[i].y, line[i].z);
		glVertex3f(line[i].x, line[i].y, line[i].z); /*draw ith coords of line*/
	}
	glEnd();
	glFlush(); /*push all buffered operations to OpenGL so that they will all be executed, returns immediately and complete in finite time*/
}
