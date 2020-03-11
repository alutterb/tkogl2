#include "def.h"
/*called in setWindowId*/
void ogl_init()
{
	glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);  // Display Mode
	glEnable(GL_BLEND); /*Blend the computed fragment color values with the values in the color buffers*/
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);/*defines the operation of blending for all draw buffers when it is enabled*/
	glClearDepth(1.0f); /*clear value for depth buffer*/
	glEnable(GL_DEPTH_TEST); /*do depth comparisons and update depth buffer*/
	glEnable(GL_LINE_SMOOTH); /*draw lines with correct filtering, otherwise draw aliased lines*/
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST); /*indicates sampling quality of antialiased(smoothing jagged edges on curved lines and diagonals) lines, as well as more pixel fragments being generated during rasterization*/
	glDepthFunc(GL_LEQUAL); /*depth buffer comparisons, passes if incoming depth value is less than or equal to stored depth value*/

	//glViewport(0, 0, w, h);
	glMatrixMode(GL_PROJECTION); /*specifies which of the viewing pipeline matrices is to be identified, we want projection transformation stack here*/
	//gluPerspective(45.0, (double)w / (double)h, 1.0, 200.0);
	gluLookAt(0, 0, 1, 0, 0, 0, 0, 1, 0); /*defines a viewing transform for camera*/
	glMatrixMode(GL_MODELVIEW); /*now apply matrix operations to modelview stack*/
	glLoadIdentity(); /*replaces current matrix with identity matrix*/
	glClearColor(1.0, 1.0, 1.0, 0.0); /*clear values for color buffers*/
}

void ogl_enableLight()
{
	/*place light source*/
	GLfloat light_position0[] = { 0.0, 1.0, 0.0, 0.0 };
	glLightfv(GL_LIGHT0, GL_POSITION, light_position0); /*sets light source parameters: the light, the parameter we want to change, and then the parameter values*/
	GLfloat light_position1[] = { 1.0, 0.0, 0.0, 0.0 };
	glLightfv(GL_LIGHT1, GL_POSITION, light_position1);
	GLfloat light_position2[] = { -1.0, 0.0, 0.0, 0.0 };
	glLightfv(GL_LIGHT2, GL_POSITION, light_position2);
	GLfloat light_position3[] = { 0.0, 0.0, 1.0, 0.0 };
	glLightfv(GL_LIGHT3, GL_POSITION, light_position3);
	GLfloat light_position4[] = { 0.0, 0.0, -1.0, 0.0 };
	glLightfv(GL_LIGHT4, GL_POSITION, light_position4);

	/*set diffuse and specular light colors*/
	GLfloat lightColor0[] = { 0.4f, 0.4f, 0.4f, 1.0f };
	glLightfv(GL_LIGHT0, GL_SPECULAR, lightColor0);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lightColor0);
	glLightfv(GL_LIGHT1, GL_SPECULAR, lightColor0);
	glLightfv(GL_LIGHT1, GL_DIFFUSE, lightColor0);
	glLightfv(GL_LIGHT2, GL_SPECULAR, lightColor0);
	glLightfv(GL_LIGHT2, GL_DIFFUSE, lightColor0);
	glLightfv(GL_LIGHT3, GL_SPECULAR, lightColor0);
	glLightfv(GL_LIGHT3, GL_DIFFUSE, lightColor0);
	glLightfv(GL_LIGHT4, GL_SPECULAR, lightColor0);
	glLightfv(GL_LIGHT4, GL_DIFFUSE, lightColor0);

	/*turn on lights and depth*/
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	glEnable(GL_LIGHT2);
	glEnable(GL_LIGHT3);
	glEnable(GL_LIGHT4);
	glEnable(GL_DEPTH_TEST);

	glShadeModel(GL_SMOOTH); /*choosing shading technique, in this case, smooth causes the computed colors of the vertices to be interpolated as the primitive is rasterized.*/
	glLightModeli(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); /*sets light MODEL parameters*/
	glEnable(GL_COLOR_MATERIAL); /*color driven materials enabled*/
}

void ogl_disableLight()
{
	glDisable(GL_LIGHTING);
}


void ogl_getObjCoordinate(int x, int y, GLdouble* posX, GLdouble* posY, GLdouble* posZ, char* buf)
{
	/*stores data*/
	GLint viewport[4];
	GLdouble modelview[16];
	GLdouble projection[16];
	GLfloat winX, winY, winZ;

	/*grabs values for respective openGL matrices*/
	glGetDoublev(GL_MODELVIEW_MATRIX, modelview); /*returns value of selected parameter(s) and places it in modelview*/
	glGetDoublev(GL_PROJECTION_MATRIX, projection);
	glGetIntegerv(GL_VIEWPORT, viewport);

	winX = (float)x;
	winY = (float)viewport[3] - (float)y;
	/*glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *data) here, x and y represent window coords of first pixel read from frame buffer*/
	glReadPixels(x, (int)winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ); /*reads a block of pixels from framebuffer, handling depth*/
	/*GLint gluUnProject(GLdouble winX, glDouble winY, glDouble winZ, const GLdouble * model, const GLdouble * proj, const GLint * view, GLdouble *objX, GLdouble *objY, GLdouble *objZ)*/
	gluUnProject(winX, winY, winZ, modelview, projection, viewport, posX, posY, posZ); /*map window coordinates to object coordinates*/
	if (buf != NULL)
		sprintf(buf, "%f %f %f %f %f %f", winX, winY, winZ, *posX, *posY, *posZ);
}

void ogl_getWndCoordinate(GLdouble posX, GLdouble posY, GLdouble posZ, GLdouble* winX, GLdouble* winY, GLdouble* winZ)
{
	GLint viewport[4];
	GLdouble modelview[16];
	GLdouble projection[16];

	glGetDoublev(GL_MODELVIEW_MATRIX, modelview);
	glGetDoublev(GL_PROJECTION_MATRIX, projection);
	glGetIntegerv(GL_VIEWPORT, viewport);

	gluProject(posX, posY, posZ, modelview, projection, viewport, winX, winY, winZ); /*map object coordinates to window coordinates*/
}


