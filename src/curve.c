#include "def.h"

static curve_t** curves = NULL; /*stores each individual curve with an associated id*/
static int curve_slice_id = 0;
static int slice_amount = 0;

inline float absf(float a, float b)
{
	return a > b ? a - b : b - a;
}

int curve_buildLine(point_t* p1, point_t* p2, point_t** line)
{
	float precision = 0.001;
	if (absf(p2->x, p1->x) < precision) /*points are already close together, skip incrementation process*/
	{
		/*Build the line*/
		if (*line != NULL) ckfree((char*)* line); /*if memory already allocated, free it up*/
		*line = (point_t*)ckalloc(sizeof(point_t) * 2); /*allocate memory for line of size 2*/
		(*line)[0].x = p1->x;
		(*line)[0].y = p1->x;
		(*line)[0].z = getRealZ(p1->x, p1->y, p1->z);
		(*line)[1].x = p2->x;
		(*line)[1].y = p2->x;
		(*line)[1].z = getRealZ(p2->x, p2->y, p2->z);
		return 2;
	}
	else
	{
		if (*line != NULL) ckfree((char*)* line);
		int size = (int)(absf(p2->x, p1->x) / precision);
		//D1("size of line is %d", size);
		*line = (point_t*)ckalloc(sizeof(point_t) * size);

		float a = (p2->y - p1->y) / (p2->x - p1->x); /*slope*/
		float b = p1->y - a * p1->x; /*intercept*/
		int i = 0;

		float beg, end;
		/*starting points to build line from pt 1 to pt 2 or vice versa*/
		if (p2->x > p1->x)
		{
			beg = p1->x;
			end = p2->x;
		}
		else
		{
			beg = p2->x;
			end = p1->x;
		}
		/*build line*/
		for (float x = beg; x <= end; x += precision)
		{
			float y = a * x + b;
			(*line)[i].x = x;
			(*line)[i].y = y;
			(*line)[i].z = getRealZ(x, y, p1->z);
			/*if both points live in 3D (with same sign), increment z coord */
			if (p1->z > 0.0 && p2->z > 0.0)
			{
				(*line)[i].z += precision;
			}
			else if (p1->z < 0.0 && p2->z < 0.0)
			{
				(*line)[i].z -= precision;
			}
			//D3("x=%f,y=%f,z=%f", (*line)[i].x, (*line)[i].y, (*line)[i].z);
			i++;
		}
		return size;
	}
}

curve_t* curve_create() /*initializes curve */
{
	curve_t* curve = (curve_t*)ckalloc(sizeof(curve_t));
	curve->points[0] = NULL;
	curve->points[1] = NULL;
	curve->points[2] = NULL;
	curve->pointNum = 0;
	curve->lines1 = NULL;
	curve->lines2 = NULL;
	curve->line1Size = 0;
	curve->line2Size = 0;
	curve->next = NULL;
	return curve;
}

int curve_addDot(int id, dot_t* d)
{
	point_t* p = &d->p; /*get the dot point*/
	if (curves[id] == NULL)
	{
		curves[id] = curve_create();
		curves[id]->points[curves[id]->pointNum] = p; /*first point is p*/
		curves[id]->pointNum++;
		return 1;
	}

	curve_t* c = curves[id];
	/*advance until next is null */
	while (c->next != NULL) c = c->next;

	/*now c is last node in the chain*/
	c->points[c->pointNum] = p;
	c->pointNum++;

	/*if maximum points stored, create another curve*/
	if (c->pointNum >= 3)
	{
		//c->line1Size = curve_buildLine(c->points[0], c->points[1], &c->lines1);
		//c->line2Size = curve_buildLine(c->points[2], c->points[1], &c->lines2);

		c->next = curve_create();
		return 0;
	}
	return 1;
}

/*grabs ith curve specified by id*/
curve_t* curve_get()
{
	return curves[curve_slice_id];
}

/*gets ith dot identified by pointNum*/
int curve_getDotId()
{
	return curves[curve_slice_id]->pointNum;
}

/*frees memory and resets curves to 0*/
void curve_slice_amount(int amount)
{
	if (curves != NULL)
	{
		for (int i = 0; i < slice_amount; i++)
		{
			curve_t* n = curves[i];
			while (n != NULL)
			{
				curve_t* t = n;
				n = n->next;
				ckfree((char*)t);
			}
		}
		ckfree((char*)curves);
		curves = NULL;
	}
	curve_slice_id = 0;
	slice_amount = amount;

	if (amount > 0)
	{
		curves = (curve_t * *)ckalloc(amount * sizeof(curve_t*));
		memset(curves, 0, amount * sizeof(curve_t*));
	}
}

/*sets curve_slice_id to specified id */
int curve_slice_index(int id)
{
	if (id < slice_amount)
	{
		curve_slice_id = id;
		return id;
	}
	return -1;
}
