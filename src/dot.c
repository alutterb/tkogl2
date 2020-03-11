#include "def.h"

static dot_t** dots = NULL; /*array storing dots, dot_slice_index indicates which dots belong to which specimen*/
static int dot_slice_id = 0;
static int slice_amount = 0;
static dot_t* selected = NULL;
static int selected_id = 0;

static dot_t** anchors = NULL;
static int anchor_slice_id = 0;
static int anchor_slice_amount = 0;
static dot_t* anchor_selected = NULL;
static int anchor_selected_id = 0;

FILE* fp;

int dot_size()
{
	/*no dots, size is 0*/
	if (dots == NULL)
	{
		return 0;
	}

	int size = 0;
	dot_t* n = dots[dot_slice_id];
	/*increment size and iterate through all dots starting at dot_slice_id*/
	while (n != NULL)
	{
		size++;
		n = n->next;
	}
	return size;
}

#define IS_IN_RANGE(target, test, dotRadius) (target - dotRadius) <= test && (target + dotRadius) >= test
int dot_select(point_t* p, float dotRadius)
{
	if (dots == NULL)
	{
		return -1;
	}

	int id = 0;
	dot_t* n = dots[dot_slice_id];
	/*iterate through all dots starting from dot_slice_id*/
	while (n != NULL)
	{
		id++;
		/*if ith dot is in range of chosen point, select that dot and store its id*/
		if (IS_IN_RANGE(n->p.x, p->x, dotRadius)
			&& IS_IN_RANGE(n->p.y, p->y, dotRadius)
			&& IS_IN_RANGE(n->p.z, p->z, dotRadius)) {
			selected = n;
			selected_id = id;
			return 0;
		}
		n = n->next;
	}
	return -1;
}

int dot_selected_id()
{
	return selected_id;
}

int dot_add(point_t* p, color_t* c)
{
	if (dots == NULL)
	{
		return 0;
	}
	dot_t* node = (dot_t*)ckalloc(sizeof(dot_t));
	/*construct coords and color of node dot*/
	node->p.x = p->x;
	node->p.y = p->y;
	node->p.z = p->z;
	node->c.r = c->r;
	node->c.g = c->g;
	node->c.b = c->b;
	node->next = NULL;
	node->type = LANDMARK;

	 

	/*if ith dot doesn't exist, assign to it to node*/
	if (dots[dot_slice_id] == NULL)
	{
		dots[dot_slice_id] = node;
	}
	/*iterate to end of dots, assign new entry to node*/
	else
	{
		dot_t* n = dots[dot_slice_id];
		while (n->next != NULL) n = n->next;
		n->next = node;
	}
	return dot_size();
}
/*assigns dot coords to arg p*/
int dot_move(point_t* p)
{
	if (selected != NULL)
	{
		selected->p.x = p->x;
		selected->p.y = p->y;
		selected->p.z = p->z;
	}
	return dot_size();
}

/*assigns dot color to arg c*/
int dot_color(color_t* c)
{
	if (selected != NULL)
	{
		selected->c.r = c->r;
		selected->c.g = c->g;
		selected->c.b = c->b;
	}
	return dot_size();
}

#define DOT_EQUAL(p1, p2) p1.x == p2->x && p1.y == p2->y && p1.z == p2->z

int dot_del(point_t* p)
{
	/*if no dots, return 0*/
	if (dots == NULL)
	{
		return 0;
	}
	/*if ith dot and chosen p don't exist, return 0*/
	if (dots[dot_slice_id] == NULL || p == NULL)
	{
		return 0;
	}
	/*if ith dot matches arg p coords, move to next dot and return*/
	if (DOT_EQUAL(dots[dot_slice_id]->p, p))
	{
		dots[dot_slice_id] = dots[dot_slice_id]->next;
		return dot_size();
	}

	dot_t* n = dots[dot_slice_id];
	while (n->next != NULL)
	{
		if (DOT_EQUAL(n->next->p, p))
		{
			dot_t* tmp = n->next;
			n->next = tmp->next;
			ckfree((char*)tmp);
			break;
		}
		n = n->next;
	}

	 

	return dot_size();
}

int dot_del_selected()
{
	dot_del(&selected->p);
	selected = NULL;
	return dot_size();
}

/*gets ith dot */
dot_t* dot_get(int id)
{

	if (dots == NULL)
	{
		return NULL;
	}
	if (id == -1)
		id = dot_slice_id;
	return dots[id];
}

/*grabs dot a different way, not sure why. maybe to protect from going out of array bounds?*/
dot_t* dot_get_dot(int id, int pid)
{
	dot_t* n = dot_get(id);
	while (pid > 1 && n != NULL)
	{
		pid--;
		n = n->next;
	}
	return n;
}

dot_t* dot_get_selected()
{
	return selected;
}

/*if present, deletes dots and sets to 0, allocates memory for dots*/
void dot_slice_amount(int amount)
{
	if (dots != NULL)
	{
		for (int i = 0; i < slice_amount; i++)
		{
			dot_t* n = dots[i];
			while (n != NULL)
			{
				dot_t* t = n;
				n = n->next;
				ckfree((char*)t);
			}
		}
		ckfree((char*)dots);
		dots = NULL;
	}

	dot_slice_id = 0;
	slice_amount = amount;
	curve_slice_amount(amount);

	if (amount > 0)
	{
		dots = (dot_t * *)ckalloc(amount * sizeof(dot_t*));
		memset(dots, 0, amount * sizeof(dot_t*));
	}

	 
}

void dots_free()
{
	if (dots != NULL)
	{
		for (int i = 0; i < slice_amount; i++)
		{
			dot_t* n = dots[i];
			while (n != NULL)
			{
				dot_t* t = n;
				n = n->next;
				ckfree((char*)t);
			}
		}
		ckfree((char*)dots);
		dots = NULL;
	}

	 
}

/*sets dot_slice_id and curve_slice_id to chosen id */
int dot_slice_index(int id)
{
	if (id < slice_amount)
	{
		dot_slice_id = id;

		curve_slice_index(id);
		return id;
	}
	return -1;
}

/********************************************************************************************************************************************************************************************/

int anchor_size()
{
	if (anchors == NULL)
		return 0;

	int size = 0;
	dot_t* n = anchors[anchor_slice_id];

	while (n != NULL)
	{
		size++;
		n = n->next;
	}

	return size;
}

#define IS_IN_RANGE(target, test, dotRadius) (target - dotRadius) <= test && (target + dotRadius) >= test
int anchor_select(point_t* p, float dotRadius)
{
	if (anchors == NULL)
		return -1;

	int id = 0;
	dot_t* n = anchors[anchor_slice_id];

	while (n != NULL)
	{
		id++;
		if (IS_IN_RANGE(n->p.x, p->x, dotRadius)
			&& IS_IN_RANGE(n->p.y, p->y, dotRadius)
			&& IS_IN_RANGE(n->p.z, p->z, dotRadius)) {
			anchor_selected = n;
			anchor_selected_id = id;
			return 0;
		}
		n = n->next;
	}
	return -1;
}

int anchors_selected_id()
{
	return anchor_selected_id;
}

int anchor_add(point_t* p, color_t* c)
{
	if (anchors == NULL)
	{
		return 0;
	}
	dot_t* node = (dot_t*)ckalloc(sizeof(dot_t));
	node->p.x = p->x;
	node->p.y = p->y;
	node->p.z = p->z;
	node->c.r = c->r;
	node->c.g = c->g;
	node->c.b = c->b;
	node->next = NULL;
	node->type = ANCHOR;

	 

	if (anchors[anchor_slice_id] == NULL)
	{
		anchors[anchor_slice_id] = node;
	}
	else
	{
		dot_t* n = anchors[anchor_slice_id];
		while (n->next != NULL) n = n->next;
		n->next = node;
	}
	return anchor_size();
}

dot_t* anchor_get(int id)
{
	if (anchors == NULL)
	{
		return NULL;
	}
	if (id == -1)
		id = anchor_slice_id;

	return anchors[id];
}

int anchor_move(point_t* p)
{
	if (anchor_selected != NULL)
	{
		anchor_selected->p.x = p->x;
		anchor_selected->p.y = p->y;
		anchor_selected->p.z = p->z;
	}
	return anchor_size();
}


int anchor_color(color_t* c)
{
	if (anchor_selected != NULL)
	{
		anchor_selected->c.r = c->r;
		anchor_selected->c.g = c->g;
		anchor_selected->c.b = c->b;
	}
	return anchor_size();
}

#define DOT_EQUAL(p1, p2) p1.x == p2->x && p1.y == p2->y && p1.z == p2->z

int anchor_del(point_t* p)
{
	if (anchors == NULL)
	{
		return 0;
	}

	if (anchors[anchor_slice_id] == NULL || p == NULL)
	{
		return 0;
	}

	if (DOT_EQUAL(anchors[anchor_slice_id]->p, p))
	{
		anchors[anchor_slice_id] = anchors[anchor_slice_id]->next;
		return anchor_size();
	}

	dot_t* n = anchors[anchor_slice_id];
	while (n->next != NULL)
	{
		if (DOT_EQUAL(n->next->p, p))
		{
			dot_t* tmp = n->next;
			n->next = tmp->next;
			ckfree((char*)tmp);
			break;
		}
		n = n->next;
	}

	 

	return anchor_size();
}

int anchor_del_selected()
{
	anchor_del(&anchor_selected->p);
	anchor_selected = NULL;
	return anchor_size();
}


dot_t* anchor_get_anchor(int id, int pid)
{
	dot_t* n = anchor_get(id);
	while (pid > 1 && n != NULL)
	{
		pid--;
		n = n->next;
	}
	return n;
}

dot_t* anchor_get_selected()
{
	return anchor_selected;
}

void anchors_slice_amount(int amount)
{
	if (anchors != NULL)
	{
		for (int i = 0; i < anchor_slice_amount; i++)
		{
			dot_t* n = anchors[i];
			while (n != NULL)
			{
				dot_t* t = n;
				n = n->next;
				ckfree((char*)t);
			}
		}
		ckfree((char*)anchors);
		anchors = NULL;
	}

	anchor_slice_id = 0;
	anchor_slice_amount = amount;
	curve_slice_amount(amount);

	if (amount > 0)
	{
		anchors = (dot_t * *)ckalloc(amount * sizeof(dot_t*));
		memset(anchors, 0, amount * sizeof(dot_t*));
	}

	 
}

void anchors_free()
{
	if (anchors != NULL)
	{
		for (int i = 0; i < anchor_slice_amount; i++)
		{
			dot_t* n = anchors[i];
			while (n != NULL)
			{
				dot_t* t = n;
				n = n->next;
				ckfree((char*)t);
			}
		}
		ckfree((char*)anchors);
		anchors = NULL;
	}

	 
}

int anchor_slice_index(int id)
{
	if (id < anchor_slice_amount)
	{
		anchor_slice_id = id;

		curve_slice_index(id);
		return id;
	}
	return -1;
}

