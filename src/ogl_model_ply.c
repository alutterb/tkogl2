#include <math.h>
#include "def.h"

/****This file handles the LOADING of .ply files. ogl_model.c handles drawing****/

#define PLY_ATTR_GET_INT(name, buf, value) \
	if (strncmp(name, buffer, strlen(name)) == 0) { \
		strcpy(buf, buf + strlen(name)); \
		sscanf(buffer, "%i", value); \
	}

static void calculateNormal(float* coord1, float* coord2, float* coord3, float* norm)
{
	/* calculate Vector1 and Vector2 */
	float va[3], vb[3], vr[3], val;
	va[0] = coord1[0] - coord2[0];
	va[1] = coord1[1] - coord2[1];
	va[2] = coord1[2] - coord2[2];

	vb[0] = coord1[0] - coord3[0];
	vb[1] = coord1[1] - coord3[1];
	vb[2] = coord1[2] - coord3[2];

	/* cross product */
	vr[0] = va[1] * vb[2] - vb[1] * va[2];
	vr[1] = vb[0] * va[2] - va[0] * vb[2];
	vr[2] = va[0] * vb[1] - vb[0] * va[1];

	/* normalization factor */
	val = sqrt(vr[0] * vr[0] + vr[1] * vr[1] + vr[2] * vr[2]);

	norm[0] = vr[0] / val;
	norm[1] = vr[1] / val;
	norm[2] = vr[2] / val;
}

/*sets all values to 0 and frees drawing memory*/
static void reset(model_t* model)
{
	model->max[0] = 0.0;
	model->max[1] = 0.0;
	model->max[2] = 0.0;

	model->min[0] = 0.0;
	model->min[1] = 0.0;
	model->min[2] = 0.0;

	model->dsMax[0] = 0.0;
	model->dsMax[1] = 0.0;
	model->dsMax[2] = 0.0;

	model->dsMin[0] = 0.0;
	model->dsMin[1] = 0.0;
	model->dsMin[2] = 0.0;

	model->delta[0] = 0.0;
	model->delta[1] = 0.0;
	model->delta[2] = 0.0;
	model->delta[3] = 0.0;

	model->count = 0;
	model->dsCount = 0;
	FREE(model->vertex);
	FREE(model->normal);
	FREE(model->color);
}

/*transform model coordinates and clamp if outside range*/
float ogl_calCoordinate(float value, int id, float* delta)
{
	if (delta[3] > 1.0) {
		return (value - delta[id]) / delta[3];
	}
	else
	{
		return (value - delta[id]);
	}

	return value;
}

/*normalize model coordinates*/
float calCoordinate1(float value, int id, float* delta)
{
	if (delta[3] > 1.0) {
		return value / delta[3];
	}
	return value;
}


#define POSITIVE(i) (i > 0.0) ? i : (i * -1.0)
/*loads SPECIFICALLY .ply files*/
void ogl_loadModel(const char* filename, model_t* model)
{
	char* pch = strstr(filename, ".ply"); /*file path*/
	if (!pch)
	{
		printf("File does not have a .PLY extension. ");
		return;
	}

	FILE* file = fopen(filename, "r"); /*read contents*/
	if (!file)
	{
		printf("%s can't be opened\n", filename);
		return;
	}

	fseek(file, 0, SEEK_END); /*move to end of file*/
	long size = ftell(file) * sizeof(float); /*size of file*/

	float* Vertex_Buffer = (float*)malloc(ftell(file)); /*alloc memory for vertex drawing buffer*/
	float* VertexColor_Buffer = (float*)malloc(ftell(file)); /*alloc memory for vertex color buffer*/

	if (Vertex_Buffer == NULL) return;

	reset(model); /**/

	/*alloc memory for vertices and normals*/
	model->vertex = (float*)malloc(size);
	model->normal = (float*)malloc(size);

	/*variable definitions*/
	int i = 0;
	int temp = 0;
	int quads_index = 0;
	int triangle_index = 0;
	int normal_index = 0;
	char buffer[1000];

	int vertexNum = 0;
	int faceNum = 0;
	fseek(file, 0, SEEK_SET); /*start at beginning of file*/
	/*reads info up to end_header in .ply file*/
	while (strncmp("end_header", buffer, strlen("end_header")) != 0) {
		fgets(buffer, 300, file);
		PLY_ATTR_GET_INT("element vertex", buffer, &vertexNum);
		PLY_ATTR_GET_INT("element face", buffer, &faceNum);
		if (strncmp("property uchar red", buffer, strlen("property uchar red")) == 0) {
			model->color = (float*)malloc(size);
		}
	}
	printf("Load PLY model: vertex=%d, face=%d, colored=%s\n", vertexNum, faceNum, model->color != NULL ? "yes" : "no");

	/*Read vertices*/
	i = 0;
	for (int iterator = 0; iterator < vertexNum; iterator++)
	{
		fgets(buffer, 300, file);
		/*if color specified, read that in alongisde vertex loc, if not, just get vertex*/
		if (model->color) {
			sscanf(buffer, "%f %f %f %f %f %f", &Vertex_Buffer[i], &Vertex_Buffer[i + 1], &Vertex_Buffer[i + 2],
				&VertexColor_Buffer[i], &VertexColor_Buffer[i + 1], &VertexColor_Buffer[i + 2]);
		}
		else {
			sscanf(buffer, "%f %f %f", &Vertex_Buffer[i], &Vertex_Buffer[i + 1], &Vertex_Buffer[i + 2]);
		}

		/*checks for outlier vertices and sets to max/min values \*/
		for (int j = 0; j < 3; j++)
		{
			if (Vertex_Buffer[i + j] < model->min[j])
			{
				model->min[j] = Vertex_Buffer[i + j];
			}

			if (Vertex_Buffer[i + j] > model->max[j])
			{
				model->max[j] = Vertex_Buffer[i + j];
			}
		}
		i += 3;
	}

	for (int j = 0; j < 3; j++)
	{
		/*algorithm to search for maximum delta, store in delta[3]*/
		if (model->max[j] - model->min[j] != 0) { /*if min and max are distinct*/
			model->delta[j] = (model->max[j] + model->min[j]) / 2; /*midpoint*/
			printf("%f %f\n", POSITIVE(model->delta[j]), model->delta[3]);
			float tmp = POSITIVE(model->delta[j]) + model->max[j];
			if (tmp > model->delta[3])
			{
				model->delta[3] = tmp;
			}
		}
		printf("min=%f, max=%f, delta[j]=%f\n", model->min[j], model->max[j], model->delta[j]);
	}
	printf("max delta is %f\n", model->delta[3]);

	// read faces, deals in clusters of 9
	i = 0;
	for (int iterator = 0; iterator < faceNum; iterator++)
	{
		fgets(buffer, 300, file);
		/*if we start on face values*/
		if (buffer[0] == '3')
		{
			int vertex[3] = { 0 }; /*initialize vertices for triangle*/
			buffer[0] = ' ';
			sscanf(buffer, "%i%i%i", &vertex[0], &vertex[1], &vertex[2]); /*read in the data*/

			float coord[3][3] = { 0 };
			for (int i = 0; i < 9; i++)
			{
				//model->vertex[triangle_index + i] = Vertex_Buffer[3 * vertex[i / 3] + (i % 3)];
				//if (model->max[0] > 1.0 || model->max[1] > 1.0
				//	|| model->max[2] > 1.0 || model->min[0] < -1.0
				//	|| model->min[1] < -1.0 || model->min[2] < -1.0) {
				//	model->vertex[triangle_index + i] = (Vertex_Buffer[3 * vertex[i / 3] + (i % 3)] - delta[i % 3]) / 33.0;
				//}
				model->vertex[triangle_index + i] = ogl_calCoordinate(Vertex_Buffer[3 * vertex[i / 3] + (i % 3)], i % 3, model->delta); /*sets ith triangle vertex*/

				/*set ith triangle color*/
				if (model->color) {
					model->color[triangle_index / 9 * 12 + i / 3 * 4 + (i % 3)] = VertexColor_Buffer[3 * vertex[i / 3] + (i % 3)] / 255.0f;
					if (i % 3 == 2) model->color[triangle_index / 9 * 12 + i / 3 * 4 + 3] = 0.9f;
				}
				coord[i / 3][i % 3] = model->vertex[triangle_index + i];
			}

			// norm for faces (gives direction)
			float norm[3];
			calculateNormal(coord[0], coord[1], coord[2], norm);
			for (int i = 0; i < 9; i++)
			{
				model->normal[normal_index + i] = norm[i % 3];
			}

			/*9 faces, 9 triangles*/
			normal_index += 9;
			triangle_index += 9;
			model->count += 3;
		}
		i += 3;
	}
	fclose(file);
	// finalize max and min vertex
	for (int i = 0; i < 3; i++)
	{
		model->max[i] = ogl_calCoordinate(model->max[i], i, model->delta);
		model->min[i] = ogl_calCoordinate(model->min[i], i, model->delta);
		printf("min=%f, max=%f\n", model->min[i], model->max[i]);
	}

	free(Vertex_Buffer);
	free(VertexColor_Buffer);
	Tcl_ValidateAllMemory(__FILE__, __LINE__);
}

#define BEG_LING(tag) \
	char* p = NULL; \
	do { \
		p = fgets(buffer, 300, file); \
	} while (p != NULL && !feof(file) && strncmp(tag, buffer, strlen(tag)) != 0)

int ogl_loadDownSampleModel(FILE* file, model_t* model)
{
	char buffer[1000] = { 0 };

	BEG_LING("Surface=");/*set filepointer to tag*/
	PLY_ATTR_GET_INT("Surface=", buffer, &model->dsCount); /*read in dsCount value*/
	printf("Load down sample model: vertex=%d\n", model->dsCount);

	if (model->dsCount <= 0)
	{
		return 0;
	}

	FREE(model->dsVertex);
	model->dsVertex = (float*)malloc(model->dsCount * 3 * sizeof(float));

	 

	/*Read Vertices*/
	for (int i = 0; i < model->dsCount * 3; i += 3)
	{
		fgets(buffer, 300, file);
		sscanf(buffer, "%f %f %f", &model->dsVertex[i], &model->dsVertex[i + 1], &model->dsVertex[i + 2]); /*read in downsampled vertices*/

		if (i >= 3 * 5)
		{
			/*handle outliers and grab coords*/
			if (model->dsVertex[i] < model->dsMin[X]) model->dsMin[X] = model->dsVertex[i];
			if (model->dsVertex[i + Y] < model->dsMin[Y]) model->dsMin[Y] = model->dsVertex[i + Y];
			if (model->dsVertex[i + Z] < model->dsMin[Z]) model->dsMin[Z] = model->dsVertex[i + Z];

			if (model->dsVertex[i] > model->dsMax[X]) model->dsMax[X] = model->dsVertex[i];
			if (model->dsVertex[i + Y] > model->dsMax[Y]) model->dsMax[Y] = model->dsVertex[i + Y];
			if (model->dsVertex[i + Z] > model->dsMax[Z]) model->dsMax[Z] = model->dsVertex[i + Z];
			model->dsVertex[i] = ogl_calCoordinate(model->dsVertex[i], 0, model->delta);// +0.11;
			model->dsVertex[i + 1] = ogl_calCoordinate(model->dsVertex[i + 1], 1, model->delta);
			model->dsVertex[i + 2] = ogl_calCoordinate(model->dsVertex[i + 2], 2, model->delta);
		}
		else
		{
			/*if no outliers, grab coords*/
			model->dsVertex[i] = ogl_calCoordinate(model->dsVertex[i], 0, model->delta);// +0.11;
			model->dsVertex[i + 1] = ogl_calCoordinate(model->dsVertex[i + 1], 1, model->delta);// +1.27;
			model->dsVertex[i + 2] = ogl_calCoordinate(model->dsVertex[i + 2], 2, model->delta);// -0.02;
		}
	}
	double delta[4] = { 0.0 };
	for (int j = 0; j < 3; j++)
	{
		if (model->dsMax[j] - model->dsMin[j] != 0) { // distinct max/min
			delta[j] = (model->dsMax[j] + model->dsMin[j]) / 2; /*midpoint*/
			float tmp = POSITIVE(delta[j]) + model->dsMax[j]; //go beyond delta[j] of max
			if (tmp > delta[3]) //if our jump exceeds maximum threshold
			{
				delta[3] = tmp; //set our new threshold to that jump
			}
		}
		printf("min=%f, max=%f, delta[j]=%f\n", model->dsMin[j], model->dsMax[j], delta[j]);
	}
	for (int i = 15; i < model->dsCount * 3; i += 3)
	{
		//model->dsVertex[i] = calCoordinate(model->dsVertex[i], 0, delta);
		//model->dsVertex[i + 1] = calCoordinate(model->dsVertex[i + 1], 1, delta);
		//model->dsVertex[i + 2] = calCoordinate(model->dsVertex[i + 2], 2, delta);
	}

	Tcl_ValidateAllMemory(__FILE__, __LINE__);
	return model->dsCount;

	 
}

void ogl_loadLandmark(FILE* file, model_t* models, int model_size)
{
	char buffer[1000] = { 0 };
	int lmkNum = 0;
	BEG_LING("LM3="); /*start at LM3*/
	PLY_ATTR_GET_INT("LM3=", buffer, &lmkNum);
	printf("Load landmark: number=%d\n", lmkNum);

	if (lmkNum <= 0)
	{
		return;
	}

	/*read vertices*/
	for (int i = 0; i < lmkNum; i++)
	{
		point_t p;
		fgets(buffer, 300, file);
		sscanf(buffer, "%lf %lf %lf", &p.x, &p.y, &p.z);/*read x,y,z coords of point*/
		D3("raw dot is %f, %f, %f", p.x, p.y, p.z); /*prints info*/

		for (int j = 0; j < model_size; j++)
		{
			/*calculate point coords*/
			p.x = ogl_calCoordinate(p.x, 0, models[j].delta);
			p.y = ogl_calCoordinate(p.y, 1, models[j].delta);
			p.z = ogl_calCoordinate(p.z, 2, models[j].delta);

			if (!valiadteDot(&p))
			{
				continue;
			}

			/*assign color and draw*/
			D3("add dot %f, %f, %f", p.x, p.y, p.z);
			color_t c = { -1.0, -1.0, -1.0 };
			dot_slice_index(j);
			dot_add(&p, &c);
		}
	}
}

/*assigns null terminator after whitespace*/
void rtrim(char* buf)
{
	char* back = buf + strlen(buf);
	while (isspace(*--back));
	*(back + 1) = '\0';
}

int ogl_loadDgtModel(const char* filename, model_t* model)
{
	FILE* file = fopen(filename, "r");
	if (!file)
	{
		printf("%s can't be opened\n", filename);
	}

	char buffer[1000] = { 0 };
	int amount = 0;
	fseek(file, 0, SEEK_SET);
	/*count IDs*/
	while (!feof(file)) {
		BEG_LING("ID=");
		if (!feof(file))
			amount++;
	}

	if (amount == 0) {
		printf("Cannot find valid information in dgt file\n");
		fclose(file);
		return 0;
	}
	/*if nothing was read, free everything*/
#if 0
	if (model != NULL)
	{
		ckfree((char*)* model);
		model = NULL;
	}

	model = (model_t)ckalloc(sizeof(model_t));
	memset(model, 0, sizeof(model_t));

	/*error checking*/
#endif

	fseek(file, 0, SEEK_SET);
	//for (int i = 0; i < amount; i++)
	//{
	BEG_LING("ID=");
	if (feof(file)) printf("End of file");
	//rtrim(buffer);
	//D1("Load ply: %s", buffer + strlen("ID="));
	//ogl_loadModel(buffer + strlen("ID="), &(*models)[i]);
	if (ogl_loadDownSampleModel(file, &model) <= 0)
	{
		printf("Nothing to load");
		return 0;
	}
	//}

	//fseek(file, 0, SEEK_SET);
	//dot_slice_amount(amount);
	//ogl_loadLandmark(file, *models, amount);

	fclose(file);
}

int ogl_loadLandMark(const char* filename, model_t* models, int model_id, int amount)
{
	char format[64] = { 0 };
	/*read in different file types*/
	if (strstr(filename, ".pts"))
	{
		strcpy(format, "%*s %f %f %f");
	}
	else if (strstr(filename, ".csv"))
	{
		strcpy(format, "%*[^,],%f,%f,%f");
	}
	else
	{
		D1("%s is not a supported format\n", filename);
		return 0;
	}

	FILE* file = fopen(filename, "r");
	if (!file)
	{
		D1("%s can't be opened\n", filename);
		return 0;
	}

	D2("Start to load %d landmark from %s\n", amount, filename);
	int added = 0;
	char buffer[1000] = { 0 };
	fseek(file, 0, SEEK_SET);
	float x, y, z;
	for (int i = 0; i < amount; )
	{
		if (feof(file)) break;

		fgets(buffer, 300, file);
		int n = sscanf(buffer, format, &x, &y, &z); /*read in x,y,z*/
		if (n == 3)
		{
			/*calculate point coords*/
			point_t p;
			p.x = ogl_calCoordinate(x, 0, models[model_id].delta);
			p.y = ogl_calCoordinate(y, 1, models[model_id].delta);
			p.z = ogl_calCoordinate(z, 2, models[model_id].delta);

			if (!valiadteDot(&p))
			{
				continue;
			}

			D3("add dot %f, %f, %f", p.x, p.y, p.z);
			/*color landmark pt and add it*/
			color_t c = { -1.0, -1.0, -1.0 };
			dot_slice_index(model_id);
			dot_add(&p, &c);
			i++;
		}
	}
	fclose(file);
	return added;
}


