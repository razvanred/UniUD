#include "visualt/visualt.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

typedef struct CharMap CharMap;

typedef struct Obj Obj;

typedef struct BoolMap {
	bool *chars;
	unsigned int width, height;
} BoolMap;

//----INTERNALs----
#define vtPuts(STRING) fputs((STRING), stdout)

#define vt32Puts(UINT32, LENGTH) fwrite((UINT32), sizeof(uint32_t), (LENGTH), stdout);

#define vt32Putchar(UINT32) putchar((UINT32)>>0&0xffu);   \
                            putchar((UINT32)>>8&0xffu);   \
                            putchar((UINT32)>>16&0xffu);  \
                            putchar((UINT32)>>24&0xffu)

#define vtInitializeObj(OBJ)  (OBJ)->x = 0;           \
                              (OBJ)->y = 0;           \
                              (OBJ)->visible = true;  \
                              (OBJ)->penSize = 0;     \
                              (OBJ)->penChar = '#';   \
                              (OBJ)->currentSprite = (OBJ)->sprites

#define vtInitializeCharMap(CHARMAP, WIDTH, HEIGHT) (CHARMAP)->width = (WIDTH);   \
                                                    (CHARMAP)->height = (HEIGHT); \
                                                    (CHARMAP)->chars = malloc(vtSizeofChars(CHARMAP))

#define vtFreeCharMap(CHARMAP) free((CHARMAP)->chars)

#define vt32EndiannessSwap(UINT32) ((((UINT32)>>24u)&0xffu)|(((UINT32)>>8u)&0xff00u)|(((UINT32)<<8u)&0xff0000u)|(((UINT32)<<24u)&0xff000000u))

#define vtSizeofChars(CHARMAP) ((CHARMAP)->width*(CHARMAP)->height*sizeof(uint32_t))

#define vtClearCharMap(CHARMAP) memset((CHARMAP)->chars, 0, vtSizeofChars((CHARMAP)))

#define vtNormalizePosition(CHARMAP, SPRITE, SPRITEX, SPRITEY, X, Y)  (X) = (int)((CHARMAP)->width/2-(SPRITE)->width/2+(SPRITEX));  \
                                                                      (Y) = (int)((CHARMAP)->height/2-(SPRITE)->height/2-(SPRITEY))

static void vtInitializeStringCharMap(CharMap *const charMap, const uint8_t *const utf8Text) {
	unsigned int charMapWidth = 1, charMapHeight = 1;
	uint8_t *t;

	//count size
	for(unsigned int width = 0, i = 0; utf8Text[i] != '\0'; i++) {
		if((utf8Text[i]&0xc0u) != 0x80u) { // partial code point: 10xx xxxx
			if(utf8Text[i] == '\n') {
				charMapHeight++;
				width = 0;
			} else {
				width++;
				if(width > charMapWidth) {
					charMapWidth = width;
				}
			}
		}
	}
	vtInitializeCharMap(charMap, charMapWidth, charMapHeight);
	//parse
	for(unsigned int i = 0, y = 0; y < charMapHeight; i++, y++) {
		for(unsigned int x = 0; x < charMapWidth; x++) {
			charMap->chars[x+y*charMapWidth] = 0;
			t = (uint8_t *)&charMap->chars[x+y*charMapWidth];
			if(utf8Text[i] >= 0xf0) {         //4B code point: 1111 0xxx
				t[0] = utf8Text[i++];
				t[1] = utf8Text[i++];
				t[2] = utf8Text[i++];
				t[3] = utf8Text[i++];
			} else if(utf8Text[i] >= 0xe0) {  //3B code point: 1110 xxxx
				t[0] = utf8Text[i++];
				t[1] = utf8Text[i++];
				t[2] = utf8Text[i++];
			} else if(utf8Text[i] >= 0xc0) {  //2B code point: 110x xxxx
				t[0] = utf8Text[i++];
				t[1] = utf8Text[i++];
			} else {                          //1B code point: 0xxx xxxx
				if(utf8Text[i] != '\n' && utf8Text[i] != '\0') {
					if(utf8Text[i] != '\v') {
						t[0] = utf8Text[i++];
					} else {
						t[0] = utf8Text[i++] && 0;
					}
				} else {
					for(; x < charMapWidth; x++) {
						charMap->chars[x+y*charMapWidth] = 0;
					}
				}
			}
		}
	}
}

#ifdef VISUALT_UNBUFFERED_PRINT

static void vtPrintCharMap(const CharMap *const charMap, const bool border) {
	const unsigned int width = charMap->width, height = charMap->height;
	const uint32_t *const chars = charMap->chars;

	if(border) {
		vtPuts("┌");
		for(unsigned int x = 0; x < width-1; x++) {
			vtPuts("─");
		}
		vtPuts("─┐\n");
		for(unsigned int y = 0; y < width*height; y += width) {
			vtPuts("│");
			vt32Puts(&chars[0+y], width);
			vtPuts("│\n");
		}
		vtPuts("└");
		for(unsigned int x = 0; x < width-1; x++) {
			vtPuts("─");
		}
		vtPuts("─┘\n");
	} else {
		for(unsigned int y = 0; y < width*height; y += width) {
			vtPuts("│");
			vt32Puts(&chars[0+y], width);
			vtPuts("│\n");
		}
	}
}

#else

static void vtPrintCharMap(const CharMap *const charMap, const bool border) {
	const unsigned int width = charMap->width, height = charMap->height;
	const uint32_t *const chars = charMap->chars;
	const uint8_t *t;
	uint8_t *const buffer = malloc(sizeof(uint8_t)+width*sizeof(uint32_t));

	if(border) {
		vtPuts("┌");
		for(unsigned int x = 0; x < width-1; x++) {
			vtPuts("─");
		}
		vtPuts("─┐\n");
		for(unsigned y = 0; y < height; y++) {
			for(unsigned int i = 0, x = 0; x < width; x++) {
				t = (uint8_t *)&chars[x+y*width];
				if(t[0] >= 0xf0) {  //4B code point: 1111 0xxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
					buffer[i++] = t[2];
					buffer[i++] = t[3];
				} else if(t[0] >= 0xe0) { //3B code point: 1110 xxxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
					buffer[i++] = t[2];
				} else if(t[0] >= 0xc0) { //2B code point: 110x xxxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
				} else {  //1B code point: 0xxx xxxx
					if(t[0] != 0) {
						buffer[i++] = t[0];
					} else {
						buffer[i++] = ' ';
					}
				}
				buffer[i] = '\0';
			}
			vtPuts("│");
			vtPuts((char *)buffer);
			vtPuts("│\n");
		}
		vtPuts("└");
		for(unsigned int x = 0; x < width-1; x++) {
			vtPuts("─");
		}
		vtPuts("─┘\n");
	} else {
		for(unsigned int y = 0; y < height; y++) {
			for(unsigned int i = 0, x = 0; x < width; x++) {
				t = (uint8_t *)&chars[x+y*width];
				if(t[0] >= 0xf0) {  //4B code point: 1111 0xxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
					buffer[i++] = t[2];
					buffer[i++] = t[3];
				} else if(t[0] >= 0xe0) { //3B code point: 1110 xxxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
					buffer[i++] = t[2];
				} else if(t[0] >= 0xc0) { //2B code point: 110x xxxx
					buffer[i++] = t[0];
					buffer[i++] = t[1];
				} else {  //1B code point: 0xxx xxxx
					buffer[i++] = t[0];
				}
				buffer[i] = '\0';
			}
			puts((char *)buffer);
		}
	}
	free(buffer);
}

#endif

static unsigned int vtPrintStringCharMap(const CharMap *const charMap, const bool border, uint8_t **const utf8String) {
	const unsigned int width = charMap->width, height = charMap->height;
	const uint32_t *const chars = charMap->chars;
	const uint8_t *t;
	unsigned int length = 0, i = 0;

	for(unsigned int y = 0; y < height; length++, y++) {
		for(unsigned int x = 0; x < width; x++) {
			t = (uint8_t *)&chars[x+y*width];
			if(t[0] >= 0xf0) {        //4B code point: 1111 0xxx
				length += 4;
			} else if(t[0] >= 0xe0) { //3B code point: 1110 xxxx
				length += 3;
			} else if(t[0] >= 0xc0) { //2B code point: 110x xxxx
				length += 2;
			} else {               //1B code point: 0xxx xxxx
				length += 1;
			}
		}
	}
	if(border) {
		length += (3*((width+2)*2+(2*height)) //first and last row + │ for every line
				+2 //first \n and last \0
		)*sizeof(uint8_t);
	}

	*utf8String = malloc(length*sizeof(uint8_t));
	uint8_t *const string = *utf8String;

	if(border) {
		i = length-2;
		string[i--] = 0x98u;  //┘
		string[i--] = 0x94u;  //┘
		string[i--] = 0xe2u;  //┘
		for(unsigned int j = 0; j < width; j++) {
			string[i--] = 0x80u;  //─
			string[i--] = 0x94u;  //─
			string[i--] = 0xe2u;  //─
		}
		string[i--] = 0x94u;  //└
		string[i--] = 0x94u;  //└
		string[i] = 0xe2u;    //└
		i = 0;
		string[i++] = 0xe2u;  //┌
		string[i++] = 0x94u;  //┌
		string[i++] = 0x8cu;  //┌
		for(unsigned int j = 0; j < width; j++) {
			string[i++] = 0xe2u;  //─
			string[i++] = 0x94u;  //─
			string[i++] = 0x80u;  //─
		}
		string[i++] = 0xe2u;  //┐
		string[i++] = 0x94u;  //┐
		string[i++] = 0x90u;  //┐
		string[i++] = '\n';
	}
	for(unsigned int y = 0; y < height; y++) {
		if(border) {
			string[i++] = 0xe2u;  //│
			string[i++] = 0x94u;  //│
			string[i++] = 0x82u;  //│
		}
		for(unsigned int x = 0; x < width; x++) {
			t = (uint8_t *)&chars[x+y*width]; // t = chars[x+y*width];
			if(t[0] >= 0xf0) { //4B code point: 1111 0xxx
				string[i++] = t[0];
				string[i++] = t[1];
				string[i++] = t[2];
				string[i++] = t[3];
			} else if(t[0] >= 0xe0) { //3B code point: 1110 xxxx
				string[i++] = t[0];
				string[i++] = t[1];
				string[i++] = t[2];
			} else if(t[0] >= 0xc0) { //2B code point: 110x xxxx
				string[i++] = t[0];
				string[i++] = t[1];
			} else { //1B code point: 0xxx xxxx
				if(t[0] != 0) {
					string[i++] = t[0];
				} else {
					string[i++] = ' ';
				}
			}
		}
		if(border) {
			string[i++] = 0xe2u;  //│
			string[i++] = 0x94u;  //│
			string[i++] = 0x82u;  //│
		}
		string[i++] = '\n';
	}
	string[length-1] = '\0';
	return length;
}

static void vtStamp(const CharMap *const charMap, const CharMap *const sprite, int spriteX, int spriteY) {
	vtNormalizePosition(charMap, sprite, spriteX, spriteY, spriteX, spriteY);
	for(unsigned int y = 0; y < sprite->height; y++) {
		for(unsigned int x = 0; x < sprite->width; x++) {
			if(sprite->chars[x+y*sprite->width] && spriteX+(int)x >= 0 && spriteX+(int)x < (int)charMap->width && spriteY+(int)y >= 0 && spriteY+(int)y < (int)charMap->height) {
				charMap->chars[(spriteX+x)+(spriteY+y)*charMap->width] = sprite->chars[x+y*sprite->width];
			}
		}
	}
}

static void vtMask(const BoolMap *const boolMap, const CharMap *const sprite, int spriteX, int spriteY) {
	vtNormalizePosition(boolMap, sprite, spriteX, spriteY, spriteX, spriteY);
	for(unsigned int y = 0; y < sprite->height; y++) {
		for(unsigned int x = 0; x < sprite->width; x++) {
			if(sprite->chars[x+y*sprite->width] && spriteX+(int)x >= 0 && spriteX+(int)x < (int)boolMap->width && spriteY+(int)y >= 0 && spriteY+(int)y < (int)boolMap->height) {
				boolMap->chars[(spriteX+x)+(spriteY+y)*boolMap->width] = true;
			}
		}
	}
}

static void vtRender(const CharMap *const charMap, const unsigned int objLength, const Obj *const *const objs, bool clearCharmap) {
	if(clearCharmap) {
		vtClearCharMap(charMap);
	}
	for(unsigned int i = 0; i < objLength; i++) {
		if(objs[i]->visible) {
			vtStamp(charMap, objs[i]->currentSprite, objs[i]->x, objs[i]->y);
		}
	}
}

static void vtLine(const CharMap *const canvas, const unsigned short penSize, const uint32_t penChar, int x0, int y0, const int x1, const int y1) {
	CharMap stroke;
	switch(penSize) {
		default:
		case 0:
			stroke = (CharMap){.width=1, .height=1, .chars=(uint32_t[1*1]){penChar}};
			break;
		case 1:
			stroke = (CharMap){.width=2, .height=2, .chars=(uint32_t[2*2]){penChar, penChar, penChar, penChar}};
			break;
		case 2:
			stroke = (CharMap){.width=4, .height=3, .chars=(uint32_t[4*3]){0, penChar, penChar, 0, penChar, penChar, penChar, penChar, 0, penChar, penChar, 0}};
			break;
		case 3:
			stroke = (CharMap){.width=5, .height=3, .chars=(uint32_t[5*3]){0, penChar, penChar, penChar, 0, penChar, penChar, penChar, penChar, penChar, 0, penChar, penChar, penChar, 0}};
			break;
		case 4:
			stroke = (CharMap){.width=7, .height=4, .chars=(uint32_t[7*4]){0, 0, penChar, penChar, penChar, 0, 0, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, 0, 0, penChar, penChar, penChar, 0, 0}};
			break;
		case 5:
			stroke = (CharMap){.width=8, .height=4, .chars=(uint32_t[8*4]){0, penChar, penChar, penChar, penChar, penChar, penChar, 0, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, penChar, 0, penChar, penChar, penChar, penChar, penChar, penChar, 0}};
			break;
	}
	int dx = abs(x1-x0), dy = abs(y1-y0);
	int t, err = (dx > dy ? dx : -dy)/2;
	const short stepX = x0 < x1 ? (short)1 : (short)-1, stepY = y0 < y1 ? (short)1 : (short)-1;
	while(1) {
		vtStamp(canvas, &stroke, x0, y0);
		if(x0 == x1 && y0 == y1) {
			break;
		}
		t = err;
		if(t > -dx) {
			err -= dy;
			x0 += stepX;
		}
		if(t < dy) {
			err += dx;
			y0 += stepY;
		}
	}
}

//----MISC----
void about() {
	printf("                 .:Visual T:.\n"
	       "         ver ");
	puts("2.0.0  15 December 2019");
	puts("                 GNU LGPL-3.0\n"
	     "\n"
	     " A text-based graphic library written by Lucide\n"
	     "       https://github.com/Lucide/VisualT\n"
	     "\n"
	     "                                \\    /\\\n"
	     "       In Memory of Simba        )  ( ')\n"
	     "                                (  /  )\n"
	     "                           jgs   \\(__)|\n");
}

//----INITIALIZATION----
void initializeBlankObj(Obj *const obj, const unsigned int sizesLength, const unsigned int (*const sizes)[2]) {
	assert(sizesLength > 0);

	obj->length = sizesLength;
	obj->sprites = malloc(sizesLength*sizeof(CharMap));
	for(unsigned int i = 0; i < sizesLength; i++) {
		assert(sizes[i][0] > 0);
		assert(sizes[i][1] > 0);
		vtInitializeCharMap(&obj->sprites[i], sizes[i][0], sizes[i][1]);
		vtClearCharMap(&obj->sprites[i]);
	}
	vtInitializeObj(obj);
}

void initializeArrayObj(Obj *const obj, const uint32_t *v) {
	CharMap *sprite;

	obj->length = v[0];
	obj->sprites = malloc(obj->length*sizeof(CharMap));
	for(unsigned int i = 0, j = 1; i < obj->length; i++) {
		sprite = &obj->sprites[i];
		vtInitializeCharMap(sprite, v[j], v[j+1]);
		j += 2;
		for(unsigned int y = 0; y < sprite->height; y++) {
			for(unsigned int x = 0; x < sprite->width; x++, j++) {
				sprite->chars[x+y*sprite->width] = vt32EndiannessSwap(v[j]);
			}
		}
	}
	vtInitializeObj(obj);
}

void initializeFileObj(Obj *const obj, const char path[const]) {
	unsigned int vLength = 0;
	uint32_t *v;
	FILE *const file = fopen(path, "r");

	if(file) {
		while(fscanf(file, "%*" SCNu32) != EOF) { // NOLINT(cert-err34-c)
			vLength++;
		}
		v = malloc(vLength*sizeof(uint32_t));
		rewind(file);
		for(unsigned int i = 0; i < vLength; i++) {
			fscanf(file, "%" SCNu32, &v[i]); // NOLINT(cert-err34-c)
		}
		fclose(file);

		// for(unsigned int i = 0; i < vLength; i++) {
		// 	printf("%" PRIu32 " ", vt32EndiannessSwap(v[i]));
		// }
		// putchar('\n');

		initializeArrayObj(obj, v);
		free(v);
	}
}

void initializeStringObj(Obj *const obj, unsigned int utf8StringsLength, const uint8_t *const *const utf8Strings) {
	assert(utf8StringsLength > 0);

	obj->length = utf8StringsLength;
	obj->sprites = malloc(utf8StringsLength*sizeof(CharMap));
	for(unsigned int i = 0; i < utf8StringsLength; i++) {
		vtInitializeStringCharMap(&obj->sprites[i], utf8Strings[i]);
	}
	vtInitializeObj(obj);
}

void initializeObjObj(Obj *const obj, const Obj *const src) {
	CharMap *sprite;

	*obj = *src;
	obj->sprites = malloc(obj->length*sizeof(CharMap));
	for(unsigned int i = 0; i < obj->length; i++) {
		sprite = &obj->sprites[i];
		vtInitializeCharMap(sprite, src->sprites[i].width, src->sprites[i].height);
		memcpy(sprite->chars, src->sprites[i].chars, vtSizeofChars(sprite));
	}
	obj->currentSprite = &obj->sprites[src->currentSprite-src->sprites];
}

void releaseObj(const Obj *const obj) {
	for(unsigned int i = 0; i < obj->length; i++) {
		free(obj->sprites[i].chars);
	}
	free(obj->sprites);
}

void cloneSprite(const Obj *const dest, const unsigned int spriteDest, const Obj *const src, const unsigned int spriteSrc) {
	assert(spriteSrc < src->length);
	assert(spriteDest < dest->length);

	vtFreeCharMap(&dest->sprites[spriteDest]);
	dest->sprites[spriteDest] = src->sprites[spriteSrc];
	vtInitializeCharMap(&dest->sprites[spriteDest], dest->sprites[spriteDest].width, dest->sprites[spriteDest].height);
	memcpy(dest->sprites[spriteDest].chars, src->sprites[spriteSrc].chars, vtSizeofChars(&dest->sprites[spriteDest]));
}

//----CANVAS----
void resize(Obj *const canvas, const unsigned int width, const unsigned int height) {
	assert(width > 0);
	assert(height > 0);

	free(canvas->currentSprite->chars);
	free(canvas->currentSprite->chars);
	vtInitializeCharMap(canvas->currentSprite, width, height);
	vtInitializeCharMap(canvas->currentSprite, width, height);
	clear(canvas);
}

//----REFRESH----
void render(const Obj *const canvas, const unsigned int objsLength, const Obj *const *const objs) {
	vtRender(canvas->currentSprite, objsLength, objs, true);
}

void stamp(const Obj *const canvas, const unsigned int objsLength, const Obj *const *const objs) {
	vtRender(canvas->currentSprite, objsLength, objs, false);
}

void print(const Obj *const canvas, const bool border) {
	vtPrintCharMap(canvas->currentSprite, border);
}

unsigned int printToString(const Obj *const canvas, const bool border, uint8_t **const utf8String) {
	return vtPrintStringCharMap(canvas->currentSprite, border, utf8String);
}

//----TEXT----
void setSpriteText(Obj *const obj, const uint8_t *const utf8Text) {
	free(obj->currentSprite->chars);
	vtInitializeStringCharMap(obj->currentSprite, utf8Text);
}

//----SPRITE----
unsigned int sprites(const Obj *const obj) {
	return obj->length;
}

unsigned int sprite(const Obj *const obj) {
	return obj->currentSprite-obj->sprites;
}

void nextSprite(Obj *const obj) {
	obj->currentSprite = &(obj->sprites[(obj->currentSprite-obj->sprites+1)%obj->length]);
}

void precSprite(Obj *const obj) {
	obj->currentSprite = &(obj->sprites[(obj->currentSprite-obj->sprites+obj->length-1)%obj->length]);
}

void setSprite(Obj *const obj, const unsigned int sprite) {
	assert(sprite < obj->length);

	obj->currentSprite = &obj->sprites[sprite];
}

unsigned int width(const Obj *const obj) {
	return obj->currentSprite->width;
}

unsigned int height(const Obj *const obj) {
	return obj->currentSprite->height;
}

void clear(const Obj *const canvas) {
	vtClearCharMap(canvas->currentSprite);
}

void fill(const Obj *canvas, uint32_t fillChar) {
	for(unsigned int i = canvas->currentSprite->width*canvas->currentSprite->height; i-- > 0;) {
		canvas->currentSprite->chars[i] = fillChar;
	}
}

void overlay(const Obj *const dest, const unsigned int spriteDest, const Obj *const src, const unsigned int spriteSrc) {
	assert(spriteSrc < src->length);
	assert(spriteDest < dest->length);
	assert(src->sprites[spriteSrc].width == dest->sprites[spriteDest].width);
	assert(src->sprites[spriteSrc].height == dest->sprites[spriteDest].height);

	for(unsigned int i = src->sprites[spriteSrc].width*src->sprites[spriteSrc].height; i-- > 0;) {
		if(src->sprites[spriteSrc].chars[i]) {
			dest->sprites[spriteDest].chars[i] = src->sprites[spriteSrc].chars[i];
		}
	}
}

void printAxes(const Obj *const canvas) {
	uint32_t *const chars = canvas->currentSprite->chars;
	const unsigned int width = canvas->currentSprite->width, height = canvas->currentSprite->height;

	for(unsigned int i = 0; i < width; i++) {
		chars[(i)+(height/2)*width] = '-';
	}
	for(unsigned int i = 0; i < height; i++) {
		chars[(width/2)+(i)*width] = '|';
	}
	chars[(0)+(height/2)*width] = '-';
	chars[(width/2)+(0)*width] = '+';
	chars[(width-1)+(height/2)*width] = '+';
	chars[(width/2)+(height-1)*width] = '-';
	chars[(width/2)+(height/2)*width] = '0';
}

//----OBJ----
bool visible(const Obj *const obj) {
	return obj->visible;
}

void show(Obj *const obj) {
	obj->visible = true;
}

void hide(Obj *const obj) {
	obj->visible = false;
}

void setVisibility(Obj *const obj, const bool visible) {
	obj->visible = visible;
}

//----PEN----
uint32_t penChar(const Obj *const obj) {
	return obj->penChar;
}

unsigned short penSize(const Obj *const obj) {
	return obj->penSize;
}

void setPenSize(Obj *const obj, const unsigned short size) {
	obj->penSize = size <= 5 ? size : (unsigned short)5;
}

void setPenChar(Obj *const obj, const uint32_t penChar) {
	obj->penChar = penChar;
}

//----MOVE----
int xPosition(const Obj *const obj) {
	return obj->x;
}

int yPosition(const Obj *const obj) {
	return obj->y;
}

void gotoXY(const Obj *const canvas, Obj *const obj, const int x, const int y) {
	if(canvas) {
		vtLine(canvas->currentSprite, obj->penSize, obj->penChar, obj->x, obj->y, x, y);
	}
	obj->x = x;
	obj->y = y;
}

void gotoX(const Obj *const canvas, Obj *const obj, const int x) {
	if(canvas) {
		vtLine(canvas->currentSprite, obj->penSize, obj->penChar, obj->x, obj->y, x, obj->y);
	}
	obj->x = x;
}

void gotoY(const Obj *const canvas, Obj *const obj, const int y) {
	if(canvas) {
		vtLine(canvas->currentSprite, obj->penSize, obj->penChar, obj->x, obj->y, obj->x, y);
	}
	obj->y = y;
}

void changeX(const Obj *const canvas, Obj *const obj, const int x) {
	if(canvas) {
		vtLine(canvas->currentSprite, obj->penSize, obj->penChar, obj->x, obj->y, obj->x+x, obj->y);
	}
	obj->x += x;
}

void changeY(const Obj *const canvas, Obj *const obj, const int y) {
	if(canvas) {
		vtLine(canvas->currentSprite, obj->penSize, obj->penChar, obj->x, obj->y, obj->x, obj->y-y);
	}
	obj->y += y;
}

void align(Obj *const obj, const unsigned char position) {
	switch(position) {
		default:
			return;
		case 0: //top-left
			obj->x += (int)(obj->currentSprite->width/2);
			obj->y -= (int)(obj->currentSprite->height/2);
			break;
		case 1: //top-right
			obj->x -= (int)(obj->currentSprite->width/2);
			obj->y -= (int)(obj->currentSprite->height/2);
			if(obj->currentSprite->width%2 == 0) {
				obj->x++;
			}
			break;
		case 2: //bottom-left
			obj->x += (int)(obj->currentSprite->width/2);
			obj->y += (int)(obj->currentSprite->height/2);
			if(obj->currentSprite->height%2 == 0) {
				obj->y--;
			}
			break;
		case 3: //bottom-right
			obj->x -= (int)(obj->currentSprite->width/2);
			obj->y += (int)(obj->currentSprite->height/2);
			if(obj->currentSprite->width%2 == 0) {
				obj->x++;
			}
			if(obj->currentSprite->height%2 == 0) {
				obj->y--;
			}
			break;
	}
}

bool isTouching(const Obj *const canvas, const Obj *const obj, unsigned int objsLength, const struct Obj *const *const objs) {
	const CharMap *const sprite = obj->currentSprite;
	int spriteX, spriteY;

	if(obj->visible) {
		const BoolMap boolMap = (BoolMap){malloc(canvas->currentSprite->width*canvas->currentSprite->height*sizeof(bool)), canvas->currentSprite->width, canvas->currentSprite->height};
		memset(boolMap.chars, false, vtSizeofChars(&boolMap)/sizeof(uint32_t)*sizeof(bool));
		for(unsigned int i = 0; i < objsLength; i++) {
			if(objs[i]->visible) {
				vtMask(&boolMap, objs[i]->currentSprite, objs[i]->x, objs[i]->y);
			}
		}
		vtNormalizePosition(&boolMap, sprite, obj->x, obj->y, spriteX, spriteY);
		for(unsigned int y = 0; y < sprite->height; y++) {
			for(unsigned int x = 0; x < sprite->width; x++) {
				if(sprite->chars[x+y*sprite->width] && spriteX+(int)x >= 0 && spriteX+(int)x < (int)boolMap.width && spriteY+(int)y >= 0 && spriteY+(int)y < (int)boolMap.height && boolMap.chars[(spriteX+x)+(spriteY+y)*boolMap.width]) {
					free(boolMap.chars);
					return true;
				}
			}
		}
		free(boolMap.chars);
	}
	return false;
}

bool isTouchingChar(const Obj *const canvas, const Obj *const obj, const uint32_t character) {
	const CharMap *const charMap = canvas->currentSprite, *const sprite = obj->currentSprite;
	int spriteX, spriteY;

	if(obj->visible) {
		vtNormalizePosition(charMap, sprite, obj->x, obj->y, spriteX, spriteY);
		for(unsigned int y = 0; y < sprite->height; y++) {
			for(unsigned int x = 0; x < sprite->width; x++) {
				if(sprite->chars[x+y*sprite->width] && spriteX+(int)x >= 0 && spriteX+(int)x < (int)charMap->width && spriteY+(int)y >= 0 && spriteY+(int)y < (int)charMap->height && charMap->chars[(spriteX+x)+(spriteY+y)*charMap->width] == character) {
					return true;
				}
			}
		}
	}
	return false;
}

bool isOutside(const Obj *const canvas, const Obj *const obj) {
	const CharMap *const charMap = canvas->currentSprite, *const sprite = obj->currentSprite;
	int spriteX, spriteY;

	if(obj->visible) {
		vtNormalizePosition(charMap, sprite, obj->x, obj->y, spriteX, spriteY);
		for(unsigned int y = 0; y < sprite->height; y++) {
			for(unsigned int x = 0; x < sprite->width; x++) {
				if(sprite->chars[x+y*sprite->width] && !(spriteX+(int)x >= 0 && spriteX+(int)x < (int)charMap->width && spriteY+(int)y >= 0 && spriteY+(int)y < (int)charMap->height)) {
					return true;
				}
			}
		}
	}
	return false;
}
