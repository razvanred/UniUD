#ifndef VISUALT_INCLUDE_VISUALT_VISUALT_H_
#define VISUALT_INCLUDE_VISUALT_VISUALT_H_

#include <stdbool.h>
#include <stdint.h>

// #define VISUALT_UNBUFFERED_PRINT

#define VTSTR (const uint8_t *const)
#define LTSTR VTSTR

#define VTSTRS (const uint8_t *const *const)
#define LTSTRS VTSTRS(const char *const [])

#define VTSIZES (const unsigned int (*const)[2])
#define LTSIZES VTSIZES(const unsigned int [][2])

#define VTCHAR *(const uint32_t *const)
#define LTCHAR VTCHAR

#define VTOBJS (const struct Obj *const *)
#define LTOBJS VTOBJS(const Obj *const [])

struct CharMap {
	uint32_t *chars;
	unsigned int width, height;
};

struct Obj {
	struct CharMap *sprites, *currentSprite;
	uint32_t penChar;
	int x, y;
	unsigned int length;
	unsigned short penSize;
	bool visible;
};

//----MISC----
void about(); //print info about VisualT
//----INITIALIZATION----
void initializeBlankObj(struct Obj *obj, unsigned int sizesLength, const unsigned int (*sizes)[2]);
void initializeArrayObj(struct Obj *obj, const uint32_t *v);
void initializeFileObj(struct Obj *obj, const char path[const]);
void initializeStringObj(struct Obj *obj, unsigned int utf8StringsLength, const uint8_t *const *utf8Strings);
void initializeObjObj(struct Obj *obj, const struct Obj *src);
void releaseObj(const struct Obj *obj);
void cloneSprite(const struct Obj *dest, unsigned int spriteDest, const struct Obj *src, unsigned int spriteSrc);
//----CANVAS----
void resize(struct Obj *canvas, unsigned int width, unsigned int height);
//----REFRESH----
void render(const struct Obj *canvas, unsigned int objsLength, const struct Obj *const *objs);
void print(const struct Obj *canvas, bool border);
unsigned int printToString(const struct Obj *canvas, bool border, uint8_t **utf8String);
//----TEXT----
void setSpriteText(struct Obj *obj, const uint8_t *utf8Text);
//----SPRITE----
unsigned int sprites(const struct Obj *obj);
unsigned int sprite(const struct Obj *obj);
void nextSprite(struct Obj *obj);
void precSprite(struct Obj *obj);
void setSprite(struct Obj *obj, unsigned int sprite);
unsigned int width(const struct Obj *obj);
unsigned int height(const struct Obj *obj);
void clear(const struct Obj *canvas);
void fill(const struct Obj *canvas, uint32_t fillChar);
void overlay(const struct Obj *dest, unsigned int spriteDest, const struct Obj *src, unsigned int spriteSrc);
void stamp(const struct Obj *canvas, const struct Obj *obj);
void printAxes(const struct Obj *canvas);
//----OBJ----
bool visible(const struct Obj *obj);
void show(struct Obj *obj);
void hide(struct Obj *obj);
void setVisibility(struct Obj *obj, bool visible);
//----PEN----
uint32_t penChar(const struct Obj *obj);
unsigned short penSize(const struct Obj *obj);
void setPenSize(struct Obj *obj, unsigned short size);
void setPenChar(struct Obj *obj, uint32_t penChar);
//----MOVE----
int xPosition(const struct Obj *obj);
int yPosition(const struct Obj *obj);
void moveTo(const struct Obj *canvas, struct Obj *obj, int x, int y);
void setX(const struct Obj *canvas, struct Obj *obj, int x);
void setY(const struct Obj *canvas, struct Obj *obj, int y);
void changeX(const struct Obj *canvas, struct Obj *obj, int x);
void changeY(const struct Obj *canvas, struct Obj *obj, int y);
void align(struct Obj *obj, unsigned char position);
bool isTouching(const struct Obj *canvas, const struct Obj *obj, unsigned int objsLength, const struct Obj *const *objs);
bool isTouchingChar(const struct Obj *canvas, const struct Obj *obj, uint32_t character);
bool isOutside(const struct Obj *canvas, const struct Obj *obj);

#endif

//----MISC----

//to free a dynamic 2d array mna of width width

//----POINTERS----
//to refer to an object's INDex
//to group "n" objects by passing their index
//to delete the data contained on a group; now you can reuse the group

//----GETTERS----
//to Get the Number of Objects loaded
//to Get the Object's INDex
//to Get the Object's NAME
//to Get the Object's Sprite Number
//to Get the Object's Sprite X size
//to Get the Object's Sprite Y size

//----INITIALIZATION----
//REQUIRED to initialize the library
//to load an object
//to create a dynamic textbox
//to clone (reload) the object
//to free all the memory allocated by the library

//----STAGE----
//to Get the STage Width
//to Get the STage Height
//to create/size a new stage
//to set the stage border visibility (enabled by default, for developers)

//----REFRESH----
//to render the scene
//to render to file
//to render to a printf() sequence, waiting for i milliseconds
//to move the cursor to a certain x y

//----TEXT----
//to change a textbox's content, works for every object

//----LOOK----
//to Get the object's current Sprite Number
//to Get the object's VISIBILITY state
//to Get the object's LAYER
//to show the object
//to hide the object
//to set the object's visibility (1/0)
//to move the object to the top layer
//to move the object to the bottom layer
//to move the object up a layer
//to move the object down a layer
//to switch to the object's next Sprite
//to switch to the object's precedent Sprite
//to switch to the object's n Sprite

//----PEN----
//to Get the object's PEN state
//to Get the PEN CHaracter in use
//to Get the PEN SIZE
//to set the pen size
//to set the pen character
//to disable the object's pen
//to enable the object's pen
//to set the object's pen (1/0)
//to stamp the object on the pen layer
//to shift the pen layer by m direction m=0:left m=1:right m=2:top m=3:down
//to set the pen layer to a character
//to clear the pen layer

//----MOVE----
//to Get the object's x position
//to Get the object's y position
//to know if two Objects are touching  (d=0 to check for any object)
//to know if the object is touching a Character (d=0 to check for border)
//to move the object to x y
//to move the object to x
//to move the object to y
//to change the object's x position by x
//to change the object's y position by y
//to move the object's corner to his current position (useful with textboxes) m=0:top-left m=1:top-right m=2:bottom-left m=3:bottom-right

//----DEBUG----
//to show your program general status
//to show the status of all the loaded objects
//to show the oject's status
//to render x and y axes
