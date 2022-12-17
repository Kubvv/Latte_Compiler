#ifndef __RUNTIME_H
#define __RUNTIME_H

#include <inttypes.h>

/* Library holds the definitions and implementations of all pre defined functions 
 * and methods from pre defined classes. There are also the definitions of the
 * String and Array object object, as well as the struct describing a pointer
 * and a Type */

typedef struct Str {
  int32_t len; // Length of the string
  uint8_t *data; // Pointer to the char array (string)
} Str;

typedef struct Array {
  int32_t itemSize; // Holds the size of a single item of the array
  int32_t len; // Length of the array
  void *items; // Pointer to the start of the array
} Array; // Class for representing arrays

typedef struct __attribute__((__packed__)) Type {
  struct Type *inhParent; // Parent of the class, used solely for casting purposes
  int32_t size; // Describes the total size of the given type
} Type; // Type holds crucial information about the pointer's type

typedef struct __attribute__((__packed__)) ObjReference {
  Type *type; // Type of the pointer
  void *data; // Held data of the pointer
} *obj;

obj _new(Type *t);

obj _cast(obj o, Type *t);

// get the item from array arrObj at the index ind
void *_getarritemptr(obj arrObj, int32_t ind);

// declare new array of int, byte (bool) and obj (class) respectively
// diffrent methods are used as all this type have a diffrent item size,
// but after declaring their size they all call the same method _new_arr 
obj _new_int_arr(int32_t len);
obj _new_byte_arr(int32_t len);
obj _new_obj_arr(int32_t len);

// Creates an array of length 'len' of type with size 'size'
obj _new_arr(int32_t len, int32_t size);

// Creates a new string object
obj _new_string(char *c);

// _null_err throws an error when a user code tries to access an element of a null (NullPointerException)
void _null_err();

// predefined functions
int8_t _printInt(int32_t i);
int8_t _printString(obj str);
int32_t _readInt();
obj _readString();
int8_t _error();

// predefined methods
int32_t _String_length(obj o);
int8_t _String_equals(obj o1, obj o2);
obj _String_concat(obj o1, obj o2);
int8_t _Object_equals(obj o1, obj o2);

#endif