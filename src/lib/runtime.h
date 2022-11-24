#ifndef __RUNTIME_H
#define __RUNTIME_H

#include <inttypes.h>

#define UPPER_INT_SIZE 12 //TODO check

typedef struct Str {
  uint8_t *data;
  int32_t len;
} Str;

typedef struct Array {
  void *items;
  int32_t itemSize;
  int32_t len;
} Array;

typedef struct __attribute__((__packed__)) Type {
  struct Type *inhParent;
  void *methods;
  int32_t size;
  int32_t *referenceOffsets;
  int32_t *referenceOffsetsSize;
} Type;

typedef struct __attribute__((__packed__)) ObjReference {
  Type *type;
  void *data;
} *obj;

obj _new(Type *t);
void _free(obj o);

obj _cast(obj o, Type *t);

void *_getarritemptr(obj arrObj, int32_t ind);
obj _new_int_arr(int32_t len);
obj _new_obj_arr(int32_t len);
obj _new_arr(int32_t len, int32_t size);
obj _new_string(char *c);

void _null_err();

int8_t _printInt(int32_t i);
int8_t _printString(obj str);
int32_t _readInt();
obj _readString();
int8_t _error();
obj _intToString(int32_t i);
obj _boolToString(uint8_t b); //TODO do wywalenia?

int8_t _equals_str(obj o1, obj o2);
obj _concat_str(obj o1, obj o2);
int32_t _length_str(obj o);
int8_t _equals_obj(obj o1, obj o2);
obj _to_string_obj(obj o);

#endif