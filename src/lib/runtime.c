#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"
#include <string.h>


extern Type _class_String;
extern Type _class_Object;
extern Type _class_Array;

extern void bzero(void *s, size_t n);
extern void *memcpy(void *dest, const void *src, size_t n);

typedef obj (*toStrPtr)(obj);
char *err;
uint8_t empty[] = "";

obj _new(Type *t) {
  obj res = malloc(sizeof(struct ObjReference));
  res->type = t;
  if (t->size > 0 && t != &_class_Array && t != &_class_String) {
    res->data = malloc(t->size);
    bzero(res->data, t->size);
  } else {
    res->data = NULL;
  }

  return res;
}

obj _cast(obj o, Type *t) {
  if (o != NULL) {
    Type *oldT = o->type;
    while (oldT != NULL) {
      if (t == oldT) {
        return o;
      }
      oldT = oldT->inhParent;
    }
  }

  return NULL;
}

void *_getarritemptr(obj arrObj, int32_t ind) {
  if (arrObj == NULL) {
    err = "NullPointerException - Tried accessing an element of a null array";
    _error();
  }
  Array *arr = ((Array *)arrObj->data);
  if (ind >= 0 && ind < arr->len) {
    return arr->items + arr->itemSize * ind;
  } else {
    err = "ArrayIndexOutOfBounds - Tried accesing an element beyond array bounds";
    _error();
  }
}

obj _new_int_arr(int32_t len) {
  return _new_arr(len, sizeof(int32_t));
}

obj _new_obj_arr(int32_t len) {
  return _new_arr(len, sizeof(obj));
}

obj _new_byte_arr(int32_t len) {
  return _new_arr(len, sizeof(int8_t));
}

obj _new_arr(int32_t len, int32_t size) {
  obj o = _new(&_class_Array);
  Array *arr = malloc(sizeof(Array));
  
  o->data = arr;
  arr->itemSize = size;
  arr->len = len;

  if (len < 0) {
    err = "ArrayInitException - Tried initializing array with negative size";
    _error();
  }

  if (len == 0) {
    arr->items = NULL;
    return o;
  }

  arr->items = malloc(len * size);
  bzero(arr->items, len * size);

  return o;
}

obj _new_string(char *c) {
  if (c == NULL) {
    return _new_string(empty);
  }

  obj o = _new(&_class_String);
  Str *str = malloc(sizeof(Str));
  o->data = str;
  str->len = strlen(c);
  int len = str->len;

  if (len <= 0) {
    str->data = 0;
    return o;
  } else {
    str->data = malloc(len + 1);
    memcpy(str->data, c, len);
    str->data[len] = 0; //string terminator
  }

  str->len = -1;
  return o;
}

void _null_err() {
  err = "NullPointerException - Tried referencing an object that is null";
  _error();
}

int8_t _printInt(int32_t i) {
  printf("%d\n", i);
  return 0;
}

int8_t _printString(obj str) {
  if (str == NULL) {
    str = _new_string("null");
  }

  uint8_t *printStr = ((Str *) str->data)->data;
  printf("%s\n", printStr);

  return 0;
}

int32_t _readInt() {
  int32_t res;
  char *buffer = NULL;
  size_t size = 0;

  ssize_t getline_res = getline(&buffer, &size, stdin);
  int sscanf_res = sscanf(buffer, "%d", &res);

  free(buffer);
  return res;
}

obj _readString() {
  char *buffer = NULL;
  size_t size = 0;

  ssize_t getline_res = getline(&buffer, &size, stdin);
  size = strlen(buffer);
  
  buffer[size - 1] = 0;
  obj res = _new_string(buffer);

  free(buffer);
  return res; 
}

int8_t _error() {
  if (err == NULL) {
    err = "RuntimeException - Error function run";
  }

  fprintf(stderr, "%s\n", err);
  exit(1);
  return 1;
}

int8_t _equals_str(obj o1, obj o2) {
  if (o2 == NULL || o2->type != &_class_String || _length_str(o1) != _length_str(o2)) {
    return 0;
  }
  uint8_t *str1 = ((Str *)o1->data)->data;
  uint8_t *str2 = ((Str *)o2->data)->data;
  return strcmp(str1, str2) == 0; //Think about u8_strcmp
}

int32_t _length_str(obj o) {
  Str *str = o->data;
  if (str->len < 0) {
    str->len = strlen(str->data);
  }
  return str->len;
}

obj _concat_str(obj o1, obj o2) {
  if (o2 == NULL) {
    return o1;
  }

  uint8_t *str1 = ((Str *)o1->data)->data;
  int32_t len1 = strlen(str1);
  uint8_t *str2 = ((Str *)o2->data)->data;
  int32_t len2 = strlen(str2);

  uint8_t *tmp = malloc(len1 + len2 + 1);
  strcpy(tmp, str1);
  strcpy(tmp + len1, str2);
  tmp[len1 + len2] = 0;

  obj res = _new_string(tmp);
  
  free(tmp);
  return res;
}

int8_t _equals_obj(obj o1, obj o2) {
  return (o1 == o2);
}

obj _to_string_obj(obj o) {
  return _new_string("Object");
}

