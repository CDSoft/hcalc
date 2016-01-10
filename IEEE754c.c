#include <stdint.h>

uint64_t doubleToWord64 (double   d) { return *(uint64_t*)&d; }
double   word64ToDouble (uint64_t u) { return *(double*)&u; }
uint32_t floatToWord32  (float    f) { return *(uint32_t*)&f; }
float    word32ToFloat  (uint32_t u) { return *(float*)&u; }
double   floatToDouble  (float    f) { return f; }
float    doubleToFloat  (double   d) { return d; }
