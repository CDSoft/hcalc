/* Handy Calc
Copyright (C) 2016 Christophe Delord
http://cdsoft.fr/hcalc

This file is part of Handy Calc.

Handy Calc is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Handy Calc is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Handy Calc.  If not, see <http://www.gnu.org/licenses/>.
*/

/* The module IEEE754 converts floating point numbers
 * from/to their IEEE754 representation.
 * It is coded in C and uses simple casts to make the conversion.
 */

#include <stdint.h>

uint64_t doubleToWord64 (double   d) { return *(uint64_t*)&d; }
double   word64ToDouble (uint64_t u) { return *(double*)&u; }
uint32_t floatToWord32  (float    f) { return *(uint32_t*)&f; }
float    word32ToFloat  (uint32_t u) { return *(float*)&u; }
double   floatToDouble  (float    f) { return f; }
float    doubleToFloat  (double   d) { return d; }
