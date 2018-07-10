#ifndef _BODGE_SHADOW_H
#define _BODGE_SHADOW_H

bool inShadow(vec3 lightDirection, samplerCube shadowMap,
              float near, float far, float epsilon);

#endif // _BODGE_SHADOW_H
