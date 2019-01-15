#version 330 core

invariant gl_Position;

#define LOCATION_POSITION 1

#pragma bodge: include "src/common_types.fs"

#pragma bodge: include "src/variables.vs"

#pragma bodge: use amalgam ge.shad::filament-frame-uniforms as uniform block frameUniforms
#pragma bodge: use amalgam ge.shad::filament-object-uniforms as uniform block objectUniforms

#pragma bodge: include "src/common_math.fs"
#pragma bodge: include "src/common_getters.fs"
#pragma bodge: include "src/getters.vs"
#pragma bodge: include "src/common_material.vs"

#pragma bodge: include "src/shadowing.vs"


void materialVertex(inout MaterialVertexInputs material) {
}

#pragma bodge: include "src/main.vs"
