#version 330 core

#define IBL_MAX_MIP_LEVEL 9 // std::log2f(filament::CONFIG_IBL_SIZE)

#pragma bodge: include "src/common_types.fs"

#pragma bodge: include "src/variables.fs"

#pragma bodge: use amalgam ge.shad::filament-frame-uniforms as uniform block frameUniforms
#pragma bodge: use amalgam ge.shad::filament-lights-uniforms as uniform block lightsUniforms

#pragma bodge: use amalgam ge.shad::filament-light as uniform list

#pragma bodge: include "src/common_math.fs"
#pragma bodge: include "src/common_graphics.fs"

#pragma bodge: include "src/common_getters.fs"
#pragma bodge: include "src/getters.fs"

#pragma bodge: include "src/common_material.fs"

#pragma bodge: include "src/shading_parameters.fs"


#pragma bodge: include "src/common_lighting.fs"
#pragma bodge: include "src/brdf.fs"
#pragma bodge: include "src/shading_model_standard.fs"
#pragma bodge: include "src/light_indirect.fs"
#pragma bodge: include "src/shading_lit.fs"


void material(inout MaterialInputs m) {
  prepareMaterial(m);
}

#pragma bodge: include "src/main.fs"
