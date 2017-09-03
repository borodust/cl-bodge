#version 330 core

layout(location = 0) in vec4 vBox;
layout(location = 1) in vec4 vSdfCoord;


out v_PerVertex {
  vec4 box;
  vec4 sdfCoord;
};

void main () {
  box = vBox;
  sdfCoord = vSdfCoord;
}
