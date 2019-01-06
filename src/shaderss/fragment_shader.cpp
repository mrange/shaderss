#include "stdafx.h"

char const * const get_fragment_shader ()
{
  return R"SHADER(
// -----------------------------------------------------------------------
// BEGIN - Common prelude
// -----------------------------------------------------------------------
#version 430

precision mediump float;

layout (location=0) uniform vec4 fpar[];
layout (location=0) out vec4 co;
in vec2 p;

vec2 iResolution = vec2(1.0);
float iTime = 1.0;

void mainImage(out vec4 fragColor, in vec2 fragCoord);

void main()
{
  iTime = fpar[0].x;
  iResolution.x = fpar[0].y;
  iResolution.y = fpar[0].z;
  vec2 pp = (p + 1.0)*0.5*iResolution.xy;

  mainImage(co, pp);
}
// -----------------------------------------------------------------------
// END - Common prelude
// -----------------------------------------------------------------------


// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// Minor modification from: https://www.shadertoy.com/view/4dj3Wy


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 p = vec2(-0.5, 0.0) +  1.15*(-iResolution.xy+2.0*fragCoord.xy)/iResolution.y;

	vec2 z = p;
	float f = 4.;
	float g = 4.;
	
	for( int i=0; i<24; i++ ) 
	{
		float w = float(i)*1.32457+iTime;
		vec2 z1 = vec2(2.*cos(w),2.*sin(w));		   
		z = vec2( z.x*z.x-z.y*z.y, 2.0*z.x*z.y ) + p;
		f = min( f, abs(dot(z-p,z-p) -.004*float(i)));
		g = min( g, dot(z-z1,z-z1));
	}
	
	f = 1.0+log(f)/15.0;
	g = 1.0+log(g)/8.0;


	fragColor = 1.-abs(vec4(g,f*f,f*f*f,1.0));
}

// -----------------------------------------------------------------------------
)SHADER";
}