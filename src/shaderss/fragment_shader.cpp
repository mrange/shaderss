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

// Created by mrange/2018
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

#define TOLERANCE       0.00001
#define MAX_RAY_LENGTH  5.0
#define MAX_BOUNCES     5
#define MAX_RAY_MARCHES 75

#define PI              3.141592654
#define TAU             (2.0*PI)
#define PHI             (sqrt(5.0)*0.5 + 0.5)

#define DEG2RAD         (PI/180.0)

#define FADEINTIME      2.0
#define ACTIVATETIME    4.0

#define AA              0

const vec3 gdf3  = normalize(vec3(1, 1, 1 ));
const vec3 gdf4  = normalize(vec3(-1, 1, 1));
const vec3 gdf5  = normalize(vec3(1, -1, 1));
const vec3 gdf6  = normalize(vec3(1, 1, -1));
const vec3 gdf7  = normalize(vec3(0, 1, PHI+1.0));
const vec3 gdf8  = normalize(vec3(0, -1, PHI+1.0));
const vec3 gdf9  = normalize(vec3(PHI+1.0, 0, 1));
const vec3 gdf10 = normalize(vec3(-PHI-1.0, 0, 1));
const vec3 gdf11 = normalize(vec3(1, PHI+1.0, 0));
const vec3 gdf12 = normalize(vec3(-1, PHI+1.0, 0));

const vec3 inert   = 0.5*vec3(1.0, 3.0, 2.0);
const vec3 radiant = vec3(1.0, 1.0/3.0, 1.0/2.0);

const vec3 lightPos1 = 100.0*vec3(-1.0, 0.0, 0.0);
const vec3 lightCol1 = vec3(0.63, 0.63, 1.0);

float linstep(in float f, in float t, in float x)
{
  return clamp((x - f)/(t - f), 0.0, 1.0);
}

void pR(inout vec2 p, float a)
{
  p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float maxComp(in vec3 p)
{
  return max(p.x,max(p.y,p.z));
}


float fIcosahedron(vec3 p, float r) {
	float d = 0.0;
    d = max(d, abs(dot(p, gdf3)));
    d = max(d, abs(dot(p, gdf4)));
    d = max(d, abs(dot(p, gdf5)));
    d = max(d, abs(dot(p, gdf6)));
    d = max(d, abs(dot(p, gdf7)));
    d = max(d, abs(dot(p, gdf8)));
    d = max(d, abs(dot(p, gdf9)));
    d = max(d, abs(dot(p, gdf10)));
    d = max(d, abs(dot(p, gdf11)));
    d = max(d, abs(dot(p, gdf12)));
	return d - r;
}

float impulse1(in vec3 p, out vec3 col, out float ref, out float trans, out vec3 absorb)
{
  col    = 0.3*radiant;
  ref    = 0.4;
  trans  = 0.9;

  float time = iTime - ACTIVATETIME;

  absorb = mix(inert, -(1.0 + 0.25*cos(time/8.0))*radiant, linstep(0.0, 1.0, time));

  return fIcosahedron(p, 1.0);
}

float distanceField(in vec3 p, out vec3 col, out float ref, out float trans, out vec3 absorb)
{
  pR(p.yz, -PI/4.0);
  pR(p.xy, iTime/3.0);
  pR(p.yz, PI/4.0);

  float i = impulse1(p, col, ref, trans, absorb);

  return i;
}

vec3 saturate(in vec3 a)   { return clamp(a, 0.0, 1.0); }
vec2 saturate(in vec2 a)   { return clamp(a, 0.0, 1.0); }
float saturate(in float a) { return clamp(a, 0.0, 1.0); }

vec3 getSkyColor(vec3 rayDir)
{
  vec3 lightDir1 = normalize(lightPos1);

  float ld1      = max(dot(lightDir1, rayDir), 0.0);
  vec3 final     = 0.1*lightCol1;

  if ((rayDir.y > abs(rayDir.x)*1.0) && (rayDir.y > abs(rayDir.z*0.25))) final = vec3(2.0)*rayDir.y;
  float roundBox = length(max(abs(rayDir.xz/max(0.0,rayDir.y))-vec2(0.9, 4.0),0.0))-0.1;
  final += vec3(0.8)* pow(saturate(1.0 - roundBox*0.5), 6.0);

  float time = iTime-ACTIVATETIME;

  vec3 light = linstep(0.0, 0.5, time)*(1.0 - linstep(2.0, 6.0, time))*lightCol1;

  final += light*pow(ld1, 20.0);
  return final;
}

vec3 normal(in vec3 pos)
{
  vec3  eps = vec3(.0001,0.0,0.0);
  vec3 col;
  float ref;
  float trans;
  vec3 nor;
  vec3 absorb;
  nor.x = distanceField(pos+eps.xyy, col, ref, trans, absorb) - distanceField(pos-eps.xyy, col, ref, trans, absorb);
  nor.y = distanceField(pos+eps.yxy, col, ref, trans, absorb) - distanceField(pos-eps.yxy, col, ref, trans, absorb);
  nor.z = distanceField(pos+eps.yyx, col, ref, trans, absorb) - distanceField(pos-eps.yyx, col, ref, trans, absorb);
  return normalize(nor);
}

float rayMarch(in float dmod, in vec3 ro, inout vec3 rd, float mint, float minstep, out int rep, out vec3 col, out float ref, out float trans, out vec3 absorb)
{
  float t = mint;
  float distance;
  for (int i = 0; i < MAX_RAY_MARCHES; i++)
  {
    float distance_ = distanceField(ro + rd*t, col, ref, trans, absorb);
    distance = dmod*distance_;
    if (distance < TOLERANCE || t > MAX_RAY_LENGTH) break;
    t += max(distance, minstep);
    rep = i;
  }

  if (distance > TOLERANCE) return MAX_RAY_LENGTH;

  return t;
}

vec3 postProcess(in vec3 col, in vec2 p)
{
  col=pow(clamp(col,0.0,1.0),vec3(0.75));
  col=col*0.6+0.4*col*col*(3.0-2.0*col);  // contrast
  col=mix(col, vec3(dot(col, vec3(0.33))), -0.4);  // satuation
  return col;
}

vec3 render(in vec3 ro, in vec3 rd)
{
  vec3 lightPos = 1.5*vec3(1.0, 3.0, 1.0);

  vec3 final  = vec3(0.0);

  vec3 ragg   = vec3(1.0);

  float tdist = 0.0;

  float refraction = 0.95;

  bool inside = false;

  float mint    = 0.01;
  float minstep = 0.001;

  for (int i = 0; i < MAX_BOUNCES; ++i)
  {
    if (maxComp(ragg) <  0.01) break;
    float dmod  = inside ? -1.0 : 1.0;
    vec3 absorb ;
    vec3 col    ;
    float ref   ;
    float trans ;
    int rep     ;
    float t     = rayMarch(dmod, ro, rd, mint, minstep, rep, col, ref, trans, absorb);
    tdist       += t;

    vec3 pos    = ro + t*rd;

    vec3 nor = vec3(0.0, 1.0, 0.0);

    if (t < MAX_RAY_LENGTH)
    {
      // Ray intersected object
      nor = normal(pos);
    }
    else
    {
      // Ray intersected sky
      final += ragg*getSkyColor(rd);
      break;
    }

    float fresnel = pow(1.0 - abs(dot(nor, rd)), 2.0);

    ref = mix(ref, 1.0, fresnel);
    trans = mix(trans, 0.0, fresnel);

    float mref = refraction;

    if (inside)
    {
      nor = -nor;
      mref = 1.0/refraction;
    }

    vec3 refl = reflect(rd, nor);
    vec3 refr = refract(rd, nor, mref);

    vec3 lv   = lightPos - pos;
    vec3  ld  = normalize(lv);
    float ll  = length(lv);

    float dif = max(dot(nor,ld),0.0);
    float occ = 1.0 - float(rep)/float(MAX_RAY_MARCHES);
    float l   = dif*occ;

    vec3 lr   = vec3(0.0);

    float lin = mix(0.2, 1.0, l);

    vec3 sky  = getSkyColor(refl);
    vec3 mcol = mix(lin*col + lr, sky, ref);

    vec3 beer = vec3(1.0);

    if (inside)
    {
      beer = exp(-absorb*t);
    }

    final      += (1.0 - trans)*ragg*beer*mcol;
    ragg       *= trans*beer;

    ro        = pos;

    if (refr == vec3(0.0))
    {
        rd = refl;
    }
    else
    {
      rd = refr;
      inside = !inside;
    }
  }

  return final;
}

vec3 getSample(in vec2 p)
{
  if (length(p) > 1.0) return vec3(0.0);

  vec3 ro  = vec3(3.0, 0.0, 0.0);

  vec3 la  = vec3(0.0);

  vec3 ww = normalize(la - ro);
  vec3 uu = normalize(cross(vec3(0.0,1.0,0.0), ww ));
  vec3 vv = normalize(cross(ww,uu));
  vec3 rd = normalize( p.x*uu + p.y*vv + 2.0*ww );

  vec3 col = render(ro, rd);

  return col;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
  vec2 q=fragCoord.xy/iResolution.xy;
  vec2 p = -1.0 + 2.0*q;
  p.x *= iResolution.x/iResolution.y;

#if AA == 0
  vec3 col = getSample(p);
#elif AA == 1
  vec3 col  = vec3(0.0);
  vec2 unit = 1.0/iResolution.xy;
  for(int y = 0; y < 2; ++y)
  {
    for(int x = 0; x < 2; ++x)
    {
      col += getSample(p - 0.5*unit + unit*vec2(x, y));
    }
  }

  col /= 4.0;
#endif

  float fadeIn = linstep(0.0, FADEINTIME, iTime);

  fragColor = vec4(postProcess(col, p)*fadeIn, 1.0);
}

// -----------------------------------------------------------------------------
)SHADER";
}