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

#define TOLERANCE       0.00001
#define MAX_RAY_LENGTH  8.0
#define MAX_BOUNCES     5
#define MAX_RAY_MARCHES 75

#define PI              3.141592654
#define TAU             (2.0*PI)

#define DEG2RAD         (PI/180.0)

#define AA              0

void pR(inout vec2 p, float a)
{
  p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float maxComp(in vec3 p)
{
  return max(p.x,max(p.y,p.z));
}

float lengthN(in vec3 v, in float n)
{
  v = abs(v);
  v = pow(v, vec3(n));
  return pow(v.x + v.y + v.z, 1.0/n);
}

float sdRoundCube(in vec3 p, float r)
{
  return lengthN(p, 8.0) - r;
}

float sdBox(in vec3 p, in vec3 b)
{
  vec3  di = abs(p) - b;
  return maxComp(di);
}

float sgn(float x)
{
  return (x<0.0)?-1.0:1.0;
}

void pR90(inout vec2 p)
{
  p = vec2(p.y, -p.x);
}

float pMirror(inout float p, float dist)
{
  float s = sgn(p);
  p = abs(p)-dist;
  return s;
}

float fCapsule(vec3 p, float r, float c)
{
  return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}

float capsule(in vec3 p, float r, in vec3 off, in float rot)
{
  p -= off;
  pR(p.xy, rot*DEG2RAD);
  return fCapsule(p, r, 5.0);
}

float letterw(in vec3 p, float r)
{
  vec3 o  = vec3(3.0, 0.0, 0.0);
  float a = 17.0;

  pMirror(p.x, 3.0);
  float d0 = capsule(p, r, -0.5*o, a);
  float d1 = capsule(p, r, 0.5*o, -a);
  float d  = min(d0, d1);

  return d;
}

float letterm(in vec3 p, float r)
{
  vec3 o  = vec3(3.0, 0.0, 0.0);
  float a = 22.0;
  p.x /= 1.2;

  pMirror(p.x, 2.3);
  p.x = -p.x;
  float d0 = capsule(p, r, -0.5*o, 0.0);
  float d1 = capsule(p - vec3(-1.1, 0.4, 0.0), r, 0.5*o, a);
  float d  = min(d0, d1);

  return d;
}

float fTorus(vec3 p, float smallRadius, float largeRadius)
{
  return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}

float lettero(in vec3 p, float r)
{
  pR90(p.yz);
  p.x  /= 0.9;

  return fTorus(p, r, 6.0);
}

float letterc(in vec3 p, float r)
{
  pR90(p.yz);
  p.x  /= 0.9;

  float  b = sdBox(p - vec3(6.0, 0.0, 0.0), vec3(4.0 - r));
  float  t = fTorus(p, r, 6.0);

  return max(t, -b);
}

float letters(in vec3 p, float bb, float off, float r)
{
  float  w = letterw(p - vec3(0.0, 0.0, -off), r);
  pR90(p.xz);
  float  c = letterc(p - vec3(0.0, 0.0, -off), r);
  pR90(p.xz);
  float  o = lettero(p - vec3(0.0, 0.0, -off), r);
  pR90(p.xz);
  float  m = letterm(p - vec3(0.0, 0.0, -off), r);

  return max(min(min(w ,c), min(o ,m)), bb);
}

float logo(in vec3 p, out vec3 col, out float ref, out vec3 absorb)
{
  float dist = 1.8;
  float c2 = pMirror(p.z, 2.0*dist);
  float c1 = pMirror(p.z, dist);
  float c0 = pMirror(p.x, dist);
  float c  = c0 * c1 * c2;
  if (c > 0.0)
  {
    ref    = 0.9;
    col    = vec3(1.0);
    absorb = 5.0*vec3(3.0, 2.0, 1.0);
  }
  else
  {
    ref    = 0.3;
    col    = vec3(0.0);
    absorb = 5.0*vec3(3.0, 2.0, 1.0);
  }

  return sdRoundCube(p, 1.0);
}

float angleWcom(float time)
{
  time = max(time, 0.0);
  return smoothstep(0.0, 1.0, time)*3.0*PI/2.0;
}

float wcom(in vec3 p, out vec3 col, out float ref, out vec3 absorb)
{
  float time = iTime/3.0;
  pR(p.xy, 0.5*sin(time/5.0));
  pR(p.yz, 0.5*sin(time/7.0));
  pR(p.xz, -time/3.0);
  float bb = sdBox(p - vec3(0.0, 0.0, 0.0), vec3(1.2));
  float rc = sdRoundCube(p - vec3(0.0, 0.0, 0.0), 1.0);

  float s   = 0.1;
  float i   = logo((p - vec3(0.0, 1.11, 0.0))/s, col, ref, absorb)*s;
  float ils = letters(p/s, bb, 10.0, 1.5)*s;
  float ls  = letters(p/s, bb, 10.5, 1.0)*s;

  float d = rc;
  d = max(d, -ils);

  rc = d;

  d = min(d, ls);
  d = min(d, i);

  if (d == rc)
  {
    col = vec3(1.0);
    ref = 0.0;
    absorb = 5.0*vec3(3.0, 2.0, 1.0);
  }
  else if (d == ls)
  {
    ref    = 0.9;
    col    = vec3(1.0);
    absorb = 5.0*vec3(3.0, 2.0, 1.0);
  }


  return d;
}


float distanceField(in vec3 p, out vec3 col, out float ref, out vec3 absorb)
{
  return wcom(p, col, ref, absorb);
}

vec3 saturate(in vec3 a)   { return clamp(a, 0.0, 1.0); }
vec2 saturate(in vec2 a)   { return clamp(a, 0.0, 1.0); }
float saturate(in float a) { return clamp(a, 0.0, 1.0); }

const vec3 lightPos1 = 100.0*vec3(0.5,-2.0, 1.2);
const vec3 lightPos2 = 100.0*vec3(-0.5,-2.0, -1.2);

const vec3 lightCol1 = vec3(8.0/8.0,7.0/8.0,6.0/8.0);
const vec3 lightCol2 = vec3(8.0/8.0,6.0/8.0,7.0/8.0);

vec3 getSkyColor(vec3 rayDir)
{
  vec3 lightDir1 = normalize(lightPos1);
  vec3 lightDir2 = normalize(lightPos2);

  float ld1      = max(dot(lightDir1, rayDir), 0.0);
  float ld2      = max(dot(lightDir2, rayDir), 0.0);
  vec3 final     = vec3(0.125);

  if ((rayDir.y > abs(rayDir.x)*1.0) && (rayDir.y > abs(rayDir.z*0.25))) final = vec3(2.0)*rayDir.y;
  float roundBox = length(max(abs(rayDir.xz/max(0.0,rayDir.y))-vec2(0.9, 4.0),0.0))-0.1;
  final += vec3(0.8)* pow(saturate(1.0 - roundBox*0.5), 6.0);

  final += pow(lightCol1, vec3(2.0, 1.5, 1.5)) * pow(ld1, 8.0);
  final += lightCol1 * pow(ld1, 200.0);
  final += pow(lightCol2, vec3(2.0, 1.5, 1.5)) * pow(ld2, 6.0);
  final += lightCol2 * pow(ld2, 200.0);
  return final;
}

vec3 normal(in vec3 pos)
{
  vec3  eps = vec3(.001,0.0,0.0);
  vec3 col;
  float ref;
  vec3 nor;
  vec3 absorb;
  nor.x = distanceField(pos+eps.xyy, col, ref, absorb) - distanceField(pos-eps.xyy, col, ref, absorb);
  nor.y = distanceField(pos+eps.yxy, col, ref, absorb) - distanceField(pos-eps.yxy, col, ref, absorb);
  nor.z = distanceField(pos+eps.yyx, col, ref, absorb) - distanceField(pos-eps.yyx, col, ref, absorb);
  return normalize(nor);
}

float rayMarch(in float dmod, in vec3 ro, inout vec3 rd, float mint, float maxt, out int rep, out vec3 col, out float ref, out vec3 absorb)
{
  float t = mint;
  for (int i = 0; i < MAX_RAY_MARCHES; i++)
  {
    float distance_ = distanceField(ro + rd*t, col, ref, absorb);
    float distance = dmod*distance_;
    if (distance < TOLERANCE || t > maxt) break;
    t += max(distance, 0.001);
    rep = i;
  }
  return t;
}

float softShadow(in vec3 pos, in vec3 ld, in float ll, float mint, float k)
{
  const float minShadow = 0.25;
  float res = 1.0;
  float t = mint;
  vec3 col;
  float ref;
  vec3 absorb;
  for (int i=0; i<24; i++)
  {
    float distance = distanceField(pos + ld*t, col, ref, absorb);
    res = min(res, k*distance/t);
    if (ll <= t) break;
    if(res <= minShadow) break;
    t += max(mint*0.2, distance);
  }
  return clamp(res,minShadow,1.0);
}

vec3 postProcess(in vec3 col, in vec2 q)
{
  col=pow(clamp(col,0.0,1.0),vec3(0.45));
  col=col*0.6+0.4*col*col*(3.0-2.0*col);  // contrast
  col=mix(col, vec3(dot(col, vec3(0.33))), -0.4);  // satuation
  col*=0.5+0.5*pow(19.0*q.x*q.y*(1.0-q.x)*(1.0-q.y),0.7);  // vigneting
  return col;
}



vec3 render(in vec3 ro, in vec3 rd)
{
  vec3 lightPos = 2.0*vec3(1.5, 3.0, 1.0);

  vec3 col    = vec3(0.0);

  vec3 ragg2 = vec3(1.0);

  float tdist = 0.0;

  const float refraction = 0.9;

  bool inside = false;

  for (int i = 0; i < MAX_BOUNCES; ++i)
  {
    if (maxComp(ragg2) <  0.01) break;
    float dmod  = inside ? -1.0 : 1.0;
    vec3 absorb = vec3(3.0, 2.0, 1.0);
    vec3 mat    = vec3(1.0);
    float rscale= 0.9;
    int rep     = 0;
    float t     = rayMarch(dmod, ro, rd, 0.01, MAX_RAY_LENGTH, rep, mat, rscale, absorb);
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
      col += ragg2*getSkyColor(rd);
      break;
    }

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
    // TODO: Rework shadow to "work" with transparent objects
    float sha = 1.0;
    if (!inside)
    {
      sha = softShadow(pos, ld, ll, 0.01, 64.0);
    }

    float dif = max(dot(nor,ld),0.0);
    float occ = 1.0 - float(rep)/float(MAX_RAY_MARCHES);
    float l   = dif*occ*sha;

    float lin = mix(0.2, 1.0, l);

    vec3 sky  = getSkyColor(refl);
    mat *= (0.7 + 0.3*abs(nor.zxy));
    vec3 mcol = 0.8*lin*mat + 0.2*sky;

    vec3 beer = vec3(1.0);

    if (inside)
    {
      beer = exp(-absorb*t);
    }
    col        += (1.0 - rscale)*ragg2*beer*mcol;
    ragg2      *= rscale*beer;

    ro        = pos;

    if (refr == vec3(0.0))
    {
       rd = refl;
    }
    else
    {
      rd = refr;
    }

    if (dot(refl, rd) < 0.9)
    {
      inside = !inside;
    }
  }


  return col;
}

vec3 getSample(in vec2 p)
{
  // camera

  float time = iTime;

  vec3 ro  = vec3(1.0, 6.0, 0.0);
  vec3 la  = vec3(0.0);

  pR(ro.xz, time/3.0);
  pR(ro.xy, 0.35*sin(time/5.0));
  pR(ro.yz, 0.45*sin(time/7.0));


  vec3 ww = normalize(la - ro);
  vec3 uu = normalize(cross(vec3(0.0,0.0,1.0), ww ));
  vec3 vv = normalize(cross(ww,uu));
  vec3 rd = normalize( p.x*uu + p.y*vv + 2.5*ww );

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


  float fadeIn = smoothstep(0.0, 3.0, iTime);

  fragColor = vec4(postProcess(col, q)*fadeIn, 1.0);

}

// -----------------------------------------------------------------------------
)SHADER";
}