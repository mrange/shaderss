#include "stdafx.h"

#include <algorithm>
#include <cwchar>

#include "config.hpp"

namespace
{
  wchar_t const hkey__path[]        = LR"*(Software\mrange\shaderss)*";
  wchar_t const license__cc0[]      = L"CC0 1.0 Universal (CC0 1.0) Public Domain Dedication";
  wchar_t const license__cc3[]      = L"Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.";
  wchar_t const license__cc_nc[]    = L"Creative Commons Non-commercial (NC) License.";
  wchar_t const license__unknown[]  = L"Unknown - please contact me if you object having the shader as part of this collection";
  char const shader__4dj3Wy__mandelbrot_with_orbit_traps[] = R"SHADER(
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

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
)SHADER";

  char const shader__Mld3Rn__perspex_web_lattice[] = R"SHADER(
/*
	Perspex Web Lattice
	-------------------

	I felt that Shadertoy didn't have enough Voronoi examples, so I made another one. :) I'm
	not exactly sure what it's supposed to be... My best guess it that an Alien race with no
	common sense designed a monitor system with physics defying materials. :)

	Technically speaking, there's not much to it. It's just some raymarched 2nd order Voronoi.
	The dark perspex-looking web lattice is created by manipulating the Voronoi value slightly
	and giving the effected region an ID value so as to color it differently, but that's about
	it. The details are contained in the "heightMap" function.

	There's also some subtle edge detection in order to give the example a slight comic look.
	3D geometric edge detection doesn't really differ a great deal in concept from 2D pixel
	edge detection, but it obviously involves more processing power. However, it's possible to
	combine the edge detection with the normal calculation and virtually get it for free. Kali
	uses it to great effect in his "Fractal Land" example. It's also possible to do a
	tetrahedral version... I think Nimitz and some others may have done it already. Anyway,
	you can see how it's done in the "nr" (normal) function.

	Geometric edge related examples:

	Fractal Land - Kali
	https://www.shadertoy.com/view/XsBXWt

	Rotating Cubes - Shau
	https://www.shadertoy.com/view/4sGSRc

	Voronoi mesh related:

    // I haven't really looked into this, but it's interesting.
	Weaved Voronoi - FabriceNeyret2
    https://www.shadertoy.com/view/ltsXRM

*/

#define FAR 2.

int id = 0; // Object ID - Red perspex: 0; Black lattice: 1.


// Tri-Planar blending function. Based on an old Nvidia writeup:
// GPU Gems 3 - Ryan Geiss: https://developer.nvidia.com/gpugems/GPUGems3/gpugems3_ch01.html
vec3 tex3D( sampler2D tex, in vec3 p, in vec3 n ){

    n = max((abs(n) - .2), .001);
    n /= (n.x + n.y + n.z ); // Roughly normalized.

	p = (texture(tex, p.yz)*n.x + texture(tex, p.zx)*n.y + texture(tex, p.xy)*n.z).xyz;

    // Loose sRGB to RGB conversion to counter final value gamma correction...
    // in case you're wondering.
    return p*p;
}


// Compact, self-contained version of IQ's 3D value noise function. I have a transparent noise
// example that explains it, if you require it.
float n3D(vec3 p){

	const vec3 s = vec3(7, 157, 113);
	vec3 ip = floor(p); p -= ip;
    vec4 h = vec4(0., s.yz, s.y + s.z) + dot(ip, s);
    p = p*p*(3. - 2.*p); //p *= p*p*(p*(p * 6. - 15.) + 10.);
    h = mix(fract(sin(h)*43758.5453), fract(sin(h + s.x)*43758.5453), p.x);
    h.xy = mix(h.xz, h.yw, p.y);
    return mix(h.x, h.y, p.z); // Range: [0, 1].
}

// vec2 to vec2 hash.
vec2 hash22(vec2 p) {

    // Faster, but doesn't disperse things quite as nicely. However, when framerate
    // is an issue, and it often is, this is a good one to use. Basically, it's a tweaked
    // amalgamation I put together, based on a couple of other random algorithms I've
    // seen around... so use it with caution, because I make a tonne of mistakes. :)
    float n = sin(dot(p, vec2(41, 289)));
    //return fract(vec2(262144, 32768)*n);

    // Animated.
    p = fract(vec2(262144, 32768)*n);
    // Note the ".45," insted of ".5" that you'd expect to see. When edging, it can open
    // up the cells ever so slightly for a more even spread. In fact, lower numbers work
    // even better, but then the random movement would become too restricted. Zero would
    // give you square cells.
    return sin( p*6.2831853 + iTime )*.45 + .5;

}

// 2D 2nd-order Voronoi: Obviously, this is just a rehash of IQ's original. I've tidied
// up those if-statements. Since there's less writing, it should go faster. That's how
// it works, right? :)
//
float Voronoi(in vec2 p){

	vec2 g = floor(p), o; p -= g;

	vec3 d = vec3(1); // 1.4, etc. "d.z" holds the distance comparison value.

	for(int y = -1; y <= 1; y++){
		for(int x = -1; x <= 1; x++){

			o = vec2(x, y);
            o += hash22(g + o) - p;

			d.z = dot(o, o);
            // More distance metrics.
            //o = abs(o);
            //d.z = max(o.x*.8666 + o.y*.5, o.y);//
            //d.z = max(o.x, o.y);
            //d.z = (o.x*.7 + o.y*.7);

            d.y = max(d.x, min(d.y, d.z));
            d.x = min(d.x, d.z);

		}
	}

    return max(d.y/1.2 - d.x*1., 0.)/1.2;
    //return d.y - d.x; // return 1.-d.x; // etc.

}

// The height map values. In this case, it's just a Voronoi variation. By the way, I could
// optimize this a lot further, but it's not a particularly taxing distance function, so
// I've left it in a more readable state.
float heightMap(vec3 p){

    id =0;
    float c = Voronoi(p.xy*4.); // The fiery bit.

    // For lower values, reverse the surface direction, smooth, then
    // give it an ID value of one. Ie: this is the black web-like
    // portion of the surface.
    if (c<.07) {c = smoothstep(0.7, 1., 1.-c)*.2; id = 1; }

    return c;
}

// Standard back plane height map. Put the plane at vec3(0, 0, 1), then add some height values.
// Obviously, you don't want the values to be too large. The one's here account for about 10%
// of the distance between the plane and the camera.
float m(vec3 p){

    float h = heightMap(p); // texture(iChannel0, p.xy/2.).x; // Texture work too.

    return 1. - p.z - h*.1;

}

/*
// Tetrahedral normal, to save a couple of "map" calls. Courtesy of IQ.
vec3 nr(in vec3 p){

    // Note the slightly increased sampling distance, to alleviate artifacts due to hit point inaccuracies.
    vec2 e = vec2(0.005, -0.005);
    return normalize(e.xyy * m(p + e.xyy) + e.yyx * m(p + e.yyx) + e.yxy * m(p + e.yxy) + e.xxx * m(p + e.xxx));
}
*/

/*
// Standard normal function - for comparison with the one below.
vec3 nr(in vec3 p) {
	const vec2 e = vec2(0.005, 0);
	return normalize(vec3(m(p + e.xyy) - m(p - e.xyy), m(p + e.yxy) - m(p - e.yxy),	m(p + e.yyx) - m(p - e.yyx)));
}
*/

// The normal function with some edge detection rolled into it.
vec3 nr(vec3 p, inout float edge) {

    vec2 e = vec2(.005, 0);

    // Take some distance function measurements from either side of the hit point on all three axes.
	float d1 = m(p + e.xyy), d2 = m(p - e.xyy);
	float d3 = m(p + e.yxy), d4 = m(p - e.yxy);
	float d5 = m(p + e.yyx), d6 = m(p - e.yyx);
	float d = m(p)*2.;	// The hit point itself - Doubled to cut down on calculations. See below.

    // Edges - Take a geometry measurement from either side of the hit point. Average them, then see how
    // much the value differs from the hit point itself. Do this for X, Y and Z directions. Here, the sum
    // is used for the overall difference, but there are other ways. Note that it's mainly sharp surface
    // curves that register a discernible difference.
    edge = abs(d1 + d2 - d) + abs(d3 + d4 - d) + abs(d5 + d6 - d);
    //edge = max(max(abs(d1 + d2 - d), abs(d3 + d4 - d)), abs(d5 + d6 - d)); // Etc.

    // Once you have an edge value, it needs to normalized, and smoothed if possible. How you
    // do that is up to you. This is what I came up with for now, but I might tweak it later.
    edge = smoothstep(0., 1., sqrt(edge/e.x*2.));

    // Return the normal.
    // Standard, normalized gradient mearsurement.
    return normalize(vec3(d1 - d2, d3 - d4, d5 - d6));
}

/*
// I keep a collection of occlusion routines... OK, that sounded really nerdy. :)
// Anyway, I like this one. I'm assuming it's based on IQ's original.
float cAO(in vec3 p, in vec3 n)
{
	float sca = 3., occ = 0.;
    for(float i=0.; i<5.; i++){

        float hr = .01 + i*.5/4.;
        float dd = m(n * hr + p);
        occ += (hr - dd)*sca;
        sca *= 0.7;
    }
    return clamp(1.0 - occ, 0., 1.);
}
*/

/*
// Standard hue rotation formula... compacted down a bit.
vec3 rotHue(vec3 p, float a){

    vec2 cs = sin(vec2(1.570796, 0) + a);

    mat3 hr = mat3(0.299,  0.587,  0.114,  0.299,  0.587,  0.114,  0.299,  0.587,  0.114) +
        	  mat3(0.701, -0.587, -0.114, -0.299,  0.413, -0.114, -0.300, -0.588,  0.886) * cs.x +
        	  mat3(0.168,  0.330, -0.497, -0.328,  0.035,  0.292,  1.250, -1.050, -0.203) * cs.y;

    return clamp(p*hr, 0., 1.);
}
*/

// Simple environment mapping. Pass the reflected vector in and create some
// colored noise with it. The normal is redundant here, but it can be used
// to pass into a 3D texture mapping function to produce some interesting
// environmental reflections.
//
// More sophisticated environment mapping:
// UI easy to integrate - XT95
// https://www.shadertoy.com/view/ldKSDm
vec3 eMap(vec3 rd, vec3 sn){

    vec3 sRd = rd; // Save rd, just for some mixing at the end.

    // Add a time component, scale, then pass into the noise function.
    rd.xy -= iTime*.25;
    rd *= 3.;

    //vec3 tx = tex3D(iChannel0, rd/3., sn);
    //float c = dot(tx*tx, vec3(.299, .587, .114));

    float c = n3D(rd)*.57 + n3D(rd*2.)*.28 + n3D(rd*4.)*.15; // Noise value.
    c = smoothstep(0.5, 1., c); // Darken and add contast for more of a spotlight look.

    //vec3 col = vec3(c, c*c, c*c*c*c).zyx; // Simple, warm coloring.
    vec3 col = vec3(min(c*1.5, 1.), pow(c, 2.5), pow(c, 12.)).zyx; // More color.

    // Mix in some more red to tone it down and return.
    return mix(col, col.yzx, sRd*.25+.25);

}

void mainImage(out vec4 c, vec2 u){

    // Unit direction ray, camera origin and light position.
    vec3 r = normalize(vec3(u - iResolution.xy*.5, iResolution.y)),
         o = vec3(0), l = o + vec3(0, 0, -1);

    // Rotate the canvas. Note that sine and cosine are kind of rolled into one.
    vec2 a = sin(vec2(1.570796, 0) + iTime/8.); // Fabrice's observation.
    r.xy = mat2(a, -a.y, a.x) * r.xy;


    // Standard raymarching routine. Raymarching a slightly perturbed back plane front-on
    // doesn't usually require many iterations. Unless you rely on your GPU for warmth,
    // this is a good thing. :)
    float d, t = 0.;

    for(int i=0; i<32;i++){

        d = m(o + r*t);
        // There isn't really a far plane to go beyond, but it's there anyway.
        if(abs(d)<0.001 || t>FAR) break;
        t += d*.7;

    }

    t = min(t, FAR);

    // Set the initial scene color to black.
    c = vec4(0);

    float edge = 0.; // Edge value - to be passed into the normal.

    if(t<FAR){

        vec3 p = o + r*t, n = nr(p, edge);

        l -= p; // Light to surface vector. Ie: Light direction vector.
        d = max(length(l), 0.001); // Light to surface distance.
        l /= d; // Normalizing the light direction vector.



        // Obtain the height map (destorted Voronoi) value, and use it to slightly
        // shade the surface. Gives a more shadowy appearance.
        float hm = heightMap(p);

        // Texture value at the surface. Use the heighmap value above to distort the
        // texture a bit.
        vec3 tx = tex3D(iChannel0, (p*2. + hm*.2), n);
        //tx = floor(tx*15.999)/15.; // Quantized cartoony colors, if you get bored enough.

        c.xyz = vec3(1.)*(hm*.8 + .2); // Applying the shading to the final color.

        c.xyz *= vec3(1.5)*tx; // Multiplying by the texture value and lightening.


        // Color the cell part with a fiery (I incorrectly spell it firey all the time)
        // palette and the latticey web thing a very dark color.
        //
        c.x = dot(c.xyz, vec3(.299, .587, .114)); // Grayscale.
        if (id==0) c.xyz *= vec3(min(c.x*1.5, 1.), pow(c.x, 5.), pow(c.x, 24.))*2.;
        else c.xyz *= .1;

        // Hue rotation, for anyone who's interested.
        //c.xyz = rotHue(c.xyz, mod(iTime/16., 6.283));


        float df = max(dot(l, n), 0.); // Diffuse.
        float sp = pow(max(dot(reflect(-l, n), -r), 0.), 32.); // Specular.

        if(id == 1) sp *= sp; // Increase specularity on the dark lattice.

		// Applying some diffuse and specular lighting to the surface.
        c.xyz = c.xyz*(df + .75) + vec3(1, .97, .92)*sp + vec3(.5, .7, 1)*pow(sp, 32.);

        // Add the fake environmapping. Give the dark surface less reflectivity.
        vec3 em = eMap(reflect(r, n), n); // Fake environment mapping.
        if(id == 1) em *= .5;
        c.xyz += em;

        // Edges.
        //if(id == 0)c.xyz += edge*.1; // Lighter edges.
        c.xyz *= 1. - edge*.8; // Darker edges.

        // Attenuation, based on light to surface distance.
        c.xyz *= 1./(1. + d*d*.125);

        // AO - The effect is probably too subtle, in this case, so we may as well
        // save some cycles.
        //c.xyz *= cAO(p, n);

    }


    // Vignette.
    //vec2 uv = u/iResolution.xy;
    //c.xyz = mix(c.xyz, vec3(0, 0, .5), .1 -pow(16.*uv.x*uv.y*(1.-uv.x)*(1.-uv.y), 0.25)*.1);

    // Apply some statistically unlikely (but close enough) 2.0 gamma correction. :)
    c = vec4(sqrt(clamp(c.xyz, 0., 1.)), 1.);


}
)SHADER";

  char const shader__lldBD8__transparent_crystal[] = R"SHADER(
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
)SHADER";

  char const shader__4tdSWr__2d_clouds[] = R"SHADER(
const float cloudscale = 1.1;
const float speed = 0.03;
const float clouddark = 0.5;
const float cloudlight = 0.3;
const float cloudcover = 0.2;
const float cloudalpha = 8.0;
const float skytint = 0.5;
const vec3 skycolour1 = vec3(0.2, 0.4, 0.6);
const vec3 skycolour2 = vec3(0.4, 0.7, 1.0);

const mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );

vec2 hash( vec2 p ) {
	p = vec2(dot(p,vec2(127.1,311.7)), dot(p,vec2(269.5,183.3)));
	return -1.0 + 2.0*fract(sin(p)*43758.5453123);
}

float noise( in vec2 p ) {
    const float K1 = 0.366025404; // (sqrt(3)-1)/2;
    const float K2 = 0.211324865; // (3-sqrt(3))/6;
	vec2 i = floor(p + (p.x+p.y)*K1);
    vec2 a = p - i + (i.x+i.y)*K2;
    vec2 o = (a.x>a.y) ? vec2(1.0,0.0) : vec2(0.0,1.0); //vec2 of = 0.5 + 0.5*vec2(sign(a.x-a.y), sign(a.y-a.x));
    vec2 b = a - o + K2;
	vec2 c = a - 1.0 + 2.0*K2;
    vec3 h = max(0.5-vec3(dot(a,a), dot(b,b), dot(c,c) ), 0.0 );
	vec3 n = h*h*h*h*vec3( dot(a,hash(i+0.0)), dot(b,hash(i+o)), dot(c,hash(i+1.0)));
    return dot(n, vec3(70.0));
}

float fbm(vec2 n) {
	float total = 0.0, amplitude = 0.1;
	for (int i = 0; i < 7; i++) {
		total += noise(n) * amplitude;
		n = m * n;
		amplitude *= 0.4;
	}
	return total;
}

// -----------------------------------------------

void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
    vec2 p = fragCoord.xy / iResolution.xy;
	vec2 uv = p*vec2(iResolution.x/iResolution.y,1.0);
    float time = iTime * speed;
    float q = fbm(uv * cloudscale * 0.5);

    //ridged noise shape
	float r = 0.0;
	uv *= cloudscale;
    uv -= q - time;
    float weight = 0.8;
    for (int i=0; i<8; i++){
		r += abs(weight*noise( uv ));
        uv = m*uv + time;
		weight *= 0.7;
    }

    //noise shape
	float f = 0.0;
    uv = p*vec2(iResolution.x/iResolution.y,1.0);
	uv *= cloudscale;
    uv -= q - time;
    weight = 0.7;
    for (int i=0; i<8; i++){
		f += weight*noise( uv );
        uv = m*uv + time;
		weight *= 0.6;
    }

    f *= r + f;

    //noise colour
    float c = 0.0;
    time = iTime * speed * 2.0;
    uv = p*vec2(iResolution.x/iResolution.y,1.0);
	uv *= cloudscale*2.0;
    uv -= q - time;
    weight = 0.4;
    for (int i=0; i<7; i++){
		c += weight*noise( uv );
        uv = m*uv + time;
		weight *= 0.6;
    }

    //noise ridge colour
    float c1 = 0.0;
    time = iTime * speed * 3.0;
    uv = p*vec2(iResolution.x/iResolution.y,1.0);
	uv *= cloudscale*3.0;
    uv -= q - time;
    weight = 0.4;
    for (int i=0; i<7; i++){
		c1 += abs(weight*noise( uv ));
        uv = m*uv + time;
		weight *= 0.6;
    }

    c += c1;

    vec3 skycolour = mix(skycolour2, skycolour1, p.y);
    vec3 cloudcolour = vec3(1.1, 1.1, 0.9) * clamp((clouddark + cloudlight*c), 0.0, 1.0);

    f = cloudcover + cloudalpha*f*r;

    vec3 result = mix(skycolour, clamp(skytint * skycolour + cloudcolour, 0.0, 1.0), clamp(f + c, 0.0, 1.0));

	fragColor = vec4( result, 1.0 );
}
)SHADER";

  char const shader__XljGDz__protosphere[] = R"SHADER(
/*--------------------------------------------------------------------------------------
License CC0 - http://creativecommons.org/publicdomain/zero/1.0/
To the extent possible under law, the author(s) have dedicated all copyright and related and neighboring rights to this software to the public domain worldwide. This software is distributed without any warranty.
----------------------------------------------------------------------------------------
^This means do anything you want with this code. Because we are programmers, not lawyers.

-Otavio Good
*/

// Number of times the fractal repeats
#define RECURSION_LEVELS 4
// Animation splits the sphere in different directions
// This ended up running a significantly slower fps and not looking very different. :(
//#define SPLIT_ANIM

float localTime = 0.0;
float marchCount;

float PI=3.14159265;

vec3 saturate(vec3 a) { return clamp(a, 0.0, 1.0); }
vec2 saturate(vec2 a) { return clamp(a, 0.0, 1.0); }
float saturate(float a) { return clamp(a, 0.0, 1.0); }

vec3 RotateX(vec3 v, float rad)
{
  float cos = cos(rad);
  float sin = sin(rad);
  return vec3(v.x, cos * v.y + sin * v.z, -sin * v.y + cos * v.z);
}
vec3 RotateY(vec3 v, float rad)
{
  float cos = cos(rad);
  float sin = sin(rad);
  return vec3(cos * v.x - sin * v.z, v.y, sin * v.x + cos * v.z);
}
vec3 RotateZ(vec3 v, float rad)
{
  float cos = cos(rad);
  float sin = sin(rad);
  return vec3(cos * v.x + sin * v.y, -sin * v.x + cos * v.y, v.z);
}


/*vec3 GetEnvColor(vec3 rayDir, vec3 sunDir)
{
	vec3 tex = texture(iChannel0, rayDir).xyz;
	tex = tex * tex;	// gamma correct
    return tex;
}*/

// This is a procedural environment map with a giant overhead softbox,
// 4 lights in a horizontal circle, and a bottom-to-top fade.
vec3 GetEnvColor2(vec3 rayDir, vec3 sunDir)
{
    // fade bottom to top so it looks like the softbox is casting light on a floor
    // and it's bouncing back
    vec3 final = vec3(1.0) * dot(-rayDir, sunDir) * 0.5 + 0.5;
    final *= 0.125;
    // overhead softbox, stretched to a rectangle
    if ((rayDir.y > abs(rayDir.x)*1.0) && (rayDir.y > abs(rayDir.z*0.25))) final = vec3(2.0)*rayDir.y;
    // fade the softbox at the edges with a rounded rectangle.
    float roundBox = length(max(abs(rayDir.xz/max(0.0,rayDir.y))-vec2(0.9, 4.0),0.0))-0.1;
    final += vec3(0.8)* pow(saturate(1.0 - roundBox*0.5), 6.0);
    // purple lights from side
    final += vec3(8.0,6.0,7.0) * saturate(0.001/(1.0 - abs(rayDir.x)));
    // yellow lights from side
    final += vec3(8.0,7.0,6.0) * saturate(0.001/(1.0 - abs(rayDir.z)));
    return vec3(final);
}

/*vec3 GetEnvColorReflection(vec3 rayDir, vec3 sunDir, float ambient)
{
	vec3 tex = texture(iChannel0, rayDir).xyz;
	tex = tex * tex;
    vec3 texBack = texture(iChannel0, rayDir).xyz;
    vec3 texDark = pow(texBack, vec3(50.0)).zzz;	// fake hdr texture
    texBack += texDark*0.5 * ambient;
    return texBack*texBack*texBack;
}*/

vec3 camPos = vec3(0.0), camFacing;
vec3 camLookat=vec3(0,0.0,0);

// polynomial smooth min (k = 0.1);
float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

vec2 matMin(vec2 a, vec2 b)
{
	if (a.x < b.x) return a;
	else return b;
}

float spinTime;
vec3 diagN = normalize(vec3(-1.0));
float cut = 0.77;
float inner = 0.333;
float outness = 1.414;
float finWidth;
float teeth;
float globalTeeth;

vec2 sphereIter(vec3 p, float radius, float subA)
{
    finWidth = 0.1;
    teeth = globalTeeth;
    float blender = 0.25;
    vec2 final = vec2(1000000.0, 0.0);
    for (int i = 0; i < RECURSION_LEVELS; i++)
    {
#ifdef SPLIT_ANIM
        // rotate top and bottom of sphere opposite directions
        p = RotateY(p, spinTime*sign(p.y)*0.05/blender);
#endif
        // main sphere
        float d = length(p) - radius*outness;
#ifdef SPLIT_ANIM
        // subtract out disc at the place where rotation happens so we don't have artifacts
        d = max(d, -(max(length(p) - radius*outness + 0.1, abs(p.y) - finWidth*0.25)));
#endif

        // calc new position at 8 vertices of cube, scaled
        vec3 corners = abs(p) + diagN * radius;
        float lenCorners = length(corners);
        // subtract out main sphere hole, mirrored on all axises
        float subtracter = lenCorners - radius * subA;
        // make mirrored fins that go through all vertices of the cube
        vec3 ap = abs(-p) * 0.7071;	// 1/sqrt(2) to keep distance field normalized
        subtracter = max(subtracter, -(abs(ap.x-ap.y) - finWidth));
        subtracter = max(subtracter, -(abs(ap.y-ap.z) - finWidth));
        subtracter = max(subtracter, -(abs(ap.z-ap.x) - finWidth));
        // subtract sphere from fins so they don't intersect the inner spheres.
        // also animate them so they are like teeth
        subtracter = min(subtracter, lenCorners - radius * subA + teeth);
        // smoothly subtract out that whole complex shape
        d = -smin(-d, subtracter, blender);
        //vec2 sphereDist = sphereB(abs(p) + diagN * radius, radius * inner, cut);	// recurse
        // do a material-min with the last iteration
        final = matMin(final, vec2(d, float(i)));

#ifndef SPLIT_ANIM
        corners = RotateY(corners, spinTime*0.25/blender);
#endif
        // Simple rotate 90 degrees on X axis to keep things fresh
        p = vec3(corners.x, corners.z, -corners.y);
        // Scale things for the next iteration / recursion-like-thing
        radius *= inner;
        teeth *= inner;
        finWidth *= inner;
        blender *= inner;
    }
    // Bring in the final smallest-sized sphere
    float d = length(p) - radius*outness;
    final = matMin(final, vec2(d, 6.0));
    return final;
}

vec2 DistanceToObject(vec3 p)
{
    vec2 distMat = sphereIter(p, 5.2 / outness, cut);
    return distMat;
}

// dirVec MUST BE NORMALIZED FIRST!!!!
float SphereIntersect(vec3 pos, vec3 dirVecPLZNormalizeMeFirst, vec3 spherePos, float rad)
{
    vec3 radialVec = pos - spherePos;
    float b = dot(radialVec, dirVecPLZNormalizeMeFirst);
    float c = dot(radialVec, radialVec) - rad * rad;
    float h = b * b - c;
    if (h < 0.0) return -1.0;
    return -b - sqrt(h);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    localTime = iTime - 0.0;
	// ---------------- First, set up the camera rays for ray marching ----------------
	vec2 uv = fragCoord.xy/iResolution.xy * 2.0 - 1.0;
    float zoom = 1.7;
    uv /= zoom;

	// Camera up vector.
	vec3 camUp=vec3(0,1,0);

	// Camera lookat.
	camLookat=vec3(0,0.0,0);

    // debugging camera
    float mx=iMouse.x/iResolution.x*PI*2.0-0.7 + localTime*3.1415 * 0.0625*0.666;
	float my=-iMouse.y/iResolution.y*10.0 - sin(localTime * 0.31)*0.5;//*PI/2.01;
	camPos += vec3(cos(my)*cos(mx),sin(my),cos(my)*sin(mx))*(12.2);

	// Camera setup.
	vec3 camVec=normalize(camLookat - camPos);
	vec3 sideNorm=normalize(cross(camUp, camVec));
	vec3 upNorm=cross(camVec, sideNorm);
	vec3 worldFacing=(camPos + camVec);
	vec3 worldPix = worldFacing + uv.x * sideNorm * (iResolution.x/iResolution.y) + uv.y * upNorm;
	vec3 rayVec = normalize(worldPix - camPos);

	// ----------------------------------- Animate ------------------------------------
    localTime = iTime*0.5;
    // This is a wave function like a triangle wave, but with flat tops and bottoms.
    // period is 1.0
    float rampStep = min(3.0,max(1.0, abs((fract(localTime)-0.5)*1.0)*8.0))*0.5-0.5;
    rampStep = smoothstep(0.0, 1.0, rampStep);
    // lopsided triangle wave - goes up for 3 time units, down for 1.
    float step31 = (max(0.0, (fract(localTime+0.125)-0.25)) - min(0.0,(fract(localTime+0.125)-0.25))*3.0)*0.333;

    spinTime = step31 + localTime;
    //globalTeeth = 0.0 + max(0.0, sin(localTime*3.0))*0.9;
    globalTeeth = rampStep*0.99;
    cut = max(0.48, min(0.77, localTime));
	// --------------------------------------------------------------------------------
	vec2 distAndMat = vec2(0.5, 0.0);
	float t = 0.0;
	//float inc = 0.02;
	float maxDepth = 24.0;
	vec3 pos = vec3(0,0,0);
    marchCount = 0.0;
    // intersect with sphere first as optimization so we don't ray march more than is needed.
    float hit = SphereIntersect(camPos, rayVec, vec3(0.0), 5.6);
    if (hit >= 0.0)
    {
        t = hit;
        // ray marching time
        for (int i = 0; i < 290; i++)	// This is the count of the max times the ray actually marches.
        {
            pos = camPos + rayVec * t;
            // *******************************************************
            // This is _the_ function that defines the "distance field".
            // It's really what makes the scene geometry.
            // *******************************************************
            distAndMat = DistanceToObject(pos);
            // adjust by constant because deformations mess up distance function.
            t += distAndMat.x * 0.7;
            //if (t > maxDepth) break;
            if ((t > maxDepth) || (abs(distAndMat.x) < 0.0025)) break;
            marchCount+= 1.0;
        }
    }
    else
    {
        t = maxDepth + 1.0;
        distAndMat.x = 1000000.0;
    }
    // --------------------------------------------------------------------------------
	// Now that we have done our ray marching, let's put some color on this geometry.

	vec3 sunDir = normalize(vec3(3.93, 10.82, -1.5));
	vec3 finalColor = vec3(0.0);

	// If a ray actually hit the object, let's light it.
	//if (abs(distAndMat.x) < 0.75)
    if (t <= maxDepth)
	{
        // calculate the normal from the distance field. The distance field is a volume, so if you
        // sample the current point and neighboring points, you can use the difference to get
        // the normal.
        vec3 smallVec = vec3(0.005, 0, 0);
        vec3 normalU = vec3(distAndMat.x - DistanceToObject(pos - smallVec.xyy).x,
                           distAndMat.x - DistanceToObject(pos - smallVec.yxy).x,
                           distAndMat.x - DistanceToObject(pos - smallVec.yyx).x);

        vec3 normal = normalize(normalU);

        // calculate 2 ambient occlusion values. One for global stuff and one
        // for local stuff
        float ambientS = 1.0;
        ambientS *= saturate(DistanceToObject(pos + normal * 0.1).x*10.0);
        ambientS *= saturate(DistanceToObject(pos + normal * 0.2).x*5.0);
        ambientS *= saturate(DistanceToObject(pos + normal * 0.4).x*2.5);
        ambientS *= saturate(DistanceToObject(pos + normal * 0.8).x*1.25);
        float ambient = ambientS * saturate(DistanceToObject(pos + normal * 1.6).x*1.25*0.5);
        ambient *= saturate(DistanceToObject(pos + normal * 3.2).x*1.25*0.25);
        ambient *= saturate(DistanceToObject(pos + normal * 6.4).x*1.25*0.125);
        ambient = max(0.035, pow(ambient, 0.3));	// tone down ambient with a pow and min clamp it.
        ambient = saturate(ambient);

        // calculate the reflection vector for highlights
        vec3 ref = reflect(rayVec, normal);
        ref = normalize(ref);

        // Trace a ray for the reflection
        float sunShadow = 1.0;
        float iter = 0.1;
        vec3 nudgePos = pos + normal*0.02;	// don't start tracing too close or inside the object
		for (int i = 0; i < 40; i++)
        {
            float tempDist = DistanceToObject(nudgePos + ref * iter).x;
	        sunShadow *= saturate(tempDist*50.0);
            if (tempDist <= 0.0) break;
            //iter *= 1.5;	// constant is more reliable than distance-based
            iter += max(0.00, tempDist)*1.0;
            if (iter > 4.2) break;
        }
        sunShadow = saturate(sunShadow);

        // ------ Calculate texture color ------
        vec3 texColor;
        texColor = vec3(1.0);// vec3(0.65, 0.5, 0.4)*0.1;
        texColor = vec3(0.85, 0.945 - distAndMat.y * 0.15, 0.93 + distAndMat.y * 0.35)*0.951;
        if (distAndMat.y == 6.0) texColor = vec3(0.91, 0.1, 0.41)*10.5;
        //texColor *= mix(vec3(0.3), vec3(1.0), tex3d(pos*0.5, normal).xxx);
        texColor = max(texColor, vec3(0.0));
        texColor *= 0.25;

        // ------ Calculate lighting color ------
        // Start with sun color, standard lighting equation, and shadow
        vec3 lightColor = vec3(0.0);// sunCol * saturate(dot(sunDir, normal)) * sunShadow*14.0;
        // sky color, hemisphere light equation approximation, ambient occlusion
        lightColor += vec3(0.1,0.35,0.95) * (normal.y * 0.5 + 0.5) * ambient * 0.2;
        // ground color - another hemisphere light
        lightColor += vec3(1.0) * ((-normal.y) * 0.5 + 0.5) * ambient * 0.2;


        // finally, apply the light to the texture.
        finalColor = texColor * lightColor;
        //if (distAndMat.y == ceil(mod(localTime, 4.0))) finalColor += vec3(0.0, 0.41, 0.72)*0.925;

        // reflection environment map - this is most of the light
        vec3 refColor = GetEnvColor2(ref, sunDir)*sunShadow;
        finalColor += refColor * 0.35 * ambient;// * sunCol * sunShadow * 9.0 * texColor.g;

        // fog
		finalColor = mix(vec3(1.0, 0.41, 0.41) + vec3(1.0), finalColor, exp(-t*0.0007));
        // visualize length of gradient of distance field to check distance field correctness
        //finalColor = vec3(0.5) * (length(normalU) / smallVec.x);
	}
    else
    {
	    finalColor = GetEnvColor2(rayVec, sunDir);// + vec3(0.1, 0.1, 0.1);
    }
    //finalColor += marchCount * vec3(1.0, 0.3, 0.91) * 0.001;

    // vignette?
    //finalColor *= vec3(1.0) * saturate(1.0 - length(uv/2.5));
    //finalColor *= 1.95;

	// output the final color with sqrt for "gamma correction"
	fragColor = vec4(sqrt(clamp(finalColor, 0.0, 1.0)),1.0);
}
)SHADER";

  char const shader__XdcfR8__fractal_thingy_flythrough[] = R"SHADER(
#define PI 3.14159265359
#define rot(a) mat2(cos(a+PI*vec4(0,1.5,0.5,0)))
#define SCALE 4.0
#define FOV 1.0

//f (x)=sin(a*x)*b
//f'(x)=a*b*cos(a*x)
#define PATHA vec2(0.1147, 0.2093)
#define PATHB vec2(13.0, 3.0)
vec3 camPath( float z ) {
    return vec3(sin(z*PATHA)*PATHB, z);
}
vec3 camPathDeriv( float z ) {
    return vec3(PATHA*PATHB*cos(PATHA*z), 1.0);
}

float sdBox( in vec3 p, in vec3 b, in float r, out vec3 color ) {
   	vec3 d = abs(p) - b;
    color = normalize(smoothstep(vec3(-r), vec3(0.0), d));
	return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
}

float de( in vec3 p, in float r, out vec3 color ) {

    // wrap world around camera path
    vec3 wrap = camPath(p.z);
    vec3 wrapDeriv = normalize(camPathDeriv(p.z));
    p.xy -= wrap.xy;
    p -= wrapDeriv*dot(vec3(p.xy, 0), wrapDeriv)*0.5*vec3(1,1,-1);

    // change the fractal rotation along an axis
    float q=p.z*0.074;

    // accumulate scale and distance
    float s = 1.0;
    float d = 9e9;

    // accumulate color
    vec3 albedo = vec3(0);
    float colorAcc = 0.0;

    for (float i = 0.5 ; i < 4.0 ; i += 1.14124) {
        p.xy *= rot(-i*1.5*q);
        p.xyz = p.zxy;
        p.xy = abs(fract(p.xy)*SCALE-SCALE*0.5);
        p.z *= SCALE;

        s /= SCALE;

        vec3 cube = vec3(0);
        float dist = sdBox(p, vec3(1.07, 0.54+i*0.5, 4.47+i*0.1), r, cube)*s;
        float co = cube.x*0.2+cube.y*0.4+cube.z*0.8;
        vec3 col = clamp(vec3(co*i*0.1), vec3(0), vec3(0.6));

        float alpha = max(0.001, smoothstep(r, -r, dist));
        albedo += col*alpha;
        colorAcc += alpha;

        if (i < 2.0) {
        	d = min(d, dist);
        } else {
            d = max(d,-dist);
        }
    }

    color = albedo/colorAcc;

    return d;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord ) {

    float z = iTime*1.0;
    vec3 from = camPath(z);
    vec2 uv = (fragCoord - iResolution.xy*0.5)/iResolution.y;
    vec3 forward = normalize(camPathDeriv(z));
    vec3 right = normalize(cross(forward, vec3(0, 1, 0)));
    vec3 up = cross(right, forward);
    vec3 dir = normalize(forward/tan(FOV*0.5)+right*uv.x+up*uv.y);

    if (iMouse.z > 0.5) {
        dir.yz *= rot((iMouse.y-iResolution.y*0.5)*0.01);
        dir.xz *= rot((iMouse.x-iResolution.x*0.5)*-0.01);
    }

   	// get the sine of the angular extent of a pixel
    float sinPix = sin(FOV / iResolution.y);
    // accumulate color front to back
    vec4 acc = vec4(0, 0, 0, 1);

    float totdist = 0.0;
    for (int i = 0 ; i < 100 ; i++) {
		vec3 p = from + totdist * dir;
        float r = totdist*sinPix;
        vec3 color = vec3(1);
        float dist = de(p, r, color);

        // compute color
        float ao = 1.0 - float(i)/100.0;
        color *= ao*ao;

        // cone trace the surface
        float prox = dist / r;
        float alpha = clamp(prox * -0.5 + 0.5, 0.0, 1.0);

        // accumulate color
        acc.rgb += acc.a * (alpha*color.rgb);
        acc.a *= (1.0 - alpha);

        // hit a surface, stop
        if (acc.a < 0.01) {
            break;
        }

        // continue forward
        totdist += abs(dist*0.9);
	}

    // add fog
    fragColor.rgb = clamp(acc.rgb, vec3(0), vec3(1));
    float fog = clamp(totdist/20.0, 0.0, 1.0);
    fragColor.rgb = mix(fragColor.rgb, vec3(0.4, 0.5, 0.7), fog);
    // gamma correction
    fragColor.rgb = pow(fragColor.rgb, vec3(1.0/2.2));
    // vignetting
    vec2 vig = fragCoord/iResolution.xy*2.0-1.0;
    fragColor.rgb = mix(fragColor.rgb, vec3(0), dot(vig, vig)*0.2);

	fragColor.a = 1.0;
}
)SHADER";

  char const shader__4ds3zn__apollonian [] = R"SHADER(
// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
//
// I can't recall where I learnt about this fractal.
//
// Coloring and fake occlusions are done by orbit trapping, as usual.


// Antialiasing level. Make it 2 or 3 if you have a fast machine
#define AA 1

vec4 orb;

float map( vec3 p, float s )
{
	float scale = 1.0;

	orb = vec4(1000.0);

	for( int i=0; i<8;i++ )
	{
		p = -1.0 + 2.0*fract(0.5*p+0.5);

		float r2 = dot(p,p);

        orb = min( orb, vec4(abs(p),r2) );

		float k = s/r2;
		p     *= k;
		scale *= k;
	}

	return 0.25*abs(p.y)/scale;
}

float trace( in vec3 ro, in vec3 rd, float s )
{
	float maxd = 30.0;
    float t = 0.01;
    for( int i=0; i<200; i++ )
    {
	    float precis = 0.001 * t;

	    float h = map( ro+rd*t, s );
        if( h<precis||t>maxd ) break;
        t += h;
    }

    if( t>maxd ) t=-1.0;
    return t;
}

vec3 calcNormal( in vec3 pos, in float t, in float s )
{
    float precis = 0.001 * t;

    vec2 e = vec2(1.0,-1.0)*precis;
    return normalize( e.xyy*map( pos + e.xyy, s ) +
					  e.yyx*map( pos + e.yyx, s ) +
					  e.yxy*map( pos + e.yxy, s ) +
                      e.xxx*map( pos + e.xxx, s ) );
}

vec3 render( in vec3 ro, in vec3 rd, in float anim )
{
    // trace
    vec3 col = vec3(0.0);
    float t = trace( ro, rd, anim );
    if( t>0.0 )
    {
        vec4 tra = orb;
        vec3 pos = ro + t*rd;
        vec3 nor = calcNormal( pos, t, anim );

        // lighting
        vec3  light1 = vec3(  0.577, 0.577, -0.577 );
        vec3  light2 = vec3( -0.707, 0.000,  0.707 );
        float key = clamp( dot( light1, nor ), 0.0, 1.0 );
        float bac = clamp( 0.2 + 0.8*dot( light2, nor ), 0.0, 1.0 );
        float amb = (0.7+0.3*nor.y);
        float ao = pow( clamp(tra.w*2.0,0.0,1.0), 1.2 );

        vec3 brdf  = 1.0*vec3(0.40,0.40,0.40)*amb*ao;
        brdf += 1.0*vec3(1.00,1.00,1.00)*key*ao;
        brdf += 1.0*vec3(0.40,0.40,0.40)*bac*ao;

        // material
        vec3 rgb = vec3(1.0);
        rgb = mix( rgb, vec3(1.0,0.80,0.2), clamp(6.0*tra.y,0.0,1.0) );
        rgb = mix( rgb, vec3(1.0,0.55,0.0), pow(clamp(1.0-2.0*tra.z,0.0,1.0),8.0) );

        // color
        col = rgb*brdf*exp(-0.2*t);
    }

    return sqrt(col);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    float time = iTime*0.25 + 0.01*iMouse.x;
    float anim = 1.1 + 0.5*smoothstep( -0.3, 0.3, cos(0.1*iTime) );

    vec3 tot = vec3(0.0);
    #if AA>1
    for( int jj=0; jj<AA; jj++ )
    for( int ii=0; ii<AA; ii++ )
    #else
    int ii = 1, jj = 1;
    #endif
    {
        vec2 q = fragCoord.xy+vec2(float(ii),float(jj))/float(AA);
        vec2 p = (2.0*q-iResolution.xy)/iResolution.y;

        // camera
        vec3 ro = vec3( 2.8*cos(0.1+.33*time), 0.4 + 0.30*cos(0.37*time), 2.8*cos(0.5+0.35*time) );
        vec3 ta = vec3( 1.9*cos(1.2+.41*time), 0.4 + 0.10*cos(0.27*time), 1.9*cos(2.0+0.38*time) );
        float roll = 0.2*cos(0.1*time);
        vec3 cw = normalize(ta-ro);
        vec3 cp = vec3(sin(roll), cos(roll),0.0);
        vec3 cu = normalize(cross(cw,cp));
        vec3 cv = normalize(cross(cu,cw));
        vec3 rd = normalize( p.x*cu + p.y*cv + 2.0*cw );

        tot += render( ro, rd, anim );
    }

    tot = tot/float(AA*AA);

	fragColor = vec4( tot, 1.0 );

}

void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{
    float time = iTime*0.25 + 0.01*iMouse.x;
    float anim = 1.1 + 0.5*smoothstep( -0.3, 0.3, cos(0.1*iTime) );

    vec3 col = render( fragRayOri + vec3(0.82,1.2,-0.3), fragRayDir, anim );
    fragColor = vec4( col, 1.0 );
}
)SHADER";

  char const shader__lsV3Rc__unmandelboxing [] = R"SHADER(
precision mediump float;
uniform vec4 I;

vec3 Z(vec3 p,float a) {
    return vec3(cos(a)*p.y+sin(a)*p.x,cos(a)*p.x-sin(a)*p.y,p.z);
}

float F(vec3 P) {
    float R=sin((iTime+P.z)*.03176)*.45+.5,S=3.4312-sin(iTime*.001);
    vec4 p=vec4(P,1),o=p,s=vec4(S,S,S,abs(S))/R;
    for(int i=0;i<24;i++) {
        if(i==3||i==7||i==11||i==15||i==19||i==23)R=sin(((iTime+P.z)*.01+float(i)*0.25*sin(iTime*.00012211154)*3.8)*3.176)*0.45+0.5;
        p.xyz=clamp(p.xyz,-1.,1.)*2.-p.xyz;
        float r2=dot(p.xyz,p.xyz);
        if(r2>1000.)break;
        p=p*clamp(max(R/r2,R),0.,1.)*s+o;
    }
    return((length(p.xyz)-abs(S-1.))/p.w-pow(abs(S),float(1-24)));
}

float D(vec3 p) {
    vec3 c=vec3(10.,10.,8.);
    p=mod(p,c)-.5*c;
    vec3 q=abs(Z(p,p.z*3.1415/10.*4.));
    float d2=max(q.z-10.,max((q.x*0.866025+q.y*0.5),q.y)-.08);
    p=Z(p,p.z*3.1415/10.*(length(p.xy)-3.)*sin(iTime*.0001)*.8);
    return max(F(p),-d2);
}

vec3 R(vec3 p,vec3 d) {
    float td=0.,rd=0.;
    for(int i=0;i<80;i++) {
        if((rd=D(p))<pow(td,1.5)*.004)break;
        td+=rd;
        p+=d*rd;
    }
    float md=D(p),e=.0025;
    vec3 n=normalize(vec3(D(p+vec3(e,0,0))-D(p-vec3(e,0,0)),D(p+vec3(0,e,0))-D(p-vec3(0,e,0)),D(p+vec3(0,0,e))-D(p-vec3(0,0,e))));
    e*=.5;
    float occ=1.+(D(p+n*.02+vec3(-e,0,0))+D(p+n*.02+vec3(+e,0,0))+D(p+n*.02+vec3(0,-e,0))+D(p+n*.02+vec3(0,e,0))+D(p+n*.02+vec3(0,0,-e))+D(p+n*.02+vec3(0,0,e))-.03)*20.;
    occ=clamp(occ,0.,1.);
    float br=(pow(clamp(dot(n,-normalize(d+vec3(.3,-.9,.4)))*.6+.4, 0.,1.),2.7)*.8+.2)*occ/(td*.5+1.);
    float fog=clamp(1./(td*td*1.8+.4),0.,1.);
    return mix(vec3(br,br/(td*td*.2+1.),br/(td+1.)),vec3(0.,0.,0.),1.-fog);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
    vec2 f=fragCoord.xy;
    vec3 d=vec3((f-vec2(iResolution/2.))/iResolution.y*2.,1.);
    vec3 c=pow(R(vec3(5.,5.,iTime*.1),normalize(d*vec3(1.,1.,1.-(length(d.xy)*.9)))),vec3(.6,.6,.6));
    //fragColor=vec4(c,1.);
    //fragColor=vec4(pow(floor(c*vec3(8.,8.,4.)+fract(f.x/4.+f.y/2.)/2.)/(vec3(7.,7.,3.)),vec3(1.5,1.5,1.5)),1.);
    vec3 scaledColour = c * vec3(8.,8.,4.);
    //float ditherOffset = 0.;
    float ditherOffset = fract(f.x/4.+f.y/2.)/1.5-.25;
    ditherOffset *= 1.5;
    //float ditherOffset = fract(f.x/2.)/4.+fract(f.y/2.)/2.;
    fragColor=vec4(pow(floor(max(scaledColour+ditherOffset,0.))/vec3(7.,7.,3.),vec3(1.5,1.5,1.5)),1.);
}
)SHADER";

  char const shader__4tc3zf__galvanize [] = R"SHADER(
//***************************************************************************************************
//
// Galvanize / Alcatraz
// Jochen "Virgill" Feldkoetter
//
// Intro for Nordlicht demoparty 2014      Shadertoy version
//
//***************************************************************************************************



int efx = 0;
int refleco = 0;
int snowo = 0;
vec4 orbitTrap = vec4(0.0);
float blend =0.0;
float d = 0.0;
float m = 0.0;
float kalitime =0.;
float depth = 0.;
float prec =0.;
const float scene = 35.;


// Rotate
vec3 rotXaxis(vec3 p, float rad)
{
	float z2 = cos(rad) * p.z - sin(rad) * p.y;
	float y2 = sin(rad) * p.z + cos(rad) * p.y;
	p.z = z2;
	p.y = y2;
	return p;
}

vec3 rotYaxis(vec3 p, float rad)
{
	float x2 = cos(rad) * p.x - sin(rad) * p.z;
	float z2 = sin(rad) * p.x + cos(rad) * p.z;
	p.x = x2;
	p.z = z2;
	return p;
}

vec3 rotZaxis(vec3 p, float rad)
{
	float x2 = cos(rad) * p.x - sin(rad) * p.y;
	float y2 = sin(rad) * p.x + cos(rad) * p.y;
	p.x = x2;
	p.y = y2;
	return p;
}


// noise functions
float rand1(vec2 co)
{
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float rand2(vec2 co)
{
    return fract(cos(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}




// polyomial smooth min (IQ)
float sminPoly( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}


// exponential smooth min (IQ)
float smin( float a, float b, float k )
{
    float res = exp( -k*a ) + exp( -k*b );
    return -log( res )/k;
}


// length
float length2(vec2 p)
{
  	return dot(p, p);
}

// worley effect
float worley(vec2 p)
{
	float d = 1.;
	for (int xo = -1; xo <= 1; ++xo)
	for (int yo = -1; yo <= 1; ++yo)
    {
		vec2 tp = floor(p) + vec2(xo, yo);
		d = min(d, length2(p - tp - vec2(rand1(tp))));
	}
	return 3.*exp(-4.*abs(2.*d - 1.));
}

float fworley(vec2 p)
{
	return sqrt(sqrt(sqrt(worley(p*32. + 4.3 + iTime*.250) * sqrt(worley(p * 64. + 5.3 + iTime * -.125)) * sqrt(sqrt(worley(p * -128. +7.3))))));
}


// menger
float NewMenger(vec3 z)
{
	float Scale = 3.0;
	vec3 Offset = vec3(1.0,1.0,1.0);
	int Iterations = 6;
	int ColorIterations = 3;

    for(int n = 0; n < 6; n++)
	{
	z.z*=1.+0.2*sin(iTime/4.0)+0.1;
		z = abs(z);
		if (z.x<z.y){ z.xy = z.yx;}
		if (z.x< z.z){ z.xz = z.zx;}
		if (z.y<z.z){ z.yz = z.zy;}
		z = Scale*z-Offset*(Scale-1.0);
		if( z.z<-0.5*Offset.z*(Scale-1.0))  z.z+=Offset.z*(Scale-1.0);

		if (n<ColorIterations) orbitTrap = min(orbitTrap, (vec4(abs(z),dot(z,z))));

	}
	return abs(length(z) ) * pow(Scale, float(-Iterations-1));
}



// mandelbulb (Fractalforums.com)
float Mandelbulb(vec3 p)
{
	float Scale = 3.0;
	int Iterations = 6;
	int ColorIterations = 1;
	float parachute=(1.-min(1.8*abs(sin((iTime-5.0)*3.1415/scene)),1.0)); // Fallschirm
	parachute = smoothstep(0.0,1.0,parachute)*35.0;
	vec3 w = p;
	float dr = 1.0+parachute;
	float r = 0.;
    for (int i=0; i<6; ++i)
	{
    	r = length(w);
		if (r>4.0) break;
		dr*=pow(r, 7.)*8.+1.;
		float x = w.x; float x2 = x*x; float x4 = x2*x2;
		float y = w.y; float y2 = y*y; float y4 = y2*y2;
		float z = w.z; float z2 = z*z; float z4 = z2*z2;
		float k3 = x2 + z2;
		float k2 = inversesqrt( pow(k3, 7.0) );
		float k1 = x4 + y4 + z4 - 6.0*y2*z2 - 6.0*x2*y2 + 2.0*z2*x2;
		float k4 = x2 - y2 + z2;
		w =  vec3(64.0*x*y*z*(x2-z2)*k4*(x4-6.0*x2*z2+z4)*k1*k2,-16.0*y2*k3*k4*k4 + k1*k1,-8.0*y*k4*(x4*x4 - 28.0*x4*x2*z2 + 70.0*x4*z4 - 28.0*x2*z2*z4 + z4*z4)*k1*k2);
		w-=p;
		w = rotYaxis(w,sin(iTime*0.14));
		w = rotZaxis(w,cos(iTime*0.2));
		orbitTrap = min(orbitTrap, abs(vec4(p.x*w.z, p.y*w.x, 0., 0.)));
		if (i>=ColorIterations+2) orbitTrap = vec4(0.0);
	}
	return  .5*log(r)*r/dr;
}

// kalibox (Kali / Fractalforums.com)
float Kalibox(vec3 pos)
{
	float Scale = 1.84;
	int Iterations = 14;
	int ColorIterations = 3;
	float MinRad2 = 0.34;
	vec3 Trans = vec3(0.076,-1.86,0.036);
	vec3 Julia = vec3(-0.66,-1.2+(kalitime/80.),-0.66);
	vec4 scale = vec4(Scale, Scale, Scale, abs(Scale)) / MinRad2;
	float absScalem1 = abs(Scale - 1.0);
	float AbsScaleRaisedTo1mIters = pow(abs(Scale), float(1-Iterations));
    vec4 p = vec4(pos,1), p0 = vec4(Julia,1);
	for (int i=0; i<14; i++)
		{
			p.xyz=abs(p.xyz)+Trans;
			float r2 = dot(p.xyz, p.xyz);
			p *= clamp(max(MinRad2/r2, MinRad2), 0.0, 1.0);
			p = p*scale + p0;
			if (i<ColorIterations) orbitTrap = min(orbitTrap, abs(vec4(p.xyz,r2)));
		}
		return (    (length(p.xyz) - absScalem1) / p.w - AbsScaleRaisedTo1mIters    );
}

// balls and cube
float Balls(vec3 pos)
{
	m = length(max(abs(rotYaxis(rotXaxis(pos+vec3(0.0,-0.3,0.0),iTime),iTime*0.3))-vec3(0.35,0.35,0.35),0.0))-0.02;
	m = smin (m, length(pos+vec3(0.0,-0.40,1.2+0.5*sin(0.8*iTime+0.0)))-0.4,7.4);
	m = smin (m, length(pos+vec3(0.0,-0.40,-1.2-0.5*sin(0.8*iTime+0.4)))-0.4,7.4);
	m = smin (m, length(pos+vec3(-1.2-0.5*sin(0.8*iTime+0.8),-0.40,0.0))-0.4,7.4);
	m = smin (m, length(pos+vec3(1.2+0.5*sin(0.8*iTime+1.2),-0.40,0.0))-0.4,7.4);
	m = smin (m, length(pos+vec3(0.0,-1.6+0.5*-sin(0.8*iTime+1.6),0.0))-0.4,7.4);
	//m+= klang1*(0.003*cos(50.*pos.x)+0.003*cos(50.*pos.y)); //distortion
	orbitTrap = vec4(length(pos)-0.8*pos.z,length(pos)-0.8*pos.y,length(pos)-0.8*pos.x,0.0)*1.0;
	return m;
}

// plane
float sdPlane(in vec3 p)
{
	return p.y+(0.025*sin(p.x*10.  +1.4*iTime  ))+(0.025*sin(p.z*12.3*cos(0.4-p.x)+  1.6*iTime  ))-0.05;
}

// cylinder
float sdCylinder( vec3 p, vec3 c )
{
	return length(p.xz-c.xy)-c.z;
}


// scene
float map(in vec3 p)
{
	orbitTrap = vec4(10.0);
	d = sdPlane(p);

	if (efx == 0) {			// balls and cube
	m = Balls(p);
	}
	if (efx == 1) {			// milky menger
	m = NewMenger(rotYaxis(rotXaxis(p-vec3(0.0,sin(iTime/0.63)+0.2,0.0),0.15*iTime),0.24*iTime));
	}
	if (efx == 2) {			// mandelbulb
	m = Mandelbulb(rotYaxis(rotXaxis(p,iTime*0.1),0.21*iTime));
	}
	if (efx == 3) {			// kalibox
	m = Kalibox(rotYaxis(rotXaxis(p,1.50),0.1*iTime));
	}
	if (efx == 4 || efx == 5) { // tunnel or swirl
	vec3 c = vec3(2.0, 8.0, 2.0);
	vec3 q = mod(p-vec3(1.0,0.1*iTime,1.0),c)-0.5*c;
	float kali = Kalibox(rotYaxis(q,0.04*iTime));
	m = max(kali,-sdCylinder(p,vec3(0.0,0.0,0.30+0.1*sin(iTime*0.2))) );
	}
	d = sminPoly (m, d, 0.04);
   	return d;
}


// normal calculation
vec3 calcNormal(in vec3 p)
{
    vec3 e = vec3(0.001, 0.0, 0.0);
    vec3 nor = vec3(map(p + e.xyy) - map(p - e.xyy),  map(p + e.yxy) - map(p - e.yxy),  map(p + e.yyx) - map(p - e.yyx));
    return normalize(nor);
}

// cast
float castRay(in vec3 ro, in vec3 rd, in float maxt)
{
    float precis = prec;
    float h = precis * 2.0;
    float t = depth;

    for(int i = 0; i < 122; i++)
	{
        if(abs(h) < precis || t > maxt) break;
        orbitTrap = vec4(10.0);
		h = map(ro + rd * t);
        t += h;
	}
    return t;
}

// softshadow (IQ)
float softshadow(in vec3 ro, in vec3 rd, in float mint, in float maxt, in float k)
{
    float sh = 1.0;
    float t = mint;
    float h = 0.0;
    for(int i = 0; i < 19; i++)  //23 gut!
	{
        if(t > maxt) continue;
		orbitTrap = vec4(10.0);
        h = map(ro + rd * t);
        sh = min(sh, k * h / t);
        t += h;
    }
    return sh;
}


// orbit color
vec3 BaseColor = vec3(0.2,0.2,0.2);
vec3 OrbitStrength = vec3(0.8, 0.8, 0.8);
vec4 X = vec4(0.5, 0.6, 0.6, 0.2);
vec4 Y = vec4(1.0, 0.5, 0.1, 0.7);
vec4 Z = vec4(0.8, 0.7, 1.0, 0.3);
vec4 R = vec4(0.7, 0.7, 0.5, 0.1);
vec3 getColor()
{
	orbitTrap.w = sqrt(orbitTrap.w);
	vec3 orbitColor = X.xyz*X.w*orbitTrap.x + Y.xyz*Y.w*orbitTrap.y + Z.xyz*Z.w*orbitTrap.z + R.xyz*R.w*orbitTrap.w;
	vec3 color = mix(BaseColor,3.0*orbitColor,OrbitStrength);
	return color;
}

// particles (Andrew Baldwin)
float snow(vec3 direction)
{
	float help = 0.0;
	const mat3 p = mat3(13.323122,23.5112,21.71123,21.1212,28.7312,11.9312,21.8112,14.7212,61.3934);
	vec2 uvx = vec2(direction.x,direction.z)+vec2(1.,iResolution.y/iResolution.x)*gl_FragCoord.xy / iResolution.xy;
	float acc = 0.0;
	float DEPTH = direction.y*direction.y-0.3;
	float WIDTH =0.1;
	float SPEED = 0.1;
	for (int i=0;i<10;i++)
	{
		float fi = float(i);
		vec2 q = uvx*(1.+fi*DEPTH);
		q += vec2(q.y*(WIDTH*mod(fi*7.238917,1.)-WIDTH*.5),SPEED*iTime/(1.+fi*DEPTH*.03));
		vec3 n = vec3(floor(q),31.189+fi);
		vec3 m = floor(n)*.00001 + fract(n);
		vec3 mp = (31415.9+m)/fract(p*m);
		vec3 r = fract(mp);
		vec2 s = abs(mod(q,1.)-.5+.9*r.xy-.45);
		float d = .7*max(s.x-s.y,s.x+s.y)+max(s.x,s.y)-.01;
		float edge = .04;
		acc += smoothstep(edge,-edge,d)*(r.x/1.0);
		help = acc;
	}
	return help;
	}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{

    if (iTime >=0. && iTime <=35. ) {efx=4; refleco=0; snowo=0;}
    if (iTime >35. && iTime <=70. ) {efx=0; refleco=1; snowo=1;}
    if (iTime >70. && iTime <=105.) {efx=1; refleco=0; snowo=1;}
    if (iTime >105.&& iTime <=140.) {efx=3; refleco=0; snowo=1;}
    if (iTime >140.&& iTime <=175.) {efx=2; refleco=0; snowo=1;}
    if (iTime >175.&& iTime <=210.) {efx=4; refleco=0; snowo=0;}
    if (iTime >210.&& iTime <=245.) {efx=5; refleco=0; snowo=0;}

	blend=min(2.0*abs(sin((iTime+0.0)*3.1415/scene)),1.0);
    if (iTime >245.) blend = 0.;
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec2 p = uv * 2.0 - 1.0;
	p.x *= iResolution.x / iResolution.y;
	float theta = sin(iTime*0.03) * 3.14 * 2.0;
    float x = 3.0 * cos(theta)+0.007*rand1(fragCoord.xy);
    float z = 3.0 * sin(theta)+0.007*rand2(fragCoord.xy);
	vec3 ro; // camera

	if (efx==0) {
	prec = 0.001;
	ro = vec3(x*0.2+1.0, 5.0, z*2.0-3.); 	// camera balls and cube
	}
	if (efx==1) {
	prec = 0.002;
	ro = vec3(x*1.2, 7.0, z*2.0);  			// camera menger
	}
	if (efx==2) {
	prec = 0.002;
	ro = vec3(x*1.0, 6.2, z*2.8);  			// camera mandelbulb
	depth =4.;
	}
	if (efx==3) {
	kalitime = 40.;
	prec = 0.002;
	ro = vec3(x*1.7, 2.6, 2.0);	 			// camera kalibox
	}
	if (efx==4) {
	//time = iTime -2.5;
	prec = 0.002;
	kalitime = iTime-15.0;
	ro = vec3(0.0, 8.0, 0.0001);   			// camera tunnel
	}
	if (efx==5) {
	prec = 0.004;
	kalitime = 210.+175.;
	ro = vec3(0, 3.8, 0.0001);   			// camera swirl
	}


	vec3 ta = vec3(0.0, 0.25, 0.0);
    vec3 cw = normalize(ta - ro);
    vec3 cp = vec3(0.0, 1.0, 0.0);
    vec3 cu = normalize(cross(cw, cp));
    vec3 cv = normalize(cross(cu, cw));
	vec3 rd = normalize(p.x * cu + p.y * cv + 7.5 * cw);

// render:
    vec3 col = vec3(0.0);
    float t = castRay(ro, rd, 12.0);
	vec3 pos = ro + rd *t;
	vec3 nor = calcNormal(pos);
	vec3 lig;
	if (efx==4 || efx ==5 )  	lig = normalize(vec3(-0.4*sin(iTime*0.15), 1.0, 0.5));
	else if (efx==3)		  	lig = normalize(vec3(-0.1*sin(iTime*0.2), 0.2, 0.4*sin(iTime*0.1)));
	else 						lig = normalize(vec3(-0.4, 0.7, 0.5));
	float dif = clamp(dot(lig, nor), 0.0, 1.0);
	float spec = pow(clamp(dot(reflect(rd, nor), lig), 0.0, 1.0), 16.0);
	float sh;
	if (efx == 1 || efx == 5) sh = softshadow(pos, lig, 0.02, 20.0, 7.0);
	vec3 color = getColor();
	col = ((0.8*dif+ spec) + 0.35*color);
	if (efx !=1 && efx != 5) sh = softshadow(pos, lig, 0.02, 20.0, 7.0);
	col = col*clamp(sh, 0.0, 1.0);


// reflections:
if (refleco == 1) {
    vec3 col2 = vec3(0.0);
	vec3 ro2 = pos-rd/t;
	vec3 rd2 = reflect(rd,nor);
    float t2 = castRay(ro2, rd2, 7.0);
	vec3 pos2 = vec3(0.0);
	if (t2<7.0) {
	pos2 = ro2 + rd2* t2;
	}
    vec3 nor2 = calcNormal(pos2);
	float dif2 = clamp(dot(lig, nor2), 0.0, 1.0);
	float spec2 = pow(clamp(dot(reflect(rd2, nor2), lig), 0.0, 1.0), 16.0);
	col+= 0.22*vec3(dif2*color+spec2);
}

// postprocessing
float klang1=0.75;
vec2 uv2=-0.3+2.*fragCoord.xy/iResolution.xy;
col-=0.20*(1.-klang1)*rand1(uv2.xy*iTime);
col*=.9+0.20*(1.-klang1)*sin(10.*iTime+uv2.x*iResolution.x);
col*=.9+0.20*(1.-klang1)*sin(10.*iTime+uv2.y*iResolution.y);
float Scr=1.-dot(uv2,uv2)*0.15;
vec2 uv3=fragCoord.xy/iResolution.xy;
float worl = fworley(uv3 * iResolution.xy / 2100.);
worl *= exp(-length2(abs(2.*uv3 - 1.)));
worl *= abs(1.-0.6*dot(2.*uv3-1.,2.*uv3-1.));
if (efx==4) col += vec3(0.4*worl,0.35*worl,0.25*worl);
if (efx==5)  col += vec3(0.2*worl);
float g2 = (blend/2.)+0.39;
float g1 = ((1.-blend)/2.);
if (uv3.y >=g2+0.11) col*=0.0;
if (uv3.y >=g2+0.09) col*=0.4;
if (uv3.y >=g2+0.07) {if (mod(uv3.x-0.06*iTime,0.18)<=0.16) col*=0.5;}
if (uv3.y >=g2+0.05) {if (mod(uv3.x-0.04*iTime,0.12)<=0.10) col*=0.6;}
if (uv3.y >=g2+0.03) {if (mod(uv3.x-0.02*iTime,0.08)<=0.06) col*=0.7;}
if (uv3.y >=g2+0.01) {if (mod(uv3.x-0.01*iTime,0.04)<=0.02) col*=0.8;}
if (uv3.y <=g1+0.10) {if (mod(uv3.x+0.01*iTime,0.04)<=0.02) col*=0.8;}
if (uv3.y <=g1+0.08) {if (mod(uv3.x+0.02*iTime,0.08)<=0.06) col*=0.7;}
if (uv3.y <=g1+0.06) {if (mod(uv3.x+0.04*iTime,0.12)<=0.10) col*=0.6;}
if (uv3.y <=g1+0.04) {if (mod(uv3.x+0.06*iTime,0.18)<=0.16) col*=0.5;}
if (uv3.y <=g1+0.02) col*=0.4;
if (uv3.y <=g1+0.00) col*=0.0;

if (snowo == 1) fragColor = (vec4(col*1.0*Scr-1.6*snow(cv), 1.0)*blend)*vec4(1.0, 0.93, 1.0, 1.0);
else fragColor = vec4(col*1.0*Scr, 1.0)*blend;
}
)SHADER";

  char const shader__4dsGRl__subsurface_scattering [] = R"SHADER(
// Ben Weston - 20/08/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// constants
const float tau = 6.28318530717958647692;


// ---USER TWEAKABLE THINGS!---

const float epsilon = .003;
const float normalPrecision = .1;
const float shadowOffset = .1;
const int traceDepth = 400; // takes time
const float drawDistance = 100.0;

const vec3 CamPos = vec3(0,40.0,-40.0);
const vec3 CamLook = vec3(0,0,0);

const vec3 lightDir = vec3(.7,1,-.1);
const vec3 fillLightDir = vec3(0,0,-1);
const vec3 lightColour = vec3(1.1,1.05,1);
const vec3 fillLightColour = vec3(.38,.4,.42);

// This should return continuous positive values when outside and negative values inside,
// which roughly indicate the distance of the nearest surface.
float Isosurface( vec3 ipos )
{
	// animate the object rotating
	float ang = iTime*tau/25.0;
	float ang2 = iTime*tau/125.0;
	float s = sin(ang), c = cos(ang);
	float s2 = sin(ang2), c2 = cos(ang2);
	vec3 pos;
	pos.y = c*ipos.y-s*ipos.z;
	pos.z = c*ipos.z+s*ipos.y;
	pos.x = ipos.x*c2+pos.z*s2;
	pos.z = pos.z*c2-ipos.x*s2;


	// smooth csg
	float smoothing = .9-.65*cos(iTime*.05);

	return
		log(
			// intersection
			1.0/(
				// union
				1.0/(
					// intersection
					exp((length(pos.xz)-10.0)/smoothing) +
					exp((-(length(pos.xz)-7.0))/smoothing) +
					exp((-(length(vec2(8.0,0)+pos.zy)-5.0))/smoothing) +
					exp((pos.y-10.0)/smoothing) +
					exp((-pos.y-10.0)/smoothing)
					)
				+ exp(-(length(pos+15.0*vec3(sin(iTime*.07),sin(iTime*.13),sin(iTime*.1)))-5.0))
				)
			// trim it with a plane
			//+ exp((dot(pos,normalize(vec3(-1,-1,1)))-10.0-10.0*sin(iTime*.17))/smoothing)
		)*smoothing
		;//+ Noise(pos*16.0)*.08/16.0; // add some subtle texture
}


// alpha controls reflection
vec4 Shading( vec3 pos, vec3 norm, vec3 visibility, vec3 rd )
{
	vec3 albedo = vec3(1);//mix( vec3(1,.8,.7), vec3(.5,.2,.1), Noise(pos*vec3(1,10,1)) );

	vec3 l = lightColour*mix(visibility,vec3(1)*max(0.0,dot(norm,normalize(lightDir))),.0);
	vec3 fl = fillLightColour*(dot(norm,normalize(fillLightDir))*.5+.5);

	vec3 view = normalize(-rd);
	vec3 h = normalize(view+lightDir);
	float specular = pow(max(0.0,dot(h,norm)),2000.0);

	float fresnel = pow( 1.0 - dot( view, norm ), 5.0 );
	fresnel = mix( .01, 1.0, min(1.0,fresnel) );

	return vec4( albedo*(l+fl)*(1.0-fresnel) + visibility*specular*32.0*lightColour, fresnel );
}

const vec3 FogColour = vec3(.1,.2,.5);

float saturate(float a) { return clamp(a, 0.0, 1.0); }

void pR(inout vec2 p, float a)
{
  p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec3 SkyColour(vec3 rayDir)
{
    pR (rayDir.yz, 1.0);
    //pR (rayDir.xy, iTime/5.0);

    // fade bottom to top so it looks like the softbox is casting light on a floor
    // and it's bouncing back
    vec3 final = vec3(1.0) * dot(-rayDir, lightDir) * 0.5 + 0.5;
    final *= 0.125;
    // overhead softbox, stretched to a rectangle
    if ((rayDir.y > abs(rayDir.x)*1.0) && (rayDir.y > abs(rayDir.z*0.25))) final = vec3(2.0)*rayDir.y;
    // fade the softbox at the edges with a rounded rectangle.
    float roundBox = length(max(abs(rayDir.xz/max(0.0,rayDir.y))-vec2(0.9, 4.0),0.0))-0.1;
    final += vec3(0.8)* pow(saturate(1.0 - roundBox*0.5), 6.0);
    // purple lights from side
    final += vec3(8.0,6.0,7.0) * saturate(0.025/(1.0 - abs(rayDir.x)));
    // yellow lights from side
    final += vec3(8.0,7.0,6.0) * saturate(0.002/(1.0 - abs(rayDir.z)));
	vec3 hdr = 1.0/(1.2-final) - 1.0/1.2;
    return vec3(final);
}

// ---END OF USER TWEAKABLE THINGS!---


float Trace( vec3 ro, vec3 rd )
{
	float t = 0.0;
	float dist = 1.0;
	for ( int i=0; i < traceDepth; i++ )
	{
		if ( abs(dist) < epsilon || t > drawDistance || t < 0.0 )
			continue;
		dist = Isosurface( ro+rd*t );
		t = t+dist;
	}

	// reduce edge sparkles, caused by reflections on failed positions
	if ( dist > epsilon )
		return drawDistance+1.0;

	return t;//vec4(ro+rd*t,dist);
}

vec3 SubsurfaceTrace( vec3 ro, vec3 rd )
{
	vec3 density = pow(vec3(.7,.5,.4),vec3(.4));
	const float confidence = .01;
	vec3 visibility = vec3(1.0);

	float lastVal = Isosurface(ro);
	float soft = 0.0;
	for ( int i=1; i < 50; i++ )
	{
		if ( visibility.x < confidence )
			continue;

		float val = Isosurface(ro);

		vec3 softened = pow(density,vec3(smoothstep(soft,-soft,val)));
//tweak this to create soft shadows, by expanding with each step (linearly)

		if ( (val-soft)*lastVal < 0.0 )
		{
			// approximate position of the surface
			float transition = -min(val-soft,lastVal)/abs(val-soft-lastVal);
			visibility *= pow(softened,vec3(transition));
		}
		else if ( val-soft < 0.0 )
		{
			visibility *= softened;
		}

		soft += .1;
		lastVal = val+soft;
		ro += rd*.4;
	}

	return visibility;
}

// get normal
vec3 GetNormal( vec3 pos )
{
	const vec2 delta = vec2(normalPrecision, 0);

	vec3 n;

// it's important this is centred on the pos, it fixes a lot of errors
	n.x = Isosurface( pos + delta.xyy ) - Isosurface( pos - delta.xyy );
	n.y = Isosurface( pos + delta.yxy ) - Isosurface( pos - delta.yxy );
	n.z = Isosurface( pos + delta.yyx ) - Isosurface( pos - delta.yyx );
	return normalize(n);
}

// camera function by TekF
// compute ray from camera parameters
vec3 GetRay( vec3 dir, float zoom, vec2 uv )
{
	uv = uv - .5;
	uv.x *= iResolution.x/iResolution.y;

	dir = zoom*normalize(dir);
	vec3 right = normalize(cross(vec3(0,1,0),dir));
	vec3 up = normalize(cross(dir,right));

	return dir + right*uv.x + up*uv.y;
}


void Humbug( inout vec4 result, inout vec3 ro, inout vec3 rd )
{
	if ( result.a < .01 )
		return; // continue; // break;

	float t = Trace(ro,rd);

	vec4 samplev = vec4( SkyColour( rd ), 0 );

	vec3 norm;
	if ( t < drawDistance )
	{
		ro = ro+t*rd;

		norm = GetNormal(ro);

		// shadow test
		/*float shadow = 1.0;
		if ( Trace( ro+lightDir*shadowOffset, lightDir ) < drawDistance )
			shadow = 0.0;*/

		vec3 subsurface;
     	//subsurface = vec3(dot(norm,lightDir));
		subsurface = SubsurfaceTrace( ro+rd*1.0, lightDir );


		samplev = Shading( ro, norm, subsurface, rd );
	}

	result.rgb += samplev.rgb*result.a;
	result.a *= samplev.a;
	result.a = clamp(result.a,0.0,1.0); // without this, chrome shows black!

	//		// fog
	//		result = mix ( vec4(FogColour, 0), result, exp(-t*t*.0002) );

	rd = reflect(rd,norm);

	ro += rd*shadowOffset;
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / iResolution.xy;

	vec3 camPos = CamPos;
	vec3 camLook = CamLook;

	vec2 camRot = vec2(iTime*.1,0)+.5*tau*(iMouse.xy-iResolution.xy*.5)/iResolution.x;
	camPos.yz = cos(camRot.y)*camPos.yz + sin(camRot.y)*camPos.zy*vec2(1,-1);
	camPos.xz = cos(camRot.x)*camPos.xz + sin(camRot.x)*camPos.zx*vec2(1,-1);

	if ( Isosurface(camPos) <= 0.0 )
	{
		// camera inside ground
		fragColor = vec4(0,0,0,0);
		return;
	}

	vec3 ro = camPos;
	vec3 rd;
	rd = GetRay( camLook-camPos, 2.0, uv );
	rd = normalize(rd);

	vec4 result = vec4(0,0,0,1);

	Humbug( result, ro, rd );
	Humbug( result, ro, rd );
	Humbug( result, ro, rd );

	fragColor = result;
}
)SHADER";

  char const shader__4sXBRn__luminescence[] = R"SHADER(
// Luminescence by Martijn Steinrucken aka BigWings - 2017
// Email:countfrolic@gmail.com Twitter:@The_ArtOfCode
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// My entry for the monthly challenge (May 2017) on r/proceduralgeneration 
// Use the mouse to look around. Uncomment the SINGLE define to see one specimen by itself.
// Code is a bit of a mess, too lazy to clean up. Hope you like it!

// Music by Klaus Lunde
// https://soundcloud.com/klauslunde/zebra-tribute

// YouTube: The Art of Code -> https://www.youtube.com/channel/UCcAlTqd9zID6aNX3TzwxJXg
// Twitter: @Steinrucken

#define INVERTMOUSE -1.

#define MAX_STEPS 100.
#define VOLUME_STEPS 8.
//#define SINGLE
#define MIN_DISTANCE 0.1
#define MAX_DISTANCE 100.
#define HIT_DISTANCE .01

#define S(x,y,z) smoothstep(x,y,z)
#define B(x,y,z,w) S(x-z, x+z, w)*S(y+z, y-z, w)
#define sat(x) clamp(x,0.,1.)
#define SIN(x) sin(x)*.5+.5

const vec3 lf=vec3(1., 0., 0.);
const vec3 up=vec3(0., 1., 0.);
const vec3 fw=vec3(0., 0., 1.);

const float halfpi = 1.570796326794896619;
const float pi = 3.141592653589793238;
const float twopi = 6.283185307179586;


vec3 accentColor1 = vec3(1., .1, .5);
vec3 secondColor1 = vec3(.1, .5, 1.);

vec3 accentColor2 = vec3(1., .5, .1);
vec3 secondColor2 = vec3(.1, .5, .6);

vec3 bg;	 	// global background color
vec3 accent;	// color of the phosphorecence

float N1( float x ) { return fract(sin(x)*5346.1764); }
float N2(float x, float y) { return N1(x + y*23414.324); }

float N3(vec3 p) {
    p  = fract( p*0.3183099+.1 );
	p *= 17.0;
    return fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
}

struct ray {
    vec3 o;
    vec3 d;
};

struct camera {
    vec3 p;			// the position of the camera
    vec3 forward;	// the camera forward vector
    vec3 left;		// the camera left vector
    vec3 up;		// the camera up vector
	
    vec3 center;	// the center of the screen, in world coords
    vec3 i;			// where the current ray intersects the screen, in world coords
    ray ray;		// the current ray: from cam pos, through current uv projected on screen
    vec3 lookAt;	// the lookat point
    float zoom;		// the zoom factor
};

struct de {
    // data type used to pass the various bits of information used to shade a de object
	float d;	// final distance to field
    float m; 	// material
    vec3 uv;
    float pump;
    
    vec3 id;
    vec3 pos;		// the world-space coordinate of the fragment
};
    
struct rc {
    // data type used to handle a repeated coordinate
	vec3 id;	// holds the floor'ed coordinate of each cell. Used to identify the cell.
    vec3 h;		// half of the size of the cell
    vec3 p;		// the repeated coordinate
    //vec3 c;		// the center of the cell, world coordinates
};
    
rc Repeat(vec3 pos, vec3 size) {
	rc o;
    o.h = size*.5;					
    o.id = floor(pos/size);			// used to give a unique id to each cell
    o.p = mod(pos, size)-o.h;
    //o.c = o.id*size+o.h;
    
    return o;
}
    
camera cam;


void CameraSetup(vec2 uv, vec3 position, vec3 lookAt, float zoom) {
	
    cam.p = position;
    cam.lookAt = lookAt;
    cam.forward = normalize(cam.lookAt-cam.p);
    cam.left = cross(up, cam.forward);
    cam.up = cross(cam.forward, cam.left);
    cam.zoom = zoom;
    
    cam.center = cam.p+cam.forward*cam.zoom;
    cam.i = cam.center+cam.left*uv.x+cam.up*uv.y;
    
    cam.ray.o = cam.p;						// ray origin = camera position
    cam.ray.d = normalize(cam.i-cam.p);	// ray direction is the vector from the cam pos through the point on the imaginary screen
}


// ============== Functions I borrowed ;)

//  3 out, 1 in... DAVE HOSKINS
vec3 N31(float p) {
   vec3 p3 = fract(vec3(p) * vec3(.1031,.11369,.13787));
   p3 += dot(p3, p3.yzx + 19.19);
   return fract(vec3((p3.x + p3.y)*p3.z, (p3.x+p3.z)*p3.y, (p3.y+p3.z)*p3.x));
}

// DE functions from IQ
float smin( float a, float b, float k )
{
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

float smax( float a, float b, float k )
{
	float h = clamp( 0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
	return mix( a, b, h ) + k*h*(1.0-h);
}

float sdSphere( vec3 p, vec3 pos, float s ) { return (length(p-pos)-s); }

// From http://mercury.sexy/hg_sdf
vec2 pModPolar(inout vec2 p, float repetitions, float fix) {
	float angle = twopi/repetitions;
	float a = atan(p.y, p.x) + angle/2.;
	float r = length(p);
	float c = floor(a/angle);
	a = mod(a,angle) - (angle/2.)*fix;
	p = vec2(cos(a), sin(a))*r;

	return p;
}
    
// -------------------------


float Dist( vec2 P,  vec2 P0, vec2 P1 ) {
    //2d point-line distance
    
	vec2 v = P1 - P0;
    vec2 w = P - P0;

    float c1 = dot(w, v);
    float c2 = dot(v, v);
    
    if (c1 <= 0. )  // before P0
    	return length(P-P0);
    
    float b = c1 / c2;
    vec2 Pb = P0 + b*v;
    return length(P-Pb);
}

vec3 ClosestPoint(vec3 ro, vec3 rd, vec3 p) {
    // returns the closest point on ray r to point p
    return ro + max(0., dot(p-ro, rd))*rd;
}

vec2 RayRayTs(vec3 ro1, vec3 rd1, vec3 ro2, vec3 rd2) {
	// returns the two t's for the closest point between two rays
    // ro+rd*t1 = ro2+rd2*t2
    
    vec3 dO = ro2-ro1;
    vec3 cD = cross(rd1, rd2);
    float v = dot(cD, cD);
    
    float t1 = dot(cross(dO, rd2), cD)/v;
    float t2 = dot(cross(dO, rd1), cD)/v;
    return vec2(t1, t2);
}

float DistRaySegment(vec3 ro, vec3 rd, vec3 p1, vec3 p2) {
	// returns the distance from ray r to line segment p1-p2
    vec3 rd2 = p2-p1;
    vec2 t = RayRayTs(ro, rd, p1, rd2);
    
    t.x = max(t.x, 0.);
    t.y = clamp(t.y, 0., length(rd2));
                
    vec3 rp = ro+rd*t.x;
    vec3 sp = p1+rd2*t.y;
    
    return length(rp-sp);
}

vec2 sph(vec3 ro, vec3 rd, vec3 pos, float radius) {
	// does a ray sphere intersection
    // returns a vec2 with distance to both intersections
    // if both a and b are MAX_DISTANCE then there is no intersection
    
    vec3 oc = pos - ro;
    float l = dot(rd, oc);
    float det = l*l - dot(oc, oc) + radius*radius;
    if (det < 0.0) return vec2(MAX_DISTANCE);
    
    float d = sqrt(det);
    float a = l - d;
    float b = l + d;
    
    return vec2(a, b);
}


vec3 background(vec3 r) {
	
    float x = atan(r.x, r.z);		// from -pi to pi	
	float y = pi*0.5-acos(r.y);  		// from -1/2pi to 1/2pi		
    
    vec3 col = bg*(1.+y);
    
	float t = iTime;				// add god rays
    
    float a = sin(r.x);
    
    float beam = sat(sin(10.*x+a*y*5.+t));
    beam *= sat(sin(7.*x+a*y*3.5-t));
    
    float beam2 = sat(sin(42.*x+a*y*21.-t));
    beam2 *= sat(sin(34.*x+a*y*17.+t));
    
    beam += beam2;
    col *= 1.+beam*.05;

    return col;
}




float remap(float a, float b, float c, float d, float t) {
	return ((t-a)/(b-a))*(d-c)+c;
}



de map( vec3 p, vec3 id ) {

    float t = iTime*2.;
    
    float N = N3(id);
    
    de o;
    o.m = 0.;
    
    float x = (p.y+N*twopi)*1.+t;
    float r = 1.;
    
    float pump = cos(x+cos(x))+sin(2.*x)*.2+sin(4.*x)*.02;
    
    x = t + N*twopi;
    p.y -= (cos(x+cos(x))+sin(2.*x)*.2)*.6;
    p.xz *= 1. + pump*.2;
    
    float d1 = sdSphere(p, vec3(0., 0., 0.), r);
    float d2 = sdSphere(p, vec3(0., -.5, 0.), r);
    
    o.d = smax(d1, -d2, .1);
    o.m = 1.;
    
    if(p.y<.5) {
        float sway = sin(t+p.y+N*twopi)*S(.5, -3., p.y)*N*.3;
        p.x += sway*N;	// add some sway to the tentacles
        p.z += sway*(1.-N);
        
        vec3 mp = p;
    	mp.xz = pModPolar(mp.xz, 6., 0.);
        
        float d3 = length(mp.xz-vec2(.2, .1))-remap(.5, -3.5, .1, .01, mp.y);
    	if(d3<o.d) o.m=2.;
        d3 += (sin(mp.y*10.)+sin(mp.y*23.))*.03;
        
        float d32 = length(mp.xz-vec2(.2, .1))-remap(.5, -3.5, .1, .04, mp.y)*.5;
        d3 = min(d3, d32);
        o.d = smin(o.d, d3, .5);
        
        if( p.y<.2) {
             vec3 op = p;
    		op.xz = pModPolar(op.xz, 13., 1.);
            
        	float d4 = length(op.xz-vec2(.85, .0))-remap(.5, -3., .04, .0, op.y);
    		if(d4<o.d) o.m=3.;
            o.d = smin(o.d, d4, .15);
        }
    }    
    o.pump = pump;
    o.uv = p;
    
    o.d *= .8;
    return o;
}

vec3 calcNormal( de o ) {
	vec3 eps = vec3( 0.01, 0.0, 0.0 );
	vec3 nor = vec3(
	    map(o.pos+eps.xyy, o.id).d - map(o.pos-eps.xyy, o.id).d,
	    map(o.pos+eps.yxy, o.id).d - map(o.pos-eps.yxy, o.id).d,
	    map(o.pos+eps.yyx, o.id).d - map(o.pos-eps.yyx, o.id).d );
	return normalize(nor);
}

de CastRay(ray r) {
    float d = 0.;
    float dS = MAX_DISTANCE;
    
    vec3 pos = vec3(0., 0., 0.);
    vec3 n = vec3(0.);
    de o, s;
    
    float dC = MAX_DISTANCE;
    vec3 p;
    rc q;
    float t = iTime;
    vec3 grid = vec3(6., 30., 6.);
        
    for(float i=0.; i<MAX_STEPS; i++) {
        p = r.o + r.d*d;
        
        #ifdef SINGLE
        s = map(p, vec3(0.));
        #else
        p.y -= t;  // make the move up
        p.x += t;  // make cam fly forward
            
        q = Repeat(p, grid);
    	
        vec3 rC = ((2.*step(0., r.d)-1.)*q.h-q.p)/r.d;	// ray to cell boundary
        dC = min(min(rC.x, rC.y), rC.z)+.01;		// distance to cell just past boundary
        
        float N = N3(q.id);
        q.p += (N31(N)-.5)*grid*vec3(.5, .7, .5);
        
		if(Dist(q.p.xz, r.d.xz, vec2(0.))<1.1)
        //if(DistRaySegment(q.p, r.d, vec3(0., -6., 0.), vec3(0., -3.3, 0)) <1.1) 
        	s = map(q.p, q.id);
        else
            s.d = dC;
        
        
        #endif
           
        if(s.d<HIT_DISTANCE || d>MAX_DISTANCE) break;
        d+=min(s.d, dC);	// move to distance to next cell or surface, whichever is closest
    }
    
    if(s.d<HIT_DISTANCE) {
        o.m = s.m;
        o.d = d;
        o.id = q.id;
        o.uv = s.uv;
        o.pump = s.pump;
        
        #ifdef SINGLE
        o.pos = p;
        #else
        o.pos = q.p;
        #endif
    }
    
    return o;
}

float VolTex(vec3 uv, vec3 p, float scale, float pump) {
    // uv = the surface pos
    // p = the volume shell pos
    
	p.y *= scale;
    
    float s2 = 5.*p.x/twopi;
    float id = floor(s2);
    s2 = fract(s2);
    vec2 ep = vec2(s2-.5, p.y-.6);
    float ed = length(ep);
    float e = B(.35, .45, .05, ed);
    
   	float s = SIN(s2*twopi*15. );
	s = s*s; s = s*s;
    s *= S(1.4, -.3, uv.y-cos(s2*twopi)*.2+.3)*S(-.6, -.3, uv.y);
    
    float t = iTime*5.;
    float mask = SIN(p.x*twopi*2. + t);
    s *= mask*mask*2.;
    
    return s+e*pump*2.;
}

vec4 JellyTex(vec3 p) { 
    vec3 s = vec3(atan(p.x, p.z), length(p.xz), p.y);
    
    float b = .75+sin(s.x*6.)*.25;
    b = mix(1., b, s.y*s.y);
    
    p.x += sin(s.z*10.)*.1;
    float b2 = cos(s.x*26.) - s.z-.7;
   
    b2 = S(.1, .6, b2);
    return vec4(b+b2);
}

vec3 render( vec2 uv, ray camRay, float depth ) {
    // outputs a color
    
    bg = background(cam.ray.d);
    
    vec3 col = bg;
    de o = CastRay(camRay);
    
    float t = iTime;
    vec3 L = up;
    

    if(o.m>0.) {
        vec3 n = calcNormal(o);
        float lambert = sat(dot(n, L));
        vec3 R = reflect(camRay.d, n);
        float fresnel = sat(1.+dot(camRay.d, n));
        float trans = (1.-fresnel)*.5;
        vec3 ref = background(R);
        float fade = 0.;
        
        if(o.m==1.) {	// hood color
            float density = 0.;
            for(float i=0.; i<VOLUME_STEPS; i++) {
                float sd = sph(o.uv, camRay.d, vec3(0.), .8+i*.015).x;
                if(sd!=MAX_DISTANCE) {
                    vec2 intersect = o.uv.xz+camRay.d.xz*sd;

                    vec3 uv = vec3(atan(intersect.x, intersect.y), length(intersect.xy), o.uv.z);
                    density += VolTex(o.uv, uv, 1.4+i*.03, o.pump);
                }
            }
            vec4 volTex = vec4(accent, density/VOLUME_STEPS); 
            
            
            vec3 dif = JellyTex(o.uv).rgb;
            dif *= max(.2, lambert);

            col = mix(col, volTex.rgb, volTex.a);
            col = mix(col, vec3(dif), .25);

            col += fresnel*ref*sat(dot(up, n));

            //fade
            fade = max(fade, S(.0, 1., fresnel));
        } else if(o.m==2.) {						// inside tentacles
            vec3 dif = accent;
    		col = mix(bg, dif, fresnel);
            
            col *= mix(.6, 1., S(0., -1.5, o.uv.y));
            
            float prop = o.pump+.25;
            prop *= prop*prop;
            col += pow(1.-fresnel, 20.)*dif*prop;
            
            
            fade = fresnel;
        } else if(o.m==3.) {						// outside tentacles
        	vec3 dif = accent;
            float d = S(100., 13., o.d);
    		col = mix(bg, dif, pow(1.-fresnel, 5.)*d);
        }
        
        fade = max(fade, S(0., 100., o.d));
        col = mix(col, bg, fade);
        
        if(o.m==4.)
            col = vec3(1., 0., 0.);
    } 
     else
        col = bg;
    
    return col;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	float t = iTime*.04;
    
    vec2 uv = (fragCoord.xy / iResolution.xy);
    uv -= .5;
    uv.y *= iResolution.y/iResolution.x; 
    
    vec2 m = iMouse.xy/iResolution.xy;
    
    if(m.x<0.05 || m.x>.95) {				// move cam automatically when mouse is not used
    	m = vec2(t*.25, SIN(t*pi)*.5+.5);
    }
	
    accent = mix(accentColor1, accentColor2, SIN(t*15.456));
    bg = mix(secondColor1, secondColor2, SIN(t*7.345231));
    
    float turn = (.1-m.x)*twopi;
    float s = sin(turn);
    float c = cos(turn);
    mat3 rotX = mat3(c,  0., s, 0., 1., 0., s,  0., -c);
    
    #ifdef SINGLE
    float camDist = -10.;
    #else
    float camDist = -.1;
    #endif
    
    vec3 lookAt = vec3(0., -1., 0.);
    
    vec3 camPos = vec3(0., INVERTMOUSE*camDist*cos((m.y)*pi), camDist)*rotX;
   	
    CameraSetup(uv, camPos+lookAt, lookAt, 1.);
    
    vec3 col = render(uv, cam.ray, 0.);
    
    col = pow(col, vec3(mix(1.5, 2.6, SIN(t+pi))));		// post-processing
    float d = 1.-dot(uv, uv);		// vignette
    col *= (d*d*d)+.1;
    
    fragColor = vec4(col, 1.);
}
)SHADER";

  shader_infos all_shader_infos
  {
    {
        L"lldBD8"
      , L"Transparent crystal"
      , L"mrange"
      , license__cc3
      , L"No modifications"
      , L"Transparent crystal inspired by the crystal language logo"
      , shader__lldBD8__transparent_crystal
      , false
    },
    {
        L"4dj3Wy"
      , L"Orbiting Mandelbrot traps"
      , L"guil"
      , license__cc3
      , L"Moved fractal to right and reduced number of iterations"
      , L"Nice looking Mandelbrot fractal with animated orbit traps"
      , shader__4dj3Wy__mandelbrot_with_orbit_traps
      , false
    },
    {
        L"Mld3Rn"
      , L"Voronoi crystal lattice"
      , L"Shane"
      , license__unknown
      , L"No modifications"
      , L"Amazing looking Voronoi lattice over an image."
      , shader__Mld3Rn__perspex_web_lattice
      , true
    },
    {
        L"4tdSWr"
      , L"2D clouds"
      , L"drift"
      , license__unknown
      , L"No modifications"
      , L"Nice looking performance 2D clouds"
      , shader__4tdSWr__2d_clouds
      , false
    },
    {
        L"XljGDz"
      , L"Protophore"
      , L"otaviogood"
      , license__cc0
      , L"No modifications"
      , L"Amazing looking sphere, I've used the 'sky box' in many shaders"
      , shader__XljGDz__protosphere
      , false
    },
    {
        L"XdcfR8"
      , L"Fractal Thingy Flythrough"
      , L"Klems"
      , license__unknown
      , L"No modifications"
      , L"Cool fractal fly through"
      , shader__XdcfR8__fractal_thingy_flythrough
      , false
    },
    {
        L"4ds3zn"
      , L"Apollonian"
      , L"iq"
      , license__cc3
      , L"No modifications"
      , L"Fly through apollonian fractal"
      , shader__4ds3zn__apollonian
      , false
    },
    {
        L"lsV3Rc"
      , L"Notch's unmandelboxing"
      , L"Edward"
      , license__unknown
      , L"No modifications"
      , L"Cool looking mandelbox fly through"
      , shader__lsV3Rc__unmandelboxing
      , false
    },
    {
        L"4tc3zf"
      , L"Galvanize"
      , L"Virgill"
      , license__unknown
      , L"No modifications"
      , L"Intro for Nordlicht demoparty 2014"
      , shader__4tc3zf__galvanize
      , false
    },
    {
        L"4dsGRl"
      , L"Subsurface Scattering"
      , L"TekF"
      , license__cc3
      , L"Replaced background texture with computed background"
      , L"Amazing!"
      , shader__4dsGRl__subsurface_scattering
      , false
    },
    {
        L"4sXBRn"
      , L"Luminescence"
      , L"BigWIngs"
      , license__cc3
      , L"No modifications"
      , L"A forest of jellyfish, extremely well done"
      , shader__4sXBRn__luminescence
      , false
    },
  };

  std::wstring get__reg_value (
    HKEY            parent
  , wchar_t const * id
  )
  {
    DWORD type {};
    DWORD expected_size {};
    if (ERROR_SUCCESS == RegQueryValueExW (
        parent
      , id
      , nullptr
      , &type
      , nullptr
      , &expected_size))
    {
      std::wstring result;
      result.resize ((expected_size + 1)/ 2); // /2 because unicode
      DWORD actual_size = expected_size;
      if (type == REG_SZ && ERROR_SUCCESS == RegQueryValueExW (
          parent
        , id
        , nullptr
        , &type
        , reinterpret_cast<BYTE*> (&result.front ())
        , &actual_size))
      {
        if (type == REG_SZ && expected_size == actual_size)
        {
          inplace_trim (result);
          return result;
        }
        else
        {
          // TODO: Log
          return std::wstring ();
        }
      }
      else
      {
        // TODO: Log
        return std::wstring ();
      }
    }
    else
    {
      // TODO: Log
      return std::wstring ();
    }
  }


  void set__reg_value (
    HKEY                  parent
  , wchar_t const *       id
  , std::wstring const &  value
  )
  {
    DWORD type {};
    DWORD expected_size {};
    CHECK (ERROR_SUCCESS == RegSetValueExW (
        parent
      , id
      , 0
      , REG_SZ
      , reinterpret_cast<BYTE const *> (value.c_str ())
      , 2*(static_cast<DWORD> (value.size ()) + 1)));
  }
}

shader_infos const & get__shader_infos ()
{
  return all_shader_infos;
}

std::size_t invalid_index = static_cast<std::size_t> (-1);

std::size_t index_of__shader (std::wstring const & id)
{
  auto i__find_shader = std::find_if (
      all_shader_infos.begin ()
    , all_shader_infos.end ()
    , [&id] (auto && si)
    {
      return si.id == id;
    });

  if (i__find_shader != all_shader_infos.end ())
  {
    return i__find_shader - all_shader_infos.begin ();
  }
  else
  {
    return invalid_index;
  }
}

shader_configuration get__current_configuration ()
{
  // TODO:
  auto const & sis = get__shader_infos ();

  shader_configuration const default_configuration =
    {
        sis.at (0)
      , 0
      , 1
      , L""
    };

  HKEY hkey__root {};
  if (ERROR_SUCCESS == RegOpenKeyExW (HKEY_CURRENT_USER, hkey__path, 0, KEY_READ, &hkey__root))
  {
    auto on_exit__close_key = on_exit_do ([hkey__root] { RegCloseKey (hkey__root); } );

    auto shader__id         = get__reg_value (hkey__root, L"shader__id"         );
    auto shader__start_time = get__reg_value (hkey__root, L"shader__start_time" );
    auto shader__speed      = get__reg_value (hkey__root, L"shader__speed"      );
    auto shader__image      = get__reg_value (hkey__root, L"shader__image"      );

    auto start_time = to_float (shader__start_time, 0.0);
    auto speed      = to_float (shader__speed     , 1.0);

    auto index_     = index_of__shader (shader__id);
    auto index      = index_ != invalid_index ? index_ : std::size_t {};
    return
      {
          sis[index]
        , start_time
        , speed
        , shader__image
      };
  }
  else
  {
    // TODO: Log
    return default_configuration;
  }
}

void set__current_configuration (shader_configuration const & configuration)
{
  HKEY hkey__root {};
  if (ERROR_SUCCESS == RegOpenKeyExW (HKEY_CURRENT_USER, hkey__path, 0, KEY_WRITE, &hkey__root))
  {
    auto on_exit__close_key = on_exit_do ([hkey__root] { RegCloseKey (hkey__root); } );

    set__reg_value (hkey__root, L"shader__id"         , configuration.shader_info.id              );
    set__reg_value (hkey__root, L"shader__start_time" , std::to_wstring (configuration.start_time ));
    set__reg_value (hkey__root, L"shader__speed"      , std::to_wstring (configuration.speed      ));
    set__reg_value (hkey__root, L"shader__image"      , configuration.image_path                  );
  }
}

std::pair<UINT, UINT> loaded_shader_configuration::get__image_dimensions ()
{
  CHECK (image_converter);
  UINT wic_width = 0;
  UINT wic_height = 0;
  CHECK_HR (image_converter->GetSize (&wic_width, &wic_height));
  return std::make_pair (wic_width, wic_height);
}
std::vector<BYTE> loaded_shader_configuration::get__image_bits ()
{
  CHECK (image_converter);

  auto dim        = get__image_dimensions ();
  auto wic_width  = dim.first;
  auto wic_height = dim.second;

  auto stride = wic_width*3;

  std::vector<BYTE> pixels;
  pixels.resize (stride*wic_height);

  WICRect wic_rect { 0, 0, static_cast<INT> (wic_width), static_cast<INT> (wic_height) };

  CHECK_HR (image_converter->CopyPixels (
      &wic_rect
    , 3*wic_width
    , static_cast<UINT> (pixels.size ())
    , &pixels.front ()
    ));

  std::vector<BYTE> row;
  row.resize (stride);

  for (auto y = 0U; y < wic_height/2; ++y)
  {
    auto from = y;
    auto to   = wic_height - y - 1;

    auto pb   = pixels.begin ();
    auto rb   = row.begin ();

    std::copy (pb + from*stride , pb + from*stride + stride , rb              );
    std::copy (pb + to*stride   , pb + to*stride + stride   , pb + from*stride);
    std::copy (rb               , rb + stride               , pb + to*stride  );
  }

  return pixels;
}

loaded_shader_configuration load__configuration (shader_configuration const & configuration)
{
  if (!configuration.shader_info.requires_image)
  {
    return
    {
        configuration
      , empty
    };
  }
  else
  {
    auto wic = cocreate_instance<IWICImagingFactory> (CLSID_WICImagingFactory);

    com_ptr<IWICBitmapDecoder> wic_decoder;

    CHECK_HR (wic->CreateDecoderFromFilename(
        configuration.image_path.c_str ()
      , nullptr
      , GENERIC_READ
      , WICDecodeMetadataCacheOnDemand
      , wic_decoder.out ()
      ));

    com_ptr<IWICBitmapFrameDecode> wic_frame_decoder;
    CHECK_HR (wic_decoder->GetFrame (0, wic_frame_decoder.out ()));

    com_ptr<IWICFormatConverter> wic_format_converter;
    CHECK_HR (wic->CreateFormatConverter (wic_format_converter.out ()));

    CHECK_HR (wic_format_converter->Initialize (
        wic_frame_decoder.get ()
      , GUID_WICPixelFormat24bppRGB
      , WICBitmapDitherTypeNone
      , nullptr
      , 0.F
      , WICBitmapPaletteTypeCustom
      ));

    return
    {
        configuration
      , wic_format_converter
    };
  }
}

