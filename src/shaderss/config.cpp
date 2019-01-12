#include "stdafx.h"

#include <algorithm>
#include <cwchar>

#include "config.hpp"

namespace
{
  wchar_t const license__cc3[] = L"License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.";
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

  shader_infos all_shader_infos 
  {
    {
        L"lldBD8"
      , L"mrange"
      , license__cc3
      , L"No modifications"
      , L"Transparent crystal inspired by the crystal language logo"
      , shader__lldBD8__transparent_crystal
      , false
    },
    {
        L"4dj3Wy"
      , L"guil"
      , license__cc3
      , L"Moved fractal to right and reduced number of iterations"
      , L"Nice looking Mandelbrot fractal with animated orbit traps"
      , shader__4dj3Wy__mandelbrot_with_orbit_traps
      , false
    },
    {
        L"Mld3Rn"
      , L"Shane"
      , license__cc3
      , L"Inferred license to be CC3 because the sample wasn't clear on license"
      , L"Amazing looking Voronoi lattice over an image."
      , shader__4dj3Wy__mandelbrot_with_orbit_traps
      , false
    },
  };

  std::wstring read__reg_value (
    HKEY            parent
  , wchar_t const * value
  )
  {
    DWORD type {};
    DWORD expected_size {};
    if (ERROR_SUCCESS == RegQueryValueExW (
        parent
      , value
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
        , value
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

  float to_float (std::wstring const & s, float default_to)
  {
    if (!s.empty ())
    {
      auto begin    = s.c_str ();
      auto end      = begin + s.size ();
      wchar_t * last= nullptr;
      auto f = std::wcstof (begin, &last);
      if (end == last)
      {
        return f;
      }
      else
      {
        return default_to;
      }
    }
    else
    {
      return default_to;
    }
  }

}

shader_infos const & get__shader_infos ()
{
  return all_shader_infos;
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
  auto hkey__path = LR"*(Software\mrange\shaderss)*";
  if (ERROR_SUCCESS == RegOpenKeyExW (HKEY_CURRENT_USER, hkey__path, 0, KEY_READ, &hkey__root))
  {
    auto on_exit__close_key = on_exit_do ([hkey__root] { RegCloseKey (hkey__root); } );

    auto shader__id         = read__reg_value (hkey__root, L"shader__id");
    auto shader__start_time = read__reg_value (hkey__root, L"shader__start_time");
    auto shader__speed      = read__reg_value (hkey__root, L"shader__speed");
    auto shader__image      = read__reg_value (hkey__root, L"shader__image");

    auto start_time = to_float (shader__start_time, 0.0);
    auto speed      = to_float (shader__speed     , 1.0);

    auto i__find_shader      = std::find_if (
        sis.begin ()
      , sis.end ()
      , [&shader__id] (auto && si) 
      { 
        return si.id == shader__id; 
      }
      );

    if (i__find_shader != sis.end ())
    {
      return
        {
            *i__find_shader
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
  else
  {
    // TODO: Log
    return default_configuration;
  }
}

void set__current_configuration (shader_configuration const & configuration)
{
  // TODO:
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
  auto wic_height = dim.first;

  auto stride = wic_width*3;

  std::vector<BYTE> pixels;
  pixels.resize (stride*wic_height);

  WICRect wic_rect { 0, 0, wic_width, wic_height };

  CHECK_HR (image_converter->CopyPixels (
      &wic_rect
    , 3*wic_width
    , pixels.size ()
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

