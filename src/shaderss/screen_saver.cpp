#include "stdafx.h"
#include "resource.h"

#include <windows.h>
#include <wincodec.h> 
#include <GL/gl.h>

#include <algorithm>
#include <cassert>
#include <memory>
#include <regex>
#include <stdexcept>
#include <string>
#include <utility>

#include "glext.h"

#include "common.hpp"
#include "config.hpp"

#pragma comment(lib, "Opengl32")

extern HINSTANCE get__hinstance () noexcept;

namespace
{
  bool        done              ;
  ULONGLONG   start             ;
  bool        screen_saver_mode ;

  HWND        hwnd              ;
  HDC         hdc               ;

  LONG        width             ;
  LONG        height            ;

  HGLRC       hrc               ;
  GLuint      pid               ;
  GLuint      fsid              ;
  GLuint      vsid              ;
  GLuint      tid               ;

  float       start_time        ;
  float       speed      = 1    ;

  constexpr int gl_functions_count = 7;

  char const * const gl_names[gl_functions_count] =
  {
    "glCreateShaderProgramv",
    "glGenProgramPipelines" ,
    "glBindProgramPipeline" ,
    "glUseProgramStages"    ,
    "glProgramUniform4fv"   ,
    "glGetProgramiv"        ,
    "glGetProgramInfoLog"   ,
  };

  void * gl_functions[gl_functions_count];

  #define oglCreateShaderProgramv         ((PFNGLCREATESHADERPROGRAMVPROC)  gl_functions[0])
  #define oglGenProgramPipelines          ((PFNGLGENPROGRAMPIPELINESPROC)   gl_functions[1])
  #define oglBindProgramPipeline          ((PFNGLBINDPROGRAMPIPELINEPROC)   gl_functions[2])
  #define oglUseProgramStages             ((PFNGLUSEPROGRAMSTAGESPROC)      gl_functions[3])
  #define oglProgramUniform4fv            ((PFNGLPROGRAMUNIFORM4FVPROC)     gl_functions[4])
  #define oglGetProgramiv                 ((PFNGLGETPROGRAMIVPROC)          gl_functions[5])
  #define oglGetProgramInfoLog            ((PFNGLGETPROGRAMINFOLOGPROC)     gl_functions[6])

  PIXELFORMATDESCRIPTOR const pfd =
  {
    sizeof(PIXELFORMATDESCRIPTOR)                         ,
    1                                                     ,
    PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER,
    PFD_TYPE_RGBA                                         ,
    32                                                    ,
    0, 0, 0, 0, 0, 0, 8, 0                                ,
    0, 0, 0, 0, 0                                         , // accum
    32                                                    , // zbuffer
    0                                                     , // stencil
    0                                                     , // aux
    PFD_MAIN_PLANE                                        ,
    0, 0, 0, 0                                            ,
  };

  WCHAR const window_title[]      = L"Shader Screen Saver"; // The title bar text
  WCHAR const window_class_name[] = L"SHADER_SS"          ; // the main window class name

  char const vertex_shader[] = R"SHADER(
#version 430

layout (location=0) in vec2 inVer;
out vec2 p;

out gl_PerVertex
{
  vec4 gl_Position;
};


void main()
{
  gl_Position=vec4(inVer,0.0,1.0);
  p=inVer;
}
)SHADER";

  char const fragment_shader_prelude[] = R"SHADER(
// -----------------------------------------------------------------------
// BEGIN - Common prelude
// -----------------------------------------------------------------------
#version 430

precision mediump float;

layout (location=0) uniform vec4 fpar[];
layout (location=0) out vec4 co;
uniform sampler2D iChannel0;

in vec2 p;

// TODO: How to make these uniform
vec2 iMouse       = vec2(0.0);
vec2 iResolution  = vec2(1.0);
float iTime       = 1.0;

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

)SHADER";

  int check_link_status (int id, char const * msg)
  {
    int result;
    oglGetProgramiv (id, GL_LINK_STATUS, &result);
    if (!result)
    {
      char    info[1536];

      oglGetProgramInfoLog (id, 1024, nullptr, info);
      OutputDebugStringA (msg);
      OutputDebugStringA ("\n");
      OutputDebugStringA (info);
      OutputDebugStringA ("\n");
      throw std::runtime_error (msg);
    }

    return id;
  }

  #define CHECK_LINK_STATUS(expr) check_link_status (expr, (__FILE__ "(" STRINGIFY(__LINE__) "): Check link status failed for - " #expr))

  LRESULT CALLBACK window_proc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
  {
    auto kill = [] ()
    {
      done = true;
      PostQuitMessage (0);
    };

    auto now = GetTickCount64 () - start;
    auto screen_saver_check = screen_saver_mode && (now > 1000);

    switch (message)
    {
    case WM_SIZE:
      width  = LOWORD (lParam);
      height = HIWORD (lParam);
      glViewport (0, 0, width, height);
      return 0;
    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint (hWnd, &ps);
        // TODO: Add any drawing code that uses hdc here...
        EndPaint (hWnd, &ps);
      }
      return 0;
    case WM_DESTROY:
      kill ();
      return 0;
    case WM_ACTIVATE:
    case WM_ACTIVATEAPP:
    case WM_NCACTIVATE:
      if (screen_saver_check && !wParam)
      {
        kill ();
        return 0;
      }
      else
      {
        return DefWindowProc (hWnd, message, wParam, lParam);
      }
    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
    case WM_MBUTTONDOWN:
    case WM_KEYDOWN:
    case WM_KEYUP:
    case WM_MOUSEMOVE:
      if (screen_saver_check)
      {
        kill ();
        return 0;
      }
      else
      {
        return DefWindowProc (hWnd, message, wParam, lParam);
      }
    case WM_SETCURSOR:
      if (screen_saver_check)
      {
        return DefWindowProc (hWnd, message, 0, lParam);  // Clears wParam
      }
      else
      {
        return DefWindowProc (hWnd, message, wParam, lParam);
      }
    case WM_SYSCOMMAND:
      if (screen_saver_check && (wParam == SC_CLOSE || wParam == SC_SCREENSAVE))
      {
        return false;
      }
      else
      {
        return DefWindowProc (hWnd, message, wParam, lParam);
      }
    default:
      return DefWindowProc (hWnd, message, wParam, lParam);
    }
  }

  ATOM register_class ()
  {
    WNDCLASSEXW wcex;

    wcex.cbSize = sizeof (WNDCLASSEX);

    wcex.style          = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc    = window_proc;
    wcex.cbClsExtra     = 0;
    wcex.cbWndExtra     = 0;
    wcex.hInstance      = get__hinstance ();
    wcex.hIcon          = LoadIcon (get__hinstance (), MAKEINTRESOURCE (IDI_SHADERSS));
    wcex.hCursor        = LoadCursor (nullptr, IDC_ARROW);
    wcex.hbrBackground  = (HBRUSH) GetStockObject(BLACK_BRUSH);
    wcex.lpszMenuName   = nullptr;
    wcex.lpszClassName  = window_class_name;
    wcex.hIconSm        = LoadIcon (wcex.hInstance, MAKEINTRESOURCE (IDI_SMALL));

    return CHECK (RegisterClassExW (&wcex));
  }

  void init_window (int nCmdShow)
  {
    hwnd = CHECK (CreateWindowExW (
        0
      , window_class_name
      , window_title
      , WS_VISIBLE | WS_OVERLAPPEDWINDOW
      , CW_USEDEFAULT
      , CW_USEDEFAULT
      , CW_USEDEFAULT
      , CW_USEDEFAULT
      , nullptr
      , nullptr
      , get__hinstance ()
      , nullptr
      ));

    ShowWindow (hwnd, nCmdShow);
    CHECK (UpdateWindow (hwnd));

    if (screen_saver_mode)
    {
      auto cx = GetSystemMetrics (SM_CXSCREEN);
      auto cy = GetSystemMetrics (SM_CYSCREEN);

      auto style = GetWindowLongW (hwnd, GWL_STYLE);
      style &= ~(WS_CAPTION | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SYSMENU);
      SetWindowLongW (hwnd, GWL_STYLE, style);

      auto ex_style = GetWindowLongW (hwnd, GWL_EXSTYLE);
      ex_style &= ~(WS_EX_DLGMODALFRAME | WS_EX_CLIENTEDGE | WS_EX_STATICEDGE);
      SetWindowLongW (hwnd, GWL_EXSTYLE, ex_style);
      SetWindowPos (
          hwnd
        , nullptr
        , 0
        , 0
        , cx
        , cy
        , SWP_FRAMECHANGED | SWP_NOZORDER | SWP_NOOWNERZORDER
        );
    }
  }


  void init_opengl ()
  {
    auto loaded_config  = load__configuration (get__current_configuration ());

    start_time  = loaded_config.shader_configuration.start_time ;
    speed       = loaded_config.shader_configuration.speed      ;

    hdc = CHECK (GetDC(hwnd));

    auto pf = CHECK (ChoosePixelFormat (hdc,&pfd));

    CHECK (SetPixelFormat (hdc,pf,&pfd));

    hrc = CHECK (wglCreateContext (hdc));

    CHECK (wglMakeCurrent(hdc, hrc));

    for (auto i = 0; i < gl_functions_count; ++i)
    {
      gl_functions[i] = CHECK (wglGetProcAddress(gl_names[i]));
    }

    if (!!loaded_config.image_converter)
    {
      auto dim    = loaded_config.get__image_dimensions ();
      auto pixels = loaded_config.get__image_bits ();
      glGenTextures (1, &tid);
      glBindTexture (GL_TEXTURE_2D, tid);
      glTexImage2D (GL_TEXTURE_2D, 0, GL_RGB, dim.first, dim.second, 0, GL_RGB, GL_UNSIGNED_BYTE, &pixels.front ());
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    //  glGenerateMipmap(GL_TEXTURE_2D);
    }

    std::string shader_source = fragment_shader_prelude;
    shader_source += loaded_config.shader_configuration.shader_info.source;

    auto vsh = vertex_shader;
    auto fsh = shader_source.c_str ();

    vsid = oglCreateShaderProgramv (GL_VERTEX_SHADER, 1, &vsh);
    fsid = oglCreateShaderProgramv (GL_FRAGMENT_SHADER, 1, &fsh);

    oglGenProgramPipelines (1, &pid);
    oglBindProgramPipeline (pid);
    oglUseProgramStages (pid, GL_VERTEX_SHADER_BIT, vsid);
    oglUseProgramStages (pid, GL_FRAGMENT_SHADER_BIT, fsid);

    if (!!loaded_config.image_converter)
    {
      CHECK_LINK_STATUS (tid);
    }
    CHECK_LINK_STATUS (vsid);
    CHECK_LINK_STATUS (fsid);
    CHECK_LINK_STATUS (pid);
  }

  void draw_gl (std::uint64_t now)
  {
    auto t = 0.001f*now;

    float fparams[4]
    {
      start_time + t*speed  ,
      width*1.f             ,
      height*1.f            ,
      0                     ,
    };

    oglProgramUniform4fv (fsid, 0, 1, fparams);

    glRects (-1, -1, 1, 1);
  }

}

int show__screen_saver (int nCmdShow, bool ssm)
{
  screen_saver_mode = ssm;

  register_class ();

  init_window (nCmdShow);

  init_opengl ();

  HACCEL hAccelTable = LoadAccelerators (get__hinstance (), MAKEINTRESOURCE (IDC_SHADERSS));

  MSG msg;

  {
    RECT client;

    CHECK (GetClientRect(hwnd, &client));
    width  = client.right - client.left;
    height = client.bottom - client.top;
  }

  start = GetTickCount64 ();

  // Main message loop:
  while (true)
  {
    while (!done && PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
    {
      if (!TranslateAccelerator (msg.hwnd, hAccelTable, &msg))
      {
        TranslateMessage (&msg);
        DispatchMessage (&msg);
      }
    }

    if (done) break;

    auto now = GetTickCount64 () - start;

    draw_gl (now);

    SwapBuffers (hdc);
    Sleep (1);
  }

  return 0;
}
